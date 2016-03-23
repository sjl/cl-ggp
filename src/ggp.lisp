(in-package #:ggp)
(named-readtables:in-readtable :fare-quasiquote)

(defparameter *debug*
  t)

(defparameter *ggp-package*
  (find-package :ggp))


;;;; GGP Player
(defclass ggp-player ()
  ((name
     :initarg :name
     :initform "CL-GGP"
     :reader player-name
     :type string
     :documentation "The name of the player.")
   (port
     :initarg :port
     :initform 9999
     :reader player-port
     :type (integer 0)
     :documentation "The port the HTTP server should listen on.")
   (current-match
     :initform nil
     :documentation "The ID of the current match the player is playing, or `nil` if it is waiting.  **Do not touch this.**")
   (server
     :documentation "The Clack server object of the player.  **Do not touch this.**  Use `start-player` and `kill-player` to start/stop the server safely."))
  (:documentation "The base class for a GGP player.  Custom players should extend this."))


(defgeneric player-start-game (player rules role start-clock play-clock)
  (:documentation "Called when the game is started.

  `rules` is a list of lists/symbols representing the GDL description of the
  game.  Note that all symbols are interned in the `GGP` package.

  "))

(defgeneric player-update-game (player moves)
  (:documentation "Called after all player have made their moves.

  `moves` will be a list of moves made by the players.

  "))

(defgeneric player-select-move (player)
  (:documentation "Called when it's time for the player to select a move to play.

  Must return a list/symbol of the GDL move to play.  Note that any symbols in
  the move should be ones that are interned in the `GGP` package.  The author is
  aware that this sucks and welcomes suggestions on how to make it less awful.

  "))

(defgeneric player-stop-game (player)
  (:documentation "Called when the game is stopped.

  This is a good place to do any teardown stuff your player might need, or maybe
  to suggest a GC to your Lisp implementation.

  "))


(defmethod player-start-game ((player ggp-player) rules role start-clock play-clock)
  nil)

(defmethod player-update-game ((player ggp-player) moves)
  nil)

(defmethod player-select-move ((player ggp-player))
  (error "Required method player-select-move is not implemented for ~A" player))

(defmethod player-stop-game ((player ggp-player))
  nil)


;;;; Utils
(defun safe-read-from-string (s)
  ;; what could go wrong
  (let ((*read-eval* nil)
        (*package* *ggp-package*))
    (read-from-string s)))

(defun render-to-string (e)
  (let ((*package* *ggp-package*))
    (format nil "~A" e)))


;;;; Clack Horseshit
(defun l (&rest args)
  (when *debug*
    (apply #'format *debug-io* args)))

(defun resp (body &key (code 200) (content-type "text/acl"))
  (list code
        (list :content-type content-type
              :content-length (length body))
        (list body)))

(defun get-body (env)
  ;; jesus christ clack why do i have to write this shit
  (let ((body (make-array (getf env :content-length)
                          :element-type 'flex:octet)))
    (read-sequence body (getf env :raw-body))
    (flex:octets-to-string body)))


;;;; GGP Protocol
(defun handle-info (player)
  `((name ,(slot-value player 'name))
    (status ,(if (slot-value player 'current-match) 'busy 'available))
    (species alien)))

(defun handle-start (player match-id role rules start-clock play-clock)
  (declare (ignore play-clock))
  (setf (slot-value player 'current-match) match-id)
  (l "Starting match ~S as ~S~%" match-id role)
  (player-start-game player rules role start-clock play-clock)
  'ready)

(defun handle-play (player match-id moves)
  (l "Handling play request with moves ~S~%" moves)
  (player-update-game player moves)
  (player-select-move player))

(defun handle-stop (player match-id moves)
  (l "Handling stop request for ~S~%" match-id)
  (player-stop-game player)
  (setf (slot-value player 'current-match) nil)
  'done)


(defun route (player request)
  "Route the request to the appropriate player function."
  (match request
    (`(info)
     (handle-info player))

    (`(play ,match-id ,moves)
     (handle-play player match-id moves))

    (`(stop ,match-id ,moves)
     (handle-stop player match-id moves))

    (`(start ,match-id ,role ,rules ,start-clock ,play-clock)
     (handle-start player match-id role rules start-clock play-clock))

    (unknown-request
      (l "UNKNOWN REQUEST: ~S~%~%" unknown-request)
      'what)))


;;;; Boilerplate
(defun should-log-p (request)
  (match request
    (`(info) nil)
    (_ t)))

(defun app (player env)
  (let* ((body (get-body env))
         (request (safe-read-from-string body))
         (should-log (should-log-p request)))
    (when should-log
      (l "~%~%Got a request ====================================~%")
      (l "~S~%" request)
      (l "==================================================~%"))
    (let* ((response (route player request))
           (rendered-response (render-to-string response)))
      (when should-log
        (l "==================================================~%")
        (l "Responding with:~%~A~%" rendered-response)
        (l "==================================================~%"))
      (resp rendered-response))))


;;;; Spinup/spindown
(defun start-player (player)
  "Start the HTTP server for the given player."
  (let* ((player-handler #'(lambda (env) (app player env)))
         (server (clack:clackup player-handler
                                :port (player-port player))))
    (setf (slot-value player 'server) server)
    player))

(defun kill-player (player)
  "Kill the HTTP server for the given player.

  This will **not** be done gently.  No cleanup will be performed if the player
  is in the middle of a game.  Be careful.

  "
  (clack.handler:stop (slot-value player 'server)))
