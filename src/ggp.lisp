(in-package :ggp)
(named-readtables:in-readtable :fare-quasiquote)


(defparameter *debug*
  t)

(defparameter *rules-package*
  (find-package :ggp-rules))

(defparameter *constant-rules-symbols*
  '(nil
    ggp-rules::info
    ggp-rules::play
    ggp-rules::stop
    ggp-rules::start

    ggp-rules::name
    ggp-rules::status
    ggp-rules::busy
    ggp-rules::available
    ggp-rules::species
    ggp-rules::alien

    ggp-rules::ready
    ggp-rules::done
    ggp-rules::what

    ggp-rules::<=
    ggp-rules::role
    ggp-rules::init
    ggp-rules::legal
    ggp-rules::terminal
    ggp-rules::goal
    ggp-rules::does
    ggp-rules::next
    ggp-rules::true
    ggp-rules::or
    ggp-rules::distinct
    ggp-rules::not

    ;; GDL-II
    ggp-rules::sees
    ggp-rules::random
    ))


;;;; GGP Player ---------------------------------------------------------------
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
   (match-roles
     :type (or null list)
     :initform nil
     :reader player-match-roles
     :documentation "A list of the roles for the current match.  Feel free to read and use this if you like.  **Do not modify this.**")
   (start-clock
     :type (or null (integer 1))
     :initform nil
     :documentation "The start clock for the current game.  **Do not touch this.**  Use the `timeout` value passed to your methods instead.")
   (play-clock
     :type (or null (integer 1))
     :initform nil
     :documentation "The play clock for the current game.  **Do not touch this.**  Use the `timeout` value passed to your methods instead.")
   (message-start
     :type (or null (integer 0))
     :initform nil
     :documentation "The (internal-real) timestamp of when the current GGP message was received.  **Do not touch this.**  Use the `timeout` value passed to your methods instead.")
   (current-match
     :initform nil
     :documentation "The ID of the current match the player is playing, or `nil` if it is waiting.  **Do not touch this.**")
   (server
     :documentation "The Clack server object of the player.  **Do not touch this.**  Use `start-player` and `kill-player` to start/stop the server safely."))
  (:documentation "The base class for a GGP player.  Custom players should extend this."))


(defgeneric player-start-game (player rules role timeout)
  (:documentation "Called when the game is started.

  `rules` is a list of lists/symbols representing the GDL description of the
  game.  Note that all symbols are interned in the `GGP-RULES` package.

  `role` is a symbol representing the role of the player in this game.

  `timeout` is the timestamp that the response to the server is due by, in
  internal-real time units.  Basically: when `(get-internal-real-time)` returns
  this number, your message better have reached the server.

  "))

(defgeneric player-update-game (player moves)
  (:documentation "Called after all players have made their moves.

  `moves` will be a list of `(role . move)` conses representing moves made by
  each player last turn.

  "))

(defgeneric player-update-game-ii (player move percepts)
  (:documentation
    "Called after all players have made their moves in a GDL-II game.

  `move` will be the move you played last turn.

  `percepts` are all the percepts you see for the round.

  "))

(defgeneric player-select-move (player timeout)
  (:documentation "Called when it's time for the player to select a move to play.

  Must return a list/symbol of the GDL move to play.  Note that any symbols in
  the move should be ones that are interned in the `GGP-RULES` package.  The
  author is aware that this sucks and welcomes suggestions on how to make it
  less awful.

  `timeout` is the timestamp that the response to the server is due by, in
  internal-real time units.  Basically: when `(get-internal-real-time)` returns
  this number, your message better have reached the server.

  "))

(defgeneric player-stop-game (player)
  (:documentation "Called when the game is stopped.

  This is a good place to do any teardown stuff your player might need, or maybe
  to suggest a GC to your Lisp implementation.

  "))


(defmethod player-start-game ((player ggp-player) rules role timeout)
  nil)

(defmethod player-update-game ((player ggp-player) moves)
  nil)

(defmethod player-update-game-ii ((player ggp-player) move percepts)
  nil)

(defmethod player-select-move ((player ggp-player) timeout)
  (error "Required method player-select-move is not implemented for ~A" player))

(defmethod player-stop-game ((player ggp-player))
  nil)


;;;; Utils --------------------------------------------------------------------
(defun safe-read-from-string (s)
  ;; what could go wrong
  (let ((*read-eval* nil)
        (*package* *rules-package*))
    (read-from-string s)))

(defun render-to-string (e)
  (let ((*package* *rules-package*))
    (format nil "~A" e)))

(defun calculate-timeout (player clock)
  "Calculate the timestamp (in internal units) that we must return by."
  (+ (slot-value player 'message-start)
     (* clock internal-time-units-per-second)))

(defun clear-rules-package ()
  (do-symbols (symbol *rules-package*) ; JESUS TAKE THE WHEEL
    (when (not (member symbol *constant-rules-symbols*))
      (unintern symbol *rules-package*))))

(defun find-roles (rules)
  (mapcar #'second
          (remove-if-not #'(lambda (rule)
                            (and (consp rule)
                                 (eql 'ggp-rules::role (first rule))))
                         rules)))

(defun zip-moves (player moves)
  (mapcar #'cons ; lol ggp
          (slot-value player 'match-roles)
          moves))

(defun read-gdl-from-file (filename)
  "Read GDL from `filename`"
  (let ((*package* *rules-package*))
    (with-open-file (stream filename)
      (loop
        :with done = (gensym)
        :for form = (read stream nil done)
        :while (not (eq form done))
        :collect form))))


;;;; Clack Horseshit ----------------------------------------------------------
(defun l (&rest args)
  (when *debug*
    (let ((*package* *rules-package*))
      (apply #'format *debug-io* args))))

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


;;;; GGP Protocol -------------------------------------------------------------
(defun handle-info (player)
  `((ggp-rules::name ,(slot-value player 'name))
    (ggp-rules::status ,(if (slot-value player 'current-match)
                          'ggp-rules::busy
                          'ggp-rules::available))
    (ggp-rules::species ggp-rules::alien)))

(defun handle-start (player match-id role rules start-clock play-clock)
  (setf (slot-value player 'current-match) match-id
        (slot-value player 'start-clock) start-clock
        (slot-value player 'play-clock) play-clock
        (slot-value player 'match-roles) (find-roles rules))
  (l "Starting match ~S as ~S~%" match-id role)
  (player-start-game player rules role (calculate-timeout player start-clock))
  'ggp-rules::ready)


(defun handle-play (player match-id moves)
  (declare (ignore match-id))
  (l "Handling play request with moves ~S~%" moves)
  (player-update-game player (zip-moves player moves))
  (player-select-move player
                      (calculate-timeout player (slot-value player 'play-clock))))

(defun handle-play-ii (player match-id turn move percepts)
  (declare (ignore match-id))
  (l "Handling GDL-II play request (turn ~D)~%    with move: ~S~%    and percepts: ~S~%"
     turn move percepts)
  (player-update-game-ii player move percepts)
  (player-select-move player
                      (calculate-timeout player (slot-value player 'play-clock))))


(defun cleanup-game (player)
  (player-stop-game player)
  (setf (slot-value player 'match-roles) nil)
  (clear-rules-package)
  (setf (slot-value player 'current-match) nil))

(defun handle-stop (player match-id moves)
  (l "Handling stop request for ~S~%" match-id)
  (player-update-game player (zip-moves player moves))
  (cleanup-game player)
  'ggp-rules::done)

(defun handle-stop-ii (player match-id turn move percepts)
  (declare (ignore turn))
  (l "Handling GDL-II stop request for ~S~%" match-id)
  (player-update-game-ii player move percepts)
  (cleanup-game player)
  'ggp-rules::done)


(defun route (player request)
  "Route the request to the appropriate player function."
  (match request
    (`(ggp-rules::info)
     (handle-info player))

    (`(ggp-rules::play ,match-id ,moves)
     (handle-play player match-id moves))

    (`(ggp-rules::play ,match-id ,turn ,move ,percepts)
     (handle-play-ii player match-id turn move percepts))

    (`(ggp-rules::stop ,match-id ,moves)
     (handle-stop player match-id moves))

    (`(ggp-rules::stop ,match-id ,turn ,move ,percepts)
     (handle-stop-ii player match-id turn move percepts))

    (`(ggp-rules::start ,match-id ,role ,rules ,start-clock ,play-clock)
     (handle-start player match-id role rules start-clock play-clock))

    (unknown-request
      (l "UNKNOWN REQUEST: ~S~%~%" unknown-request)
      'ggp-rules::what)))


;;;; Boilerplate --------------------------------------------------------------
(defun should-log-p (request)
  (match request
    (`(ggp-rules::info) nil)
    (_ t)))

(defun app (player env)
  (setf (slot-value player 'message-start) (get-internal-real-time))
  (unwind-protect
      (let* ((body (get-body env))
             (request (safe-read-from-string body))
             (should-log (should-log-p request)))
        (when should-log
          (l "~%~%Got a request ====================================~%")
          ; (l "~S~%" body)
          (l "~A~%" request)
          (l "==================================================~%"))
        (let* ((response (route player request))
               (rendered-response (render-to-string response)))
          (when should-log
            (l "==================================================~%")
            (l "Responding with:~%~A~%" rendered-response)
            (l "==================================================~%"))
          (resp rendered-response)))
    (setf (slot-value player 'message-start) nil)))


;;;; Spinup/spindown ----------------------------------------------------------
(defun start-player (player &key (server :hunchentoot) (use-thread t))
  "Start the HTTP server for the given player.

  The `:server` and `:use-thread` options will be passed along to Clack.

  "
  (let* ((player-handler #'(lambda (env) (app player env)))
         (server (clack:clackup player-handler
                                :port (player-port player)
                                :server server
                                :use-thread use-thread)))
    (setf (slot-value player 'server) server)
    player))

(defun kill-player (player)
  "Kill the HTTP server for the given player.

  This will **not** be done gently.  No cleanup will be performed if the player
  is in the middle of a game.  Be careful.

  "
  (clack.handler:stop (slot-value player 'server))
  (setf (slot-value player 'current-match) nil)
  (setf (slot-value player 'match-roles) nil)
  (clear-rules-package))
