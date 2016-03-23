(in-package #:ggp)
(named-readtables:in-readtable :fare-quasiquote)

(defparameter *debug*
  t)

(defparameter *ggp-package*
  (find-package :ggp))


;;;; GGP Player
(defclass ggp-player ()
  ((name :initarg :name :initform "CL-GGP" :reader player-name)
   (port :initarg :port :initform 9999 :reader player-port)
   (current-match :initform nil)
   (server)))

(defgeneric player-start-game (player rules role start-clock play-clock))
(defgeneric player-update-game (player moves))
(defgeneric player-select-move (player))
(defgeneric player-stop-game (player))


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

(defun should-log-p (request)
  (match request
    (`(info) nil)
    (_ t)))

;;;; Boilerplate
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
  (let* ((player-handler #'(lambda (env) (app player env)))
         (server (clack:clackup player-handler
                                :port (player-port player))))
    (setf (slot-value player 'server) server)
    player))

(defun kill-player (player)
  (clack.handler:stop (slot-value player 'server)))
