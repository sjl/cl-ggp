(in-package #:cl-user)

(defclass simple-player (ggp:ggp-player)
  ())

(defmethod ggp:player-select-move ((player simple-player))
  'wait)

(defvar *player* nil)

(setf *player* (make-instance 'simple-player
                              :name "SimplePlayer"
                              :port 5000))


(ggp:start-player *player*)
(ggp:kill-player *player*)

(setf (slot-value *player* 'ggp::current-match) nil)
