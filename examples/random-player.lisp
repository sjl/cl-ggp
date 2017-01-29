(in-package :cl-user)


(defclass random-player (ggp:ggp-player)
  ((role          :accessor p-role)
   (current-state :accessor p-current-state)
   (reasoner      :accessor p-reasoner)))

(defmethod ggp:player-start-game
    ((player random-player) rules role deadline)
  (setf (p-role player) role
        (p-reasoner player) (ggp.reasoner:make-reasoner rules)))

(defmethod ggp:player-update-game
    ((player random-player) moves)
  (setf (p-current-state player)
        (if (null moves)
          (ggp.reasoner:initial-state (p-reasoner player))
          (ggp.reasoner:next-state (p-reasoner player)
                                   (p-current-state player)
                                   moves))))

(defmethod ggp:player-select-move
    ((player random-player) deadline)
  (let ((moves (ggp.reasoner:legal-moves-for
                 (p-reasoner player)
                 (p-current-state player)
                 (p-role player))))
    (nth (random (length moves)) moves)))

(defmethod ggp:player-stop-game
    ((player random-player))
  (setf (p-current-state player) nil
        (p-reasoner player) nil
        (p-role player) nil))

(defvar *random-player*
  (make-instance 'random-player
                 :name "RandomPlayer"
                 :port 4000))

(defvar *random-player-2*
  (make-instance 'random-player
                 :name "AnotherRandomPlayer"
                 :port 5000))

; (ggp:start-player *random-player*)
; (ggp:start-player *random-player-2*)
; (ggp:kill-player *random-player*)
; (ggp:kill-player *random-player-2*)
