(in-package :cl-user)

(ql:quickload '(:cl-ggp :cl-ggp.reasoner))

;;;; Simulations --------------------------------------------------------------
(defun random-elt (sequence)
  (elt sequence (random (length sequence))))


;;;; Simulations --------------------------------------------------------------
(defun random-move (reasoner state)
  (mapcar (lambda (role)
            (cons role (random-elt
                         (ggp.reasoner:legal-moves-for reasoner state role))))
          (ggp.reasoner:roles reasoner)))

(defun random-playout-value (reasoner role state &optional our-move)
  (if (ggp.reasoner:terminalp reasoner state)
    (ggp.reasoner:goal-value-for reasoner state role)
    (let ((move (random-move reasoner state)))
      (when our-move
        (setf (cdr (assoc role move)) our-move))
      (random-playout-value reasoner role
                            (ggp.reasoner:next-state reasoner state move)))))


;;;; Player -------------------------------------------------------------------
(defclass monte-carlo-player (ggp:ggp-player)
  ((role          :accessor p-role)
   (current-state :accessor p-current-state)
   (reasoner      :accessor p-reasoner)))

(defmethod ggp:player-start-game
    ((player monte-carlo-player) rules role deadline)
  (setf (p-role player) role
        (p-reasoner player) (ggp.reasoner:make-reasoner rules)))

(defmethod ggp:player-update-game
    ((player monte-carlo-player) moves)
  (setf (p-current-state player)
        (if (null moves)
          (ggp.reasoner:initial-state (p-reasoner player))
          (ggp.reasoner:next-state (p-reasoner player)
                                   (p-current-state player)
                                   moves))))


(defun conservative-deadline (deadline &optional (seconds-of-breathing-room 1))
  (- deadline (* seconds-of-breathing-room internal-time-units-per-second)))

(defmethod ggp:player-select-move
    ((player monte-carlo-player) deadline)
  (loop
    :with conservative-deadline = (conservative-deadline deadline)
    :with reasoner = (p-reasoner player)
    :with state = (p-current-state player)
    :with role = (p-role player)
    :with our-moves = (ggp.reasoner:legal-moves-for reasoner state role)
    :with scores = (mapcar (lambda (move) (cons move 0))
                           our-moves)
    :for count :from 1
    :until (>= (get-internal-real-time) conservative-deadline)
    :do (dolist (move our-moves)
          (incf (cdr (assoc move scores))
                (random-playout-value reasoner role state move)))
    :finally (progn
               (format t "~%Ran ~D * ~D = ~D simulations~%"
                       count (length our-moves) (* count (length our-moves)))
               (format t "~%Results: ~S~%" (mapcar (lambda (score)
                                                     (cons (car score)
                                                           (/ (cdr score) count 1.0)))
                                                   scores))
               (finish-output)
               (return (car (first (sort scores #'> :key #'cdr)))))))

(defmethod ggp:player-stop-game
    ((player monte-carlo-player))
  (setf (p-current-state player) nil
        (p-reasoner player) nil
        (p-role player) nil))


;;;; Scratch ------------------------------------------------------------------
(defvar *monte-carlo-player*
  (make-instance 'monte-carlo-player
    :name "MonteCarloPlayer"
    :port 4000))

;; (ggp:start-player *monte-carlo-player*)
;; (ggp:kill-player *monte-carlo-player*)
