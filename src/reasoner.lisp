(in-package :ggp.reasoner)

;;;; Utilities ----------------------------------------------------------------
(defun gdl-rule-p (form)
  (and (consp form)
       (eq (car form) 'ggp-rules::<=)))

(defun dedupe (things)
  (remove-duplicates things :test #'equal))

(defun normalize-state (state)
  (dedupe state))


;;;; GDL Cleaning -------------------------------------------------------------
;;; Some GDL authors use (or x y) and (and x y) in their game descriptions, even
;;; though it's not part of the GDL "spec".  Worse still, some use n-ary
;;; versions of those predicates, because fuck you.  So we'll do a quick pass
;;; over the GDL to clean up these bugs.

(defun clean-or (gdl)
  (destructuring-bind (or . arguments)
      gdl
    (case (length arguments)
      (1 (first arguments))
      (2 gdl)
      (t (list or (first arguments)
               (clean-or (cons or (rest arguments))))))))

(defun clean-and (gdl)
  (destructuring-bind (and . arguments)
      gdl
    (case (length arguments)
      (1 (first arguments))
      (2 gdl)
      (t (list and (first arguments)
               (clean-and (cons and (rest arguments))))))))

(defun clean-gdl (gdl)
  (if (consp gdl)
    (case (car gdl)
      (ggp-rules::or (clean-or gdl))
      (ggp-rules::and (clean-and gdl))
      (t (cons (clean-gdl (car gdl))
               (clean-gdl (cdr gdl)))))
    gdl))


;;;; Reasoner -----------------------------------------------------------------
(defun load-gdl-preamble (db)
  (push-logic-frame-with db
    (rule db (ggp-rules::not ?x) (call ?x) ! fail)
    (fact db (ggp-rules::not ?x))

    (rule db (ggp-rules::or ?x ?y) (call ?x))
    (rule db (ggp-rules::or ?x ?y) (call ?y))

    (rule db (ggp-rules::and ?x ?y) (call ?x) (call ?y))

    (rule db (ggp-rules::distinct ?x ?x) ! fail)
    (fact db (ggp-rules::distinct ?x ?y))))

(defun make-reasoner-database ()
  (let ((db (temperance:make-database)))
    (load-gdl-preamble db)
    db))


(defclass reasoner ()
  ((database :initform (make-reasoner-database) :reader reasoner-database)
   (current-state :initform nil :accessor reasoner-state)
   (current-moves :initform nil :accessor reasoner-moves)))


(defun load-rule (rule)
  (if (gdl-rule-p rule)
    (apply #'invoke-rule t (rest rule))
    (invoke-fact t rule)))

(defun load-rules-into-reasoner (reasoner rules)
  (with-database (reasoner-database reasoner)
    (push-logic-frame-with t
      (map nil #'load-rule (clean-gdl rules)))))


(defun make-reasoner (rules)
  "Create and return a reasoner for the given GDL `rules`.

  `rules` should be a list of GDL rules with the symbols interned into the
  appropriate packages.  `ggp:player-start-game` will give you this, or you can
  use `ggp:read-gdl-from-file` to get them without a player if you want to just
  poke at the reasoner.

  "
  (let ((reasoner (make-instance 'reasoner)))
    (load-rules-into-reasoner reasoner rules)
    reasoner))


(defun apply-state (reasoner state)
  (push-logic-frame-with t
    (loop :for fact :in state
          :do (invoke-fact t `(ggp-rules::true ,fact))))
  (setf (reasoner-state reasoner) state))

(defun apply-moves (reasoner moves)
  (push-logic-frame-with t
    (loop :for (role . action) :in moves
          :do (invoke-fact t `(ggp-rules::does ,role ,action))))
  (setf (reasoner-moves reasoner) moves))


(defun clear-state (reasoner)
  (pop-logic-frame (reasoner-database reasoner))
  (setf (reasoner-state reasoner) nil))

(defun clear-moves (reasoner)
  (pop-logic-frame (reasoner-database reasoner))
  (setf (reasoner-moves reasoner) nil))


(defun ensure-state (reasoner state)
  (when (not (eql state (reasoner-state reasoner)))
    (when (not (null (reasoner-moves reasoner)))
      (clear-moves reasoner))
    (when (not (null (reasoner-state reasoner)))
      (clear-state reasoner))
    (apply-state reasoner state)))

(defun ensure-moves (reasoner moves)
  (when (not (eql moves (reasoner-moves reasoner)))
    (when (not (null (reasoner-moves reasoner)))
      (clear-moves reasoner))
    (apply-moves reasoner moves)))


(defun initial-state (reasoner)
  "Return the initial state of `reasoner`."
  (normalize-state
    (query-for (reasoner-database reasoner) ?what
               (ggp-rules::init ?what))))

(defun next-state (reasoner state moves)
  "Compute and return the successor to `state`, assuming `moves` were made.

  `moves` should be an alist of `(role . move)` pairs, which is what
  `ggp:player-update-game` will give you.

  "
  (with-database (reasoner-database reasoner)
    (ensure-state reasoner state)
    (ensure-moves reasoner moves)
    (normalize-state
      (query-for t ?what (ggp-rules::next ?what)))))


(defun legal-moves (reasoner state)
  "Return an alist of `(role . move)` for all legal moves in `state`."
  (with-database (reasoner-database reasoner)
    (ensure-state reasoner state)
    (dedupe (loop :for move :in (query-all t (ggp-rules::legal ?role ?action))
                  :collect (cons (getf move '?role)
                                 (getf move '?action))))))

(defun legal-moves-for (reasoner state role)
  "Return a list of legal moves for `role` in `state`.

  `ggp:player-select-move` must return exactly one of the items in this list.

  "
  (with-database (reasoner-database reasoner)
    (ensure-state reasoner state)
    (dedupe (invoke-query-for t '?action `(ggp-rules::legal ,role ?action)))))


(defun goal-values (reasoner state)
  "Return an alist of `(role . value)` pairs of goal values for `state`.

  Note that the GDL spec only requires that such values have meaning in terminal
  states.  Game authors sometimes add goal values to nonterminal states, but
  this is probably not something you should rely on.

  "
  (with-database (reasoner-database reasoner)
    (ensure-state reasoner state)
    (dedupe (loop :for goal :in (query-all t (ggp-rules::goal ?role ?value))
                  :collect (cons (getf goal '?role)
                                 (getf goal '?value))))))

(defun goal-value-for (reasoner state role)
  "Return the goal value for `role` in `state`, or `nil` if none exists.

  Note that the GDL spec only requires that such values have meaning in terminal
  states.  Game authors sometimes add goal values to nonterminal states, but
  this is probably not something you should rely on.

  "
  (with-database (reasoner-database reasoner)
    (ensure-state reasoner state)
    (car (invoke-query-for t '?value `(ggp-rules::goal ,role ?value)))))


(defun roles (reasoner)
  "Return a fresh list of all the roles of `reasoner`."
  (remove-duplicates
    (query-for (reasoner-database reasoner) ?who
               (ggp-rules::role ?who))))


(defun terminalp (reasoner state)
  "Return whether `state` is terminal."
  (with-database (reasoner-database reasoner)
    (ensure-state reasoner state)
    (prove t ggp-rules::terminal)))

