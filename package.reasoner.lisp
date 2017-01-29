(defpackage :ggp.reasoner
  (:use :cl :temperance)
  (:export
    :make-reasoner
    :initial-state
    :next-state
    :terminalp
    :legal-moves-for
    :goal-value-for)
  (:documentation "The package containing a simple GGP reasoner."))


