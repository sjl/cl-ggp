(defpackage :ggp.reasoner
  (:use :cl :temperance)
  (:export
    :make-reasoner
    :initial-state
    :next-state
    :terminalp
    :legal-moves-for
    :goal-value-for
    :roles)
  (:documentation "This package contains a simple GGP reasoner.  It can be useful as a starting point for writing general game players."))


