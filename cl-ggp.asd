(asdf:defsystem #:cl-ggp
  :name "ggp"
  :description "A framework for writing General Game Playing clients."

  :author "Steve Losh <steve@stevelosh.com>"
  :maintainer "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "0.0.1"

  :depends-on (#:clack
               #:flexi-streams
               #:optima
               #:fare-quasiquote-optima
               #:fare-quasiquote-readtable)

  :serial t
  :components ((:file "package")
               (:module "src"
                :components ((:file "ggp")))))
