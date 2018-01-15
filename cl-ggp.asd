(asdf:defsystem :cl-ggp
  :description "A framework for writing General Game Playing clients."

  :author "Steve Losh <steve@stevelosh.com>"
  :maintainer "Steve Losh <steve@stevelosh.com>"

  :license "MIT"
  :version "1.0.0"

  :depends-on (

               :bordeaux-threads
               :clack
               :fare-quasiquote-optima
               :fare-quasiquote-readtable
               :flexi-streams
               :optima

               )

  :serial t
  :components ((:file "package")
               (:module "src"
                :components ((:file "ggp")))))

