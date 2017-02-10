(asdf:defsystem :cl-ggp.reasoner
  :description "A reasoner to use as a starting point for General Game Playing clients."

  :author "Steve Losh <steve@stevelosh.com>"
  :maintainer "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on (:temperance
               :cl-ggp)

  :serial t
  :components ((:file "package.reasoner")
               (:module "src"
                :components ((:file "reasoner")))))

