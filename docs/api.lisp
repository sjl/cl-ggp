(ql:quickload "cl-d-api")

(d-api:generate-documentation
  :cl-ggp
  #p"docs/03-reference.markdown"
  (list "GGP" "GGP-RULES")
  "The following is a list of all user-facing parts of `cl-ggp`.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

"
  :title "Main API Reference")

(d-api:generate-documentation
  :cl-ggp.reasoner
  #p"docs/04-reference-reasoner.markdown"
  (list "GGP.REASONER")
  "cl-ggp includes a simple Prolog-based reasoner you can use as a starting point when writing general game players in the `cl-ggp.reasoner` system.

  "
  :title "Reasoner API Reference")

