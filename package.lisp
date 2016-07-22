(defpackage #:ggp
  (:use #:cl #:optima)
  (:import-from #:optima #:match)
  (:export
    :ggp-player

    :player-start-game
    :player-update-game
    :player-select-move
    :player-stop-game

    :player-name
    :player-port

    :start-player
    :kill-player)
  (:documentation "The main GGP package.")
  )

(defpackage #:ggp-rules
  (:import-from #:cl #:nil) ; fuckin lol
  (:documentation
   "Symbol storage package.

  The `GGP-RULES` package is used to hold all the symbols in the GDL game
  descriptions, as well as some special symbols in the GGP protocol.  It is
  cleared between game runs to avoid a buildup of garbage symbols (especially
  when GDL scrambling is turned on), though certain special symbols are allowed
  to survive the clearing.

  This is ugly.  I'm sorry.  I'm open to suggestions on better ways to do this.

  "))


