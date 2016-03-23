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
    :kill-player
    ))

