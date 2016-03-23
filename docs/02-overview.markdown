Overview
========

`cl-ggp` handles the GGP protocol for you.  Players are implemented as CLOS
objects.

[TOC]

Basics
------

You can create your own player by extending the `ggp-player` class, creating an
object, and calling `start-player` on it to fire it up:

    (defclass simple-player (ggp:ggp-player)
      ())

    (defvar *player* (make-instance 'simple-player
                                    :name "SimplePlayer"
                                    :port 4000))

    (ggp:start-player *player*)

`ggp-player` takes `:name` and `:port` initargs.  It has a few other internal
slots you shouldn't mess with.

You can kill a player with `kill-player`.

Functionality
-------------

`cl-ggp` defines four generic methods that are called on players at various
points in each game.  You can provide method definitions for some or all of
these to let your player do whatever it needs to do.

At a minimum you **must** implement `player-select-move`.  The others are
optional and will default to doing nothing.

### player-start-game

    (defmethod player-start-game ((player YOUR-PLAYER) rules role start-clock play-clock)
      ...)

This is called when a new game starts.

`rules` is the GDL rules of the game, parsed into Lisp lists/symbols.  You'll
probably want to feed this into a logic library.

`role` is a symbol representing which role you've been assigned.

`start-clock` is 

`play-clock` is 

### player-update-game

    (defmethod player-update-game ((player YOUR-PLAYER) moves)
      ...)

This is called once per turn, to update the game state with the moves each
player selected.

`moves` is a list of the moves made by all players.

### player-select-move

    (defmethod player-select-move ((player YOUR-PLAYER))
      ...)

This is called once per turn.  It should return the move your player wants to
do.  All players **must** implement this function.

### player-stop-game

    (defmethod player-stop-game ((player YOUR-PLAYER))
      ...)

This is called when the game is stopped.  You can use it for things like tearing
down any extra data structures you've made, suggesting a GC to your Lisp, etc.

Example Player
--------------

