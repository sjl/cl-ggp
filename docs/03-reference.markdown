# API Reference

The following is a list of all user-facing parts of `cl-ggp`.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

[TOC]

## Package GGP

### GGP-PLAYER (class)

The base class for a GGP player.  Custom players should extend this.

#### Slot NAME

* Allocation: INSTANCE
* Type: `STRING`
* Reader: `PLAYER-NAME`

The name of the player.

#### Slot PORT

* Allocation: INSTANCE
* Type: `(INTEGER 0)`
* Reader: `PLAYER-PORT`

The port the HTTP server should listen on.

#### Slot CURRENT-MATCH

* Allocation: INSTANCE

The ID of the current match the player is playing, or `nil` if it is waiting.  **Do not touch this.**

#### Slot SERVER

* Allocation: INSTANCE

The Clack server object of the player.  **Do not touch this.**  Use `start-player` and `kill-player` to start/stop the server safely.

### KILL-PLAYER (function)

    (KILL-PLAYER PLAYER)

Kill the HTTP server for the given player.

  This will **not** be done gently.  No cleanup will be performed if the player
  is in the middle of a game.  Be careful.

  

### PLAYER-SELECT-MOVE (generic function)

    (PLAYER-SELECT-MOVE PLAYER)

Called when it's time for the player to select a move to play.

  Must return a list/symbol of the GDL move to play.  Note that any symbols in
  the move should be ones that are interned in the `GGP` package.  The author is
  aware that this sucks and welcomes suggestions on how to make it less awful.

  

### PLAYER-START-GAME (generic function)

    (PLAYER-START-GAME PLAYER RULES ROLE START-CLOCK PLAY-CLOCK)

Called when the game is started.

  `rules` is a list of lists/symbols representing the GDL description of the
  game.  Note that all symbols are interned in the `GGP` package.

  

### PLAYER-STOP-GAME (generic function)

    (PLAYER-STOP-GAME PLAYER)

Called when the game is stopped.

  This is a good place to do any teardown stuff your player might need, or maybe
  to suggest a GC to your Lisp implementation.

  

### PLAYER-UPDATE-GAME (generic function)

    (PLAYER-UPDATE-GAME PLAYER MOVES)

Called after all player have made their moves.

  `moves` will be a list of moves made by the players.

  

### START-PLAYER (function)

    (START-PLAYER PLAYER)

Start the HTTP server for the given player.

