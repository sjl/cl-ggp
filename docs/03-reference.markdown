# API Reference

The following is a list of all user-facing parts of `cl-ggp`.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

[TOC]

## Package `GGP`

The main GGP package.

### `GGP-PLAYER` (class)

The base class for a GGP player.  Custom players should extend this.

#### Slot `NAME`

* Allocation: `:INSTANCE`
* Type: `STRING`
* Initarg: `:NAME`
* Initform: `"CL-GGP"`
* Reader: `PLAYER-NAME`

The name of the player.

#### Slot `PORT`

* Allocation: `:INSTANCE`
* Type: `(INTEGER 0)`
* Initarg: `:PORT`
* Initform: `9999`
* Reader: `PLAYER-PORT`

The port the HTTP server should listen on.

#### Slot `MATCH-ROLES`

* Allocation: `:INSTANCE`
* Type: `(OR NULL LIST)`
* Initform: `NIL`
* Reader: `PLAYER-MATCH-ROLES`

A list of the roles for the current match.  Feel free to read and use this if you like.  **Do not modify this.**

#### Slot `START-CLOCK`

* Allocation: `:INSTANCE`
* Type: `(OR NULL (INTEGER 1))`
* Initform: `NIL`

The start clock for the current game.  **Do not touch this.**  Use the `timeout` value passed to your methods instead.

#### Slot `PLAY-CLOCK`

* Allocation: `:INSTANCE`
* Type: `(OR NULL (INTEGER 1))`
* Initform: `NIL`

The play clock for the current game.  **Do not touch this.**  Use the `timeout` value passed to your methods instead.

#### Slot `MESSAGE-START`

* Allocation: `:INSTANCE`
* Type: `(OR NULL (INTEGER 0))`
* Initform: `NIL`

The (internal-real) timestamp of when the current GGP message was received.  **Do not touch this.**  Use the `timeout` value passed to your methods instead.

#### Slot `CURRENT-MATCH`

* Allocation: `:INSTANCE`
* Initform: `NIL`

The ID of the current match the player is playing, or `nil` if it is waiting.  **Do not touch this.**

#### Slot `SERVER`

* Allocation: `:INSTANCE`

The Clack server object of the player.  **Do not touch this.**  Use `start-player` and `kill-player` to start/stop the server safely.

### `KILL-PLAYER` (function)

    (KILL-PLAYER PLAYER)

Kill the HTTP server for the given player.

  This will **not** be done gently.  No cleanup will be performed if the player
  is in the middle of a game.  Be careful.

  

### `PLAYER-SELECT-MOVE` (generic function)

    (PLAYER-SELECT-MOVE PLAYER TIMEOUT)

Called when it's time for the player to select a move to play.

  Must return a list/symbol of the GDL move to play.  Note that any symbols in
  the move should be ones that are interned in the `GGP-RULES` package.  The
  author is aware that this sucks and welcomes suggestions on how to make it
  less awful.

  `timeout` is the timestamp that the response to the server is due by, in
  internal-real time units.  Basically: when `(get-internal-real-time)` returns
  this number, your message better have reached the server.

  

### `PLAYER-START-GAME` (generic function)

    (PLAYER-START-GAME PLAYER RULES ROLE TIMEOUT)

Called when the game is started.

  `rules` is a list of lists/symbols representing the GDL description of the
  game.  Note that all symbols are interned in the `GGP-RULES` package.

  `role` is a symbol representing the role of the player in this game.

  `timeout` is the timestamp that the response to the server is due by, in
  internal-real time units.  Basically: when `(get-internal-real-time)` returns
  this number, your message better have reached the server.

  

### `PLAYER-STOP-GAME` (generic function)

    (PLAYER-STOP-GAME PLAYER)

Called when the game is stopped.

  This is a good place to do any teardown stuff your player might need, or maybe
  to suggest a GC to your Lisp implementation.

  

### `PLAYER-UPDATE-GAME` (generic function)

    (PLAYER-UPDATE-GAME PLAYER MOVES)

Called after all players have made their moves.

  `moves` will be a list of `(role . move)` conses representing moves made by
  each player last turn.

  

### `PLAYER-UPDATE-GAME-II` (generic function)

    (PLAYER-UPDATE-GAME-II PLAYER MOVE PERCEPTS)

Called after all players have made their moves in a GDL-II game.

  `move` will be the move you played last turn.

  `percepts` are all the percepts you see for the round.

  

### `START-PLAYER` (function)

    (START-PLAYER PLAYER &KEY (SERVER :HUNCHENTOOT) (USE-THREAD T))

Start the HTTP server for the given player.

  The `:server` and `:use-thread` options will be passed along to Clack.

  

## Package `GGP-RULES`

Symbol storage package.

  The `GGP-RULES` package is used to hold all the symbols in the GDL game
  descriptions, as well as some special symbols in the GGP protocol.  It is
  cleared between game runs to avoid a buildup of garbage symbols (especially
  when GDL scrambling is turned on), though certain special symbols are allowed
  to survive the clearing.

  This is ugly.  I'm sorry.  I'm open to suggestions on better ways to do this.

  

