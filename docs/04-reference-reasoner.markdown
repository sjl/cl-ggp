# Reasoner API Reference

cl-ggp includes a simple Prolog-based reasoner you can use as a starting point when writing general game players in the `cl-ggp.reasoner` system.

  [TOC]

## Package `GGP.REASONER`

This package contains a simple GGP reasoner.  It can be useful as a starting point for writing general game players.

### `GOAL-VALUE-FOR` (function)

    (GOAL-VALUE-FOR REASONER STATE ROLE)

Return the goal value for `role` in `state`, or `nil` if none exists.

  Note that the GDL spec only requires that such values have meaning in terminal
  states.  Game authors sometimes add goal values to nonterminal states, but
  this is probably not something you should rely on.

  

### `INITIAL-STATE` (function)

    (INITIAL-STATE REASONER)

Return the initial state of `reasoner`.

### `LEGAL-MOVES-FOR` (function)

    (LEGAL-MOVES-FOR REASONER STATE ROLE)

Return a list of legal moves for `role` in `state`.

  `ggp:player-select-move` must return exactly one of the items in this list.

  

### `MAKE-REASONER` (function)

    (MAKE-REASONER RULES)

Create and return a reasoner for the given GDL `rules`.

  `rules` should be a list of GDL rules with the symbols interned into the
  appropriate packages.  `ggp:player-start-game` will give you this, or you can
  use `ggp:read-gdl-from-file` to get them without a player if you want to just
  poke at the reasoner.

  

### `NEXT-STATE` (function)

    (NEXT-STATE REASONER STATE MOVES)

Compute and return the successor to `state`, assuming `moves` were made.

  `moves` should be an alist of `(role . move)` pairs, which is what
  `ggp:player-update-game` will give you.

  

### `TERMINALP` (function)

    (TERMINALP REASONER STATE)

Return whether `state` is terminal.

