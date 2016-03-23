Overview
========

`cl-ggp` handles the GGP protocol for you.  Players are implemented as CLOS
objects.

This document assumes you know what General Game Playing is, what GDL is, and
how the GGP community/competitions/etc work.

[TOC]

Basics
------

You can create your own player by extending the `ggp-player` class, creating an
object, and calling `start-player` on it to fire it up:

    :::lisp
    (defclass simple-player (ggp:ggp-player)
      ())

    (defvar *player* (make-instance 'simple-player
                                    :name "SimplePlayer"
                                    :port 4000))

    (ggp:start-player *player*)

`ggp-player` takes `:name` and `:port` initargs, which do what you think they
do.  It has a few other internal slots you shouldn't mess with.

You can kill a player with `kill-player`.

Functionality
-------------

`cl-ggp` defines four generic methods that are called on players at various
points in each game.  You can provide method definitions for some or all of
these to let your player do whatever it needs to do.

At a minimum you **must** implement `player-select-move`.  The others are
optional and will default to doing nothing.

### player-start-game

    :::lisp
    (defmethod player-start-game ((player YOUR-PLAYER) rules role timeout)
      ...)

This is called when a new game starts.

`rules` is the GDL rules of the game, parsed into Lisp lists/symbols.  You'll
probably want to feed this into a logic library.

`role` is a symbol representing which role you've been assigned.

`timeout` is the timestamp that the response to the server is due by, in
internal-real time units (more on this later).

### player-update-game

    :::lisp
    (defmethod player-update-game ((player YOUR-PLAYER) moves)
      ...)

This is called once per turn, to allow you to update the game state with the
moves each player selected.

`moves` will be an association list of `(role . move)` conses representing the
moves made by each player last turn.

### player-select-move

    :::lisp
    (defmethod player-select-move ((player YOUR-PLAYER) timeout)
      ...)

This is called once per turn.  It needs to return the move your player wants to
do.  All players **must** implement this function.

`timeout` is the timestamp that the response to the server is due by, in
internal-real time units (more on this later).

### player-stop-game

    :::lisp
    (defmethod player-stop-game ((player YOUR-PLAYER))
      ...)

This is called when the game is stopped.  You can use it for things like tearing
down any extra data structures you've made, suggesting a GC to your Lisp, etc.

Timeouts
--------

The GGP protocol specifies time limits for players.

When the initial game description is sent, players have a limited amount of time
for "metagaming" where they might process the rules, build alternate
representations (e.g. a propnet), start searching the game's DAG, etc.

Once the initial "metagaming" phase is over, the players must each choose a move
in every round, and there is a time limit on how long it takes them to respond.

`cl-ggp` mostly handles the annoying work of calculating the time your methods
have available for work, but there are a few caveats.

First: the `timestamp` arguments your methods get are timestamps of
internal-real time.  If you're not familiar with how interal time works in
Common Lisp, you should fix that.  Read up on [get-internal-real-time][] and
[internal-time-units-per-second][].

So you need to finish responding to the request by the internal-real timestamp
given.  This brings us to the second caveat: "finishing responding" includes
returning back up the call stack and sending the HTTP response back to the game
server.  It's probably wise to bake a bit of breathing room into your player and
not use *all* the given time in `timeout`, but `cl-ggp` doesn't try to decide
how much time to reserve.  You should decide that based on things like:

* Your ping to the GGP server.
* How likely it is for your Lisp process to get descheduled by your OS, and how
  long it might take to start running again.
* Worst-case duration of a GC pause right before sending the response.
* How brave you're feeling today.

In a nutshell: when `(get-internal-real-time)` returns the number given to you
in `timeout`, your message better have already reached the server.

[get-internal-real-time]: http://www.lispworks.com/documentation/HyperSpec/Body/f_get_in.htm#get-internal-real-time
[internal-time-units-per-second]: http://www.lispworks.com/documentation/HyperSpec/Body/v_intern.htm#internal-time-units-per-second

Symbols
-------

The other tricky part about `cl-ggp` is how it handles symbols.

Game descriptions are written in GDL, a fragment of which might look like this:

    (role x)
    (role o)
    (init (control x))

    (<= (legal ?role (mark ?row ?col ?role))
      (control ?role)
      (is-blank ?row ?col))

This is obviously pretty close to Lisp &mdash; it's just a bunch of lists of
symbols &mdash; so reading it in is almost trivial.  The main question is which
package the symbols get interned into.

`cl-ggp` interns all GDL symbols into a separate package called `GGP-RULES` to
prevent polluting other packages.  It also clears this package between matches
(except for a few special symbols that survive the clearing) to prevent building
up mountains of garbage symbols from building up over time, especially when GDL
scrambing is enabled on the server.

This means that when your player's methods get symbols in their input (i.e. in
the `rules`, `role`, and `moves` arguments) those symbols will be interned in
`GGP-RULES`.  When your player returns a move to make from `player-select-move`,
any symbols inside it must be interned in `GGP-RULES` for things to work
correctly.

This is kind of shitty, and the author is aware of that.  Suggestions for less
shitty alternatives that still feel vaguely lispy are welcome.

Example Player
--------------

`cl-ggp` is pretty bare bones, so it's tough to show an example player on its
own without bringing in a logic library.  But we can at least sketch out
a stupid little player that just returns the same move all the time, regardless
of whether it's valid or not, just to show the end-to-end process of creating
a player.

First we'll define the player class and implement the required
`player-select-move` method for it:

    :::lisp
    (defclass simple-player (ggp:ggp-player)
      ())

    (defmethod ggp:player-select-move ((player simple-player) timeout)
      'ggp-rules::wait)

Our player doesn't store any state of its own, so it doesn't need any extra
slots.  Notice how `player-select-move` returns a symbol from the `GGP-RULES`
package as discussed above.

The move our stupid player always returns is `WAIT`.  If the game supports that
move we'll make it every time, otherwise the game server will reject it as
invalid and just choose a random move for us.

Now we can actually create a player:

    :::lisp
    (defvar *player*
      (make-instance 'simple-player
                     :name "SimplePlayer"
                     :port 5000))

And fire it up:

    :::lisp
    (ggp:start-player *player*)

Now we can play a few games with it.  We'll probably lose every time unless
we're playing an unscrambled game of [Don't Press the Button][dptb].

Once we're done we can kill it to free up the port:

    :::lisp
    (ggp:kill-player *player*)

[dptb]: https://bitbucket.org/snippets/sjl/erRjL
