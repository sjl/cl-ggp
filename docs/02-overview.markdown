Overview
========

This document assumes you know what [General Game Playing][] is, what [GDL][]
is, and how the GGP community/competitions/etc work.

[General Game Playing]: http://ggp.org/
[GDL]: https://en.wikipedia.org/wiki/Game_Description_Language

[TOC]

GGP Protocol
------------

The `cl-ggp` system handles the GGP network protocol and game flow for you.
Players are implemented as CLOS objects.

### Basics

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

### Game Functionality

`cl-ggp` defines four generic methods that are called on players at various
points in each game.  You can provide method definitions for some or all of
these to let your player do whatever it needs to do.

At a minimum you **must** implement `player-select-move`.  The others are
optional and will default to doing nothing.

#### player-start-game

    :::lisp
    (defmethod ggp:player-start-game ((player YOUR-PLAYER) rules role timeout)
      ...)

This is called when a new game starts.

`rules` is the GDL rules of the game, parsed into Lisp lists/symbols.  You'll
probably want to feed this into a logic library.

`role` is a symbol representing which role you've been assigned.

`timeout` is the timestamp that the response to the server is due by, in
internal-real time units (more on this later).

#### player-update-game

    :::lisp
    (defmethod ggp:player-update-game ((player YOUR-PLAYER) moves)
      ...)

This is called once per turn, to allow you to update the game state with the
moves each player selected.

`moves` will be an association list of `(role . move)` conses representing the
moves made by each player last turn.

#### player-select-move

    :::lisp
    (defmethod ggp:player-select-move ((player YOUR-PLAYER) timeout)
      ...)

This is called once per turn.  It needs to return the move your player wants to
do.  All players **must** implement this function.

`timeout` is the timestamp that the response to the server is due by, in
internal-real time units (more on this later).

#### player-stop-game

    :::lisp
    (defmethod ggp:player-stop-game ((player YOUR-PLAYER))
      ...)

This is called when the game is stopped.  You can use it for things like tearing
down any extra data structures you've made, suggesting a GC to your Lisp, etc.

### Timeouts

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

### Symbols

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
(except for a few special symbols that survive the clearing) to prevent
mountains of garbage symbols from building up over time, especially when GDL
scrambing is enabled on the server.

This means that when your player's methods get symbols in their input (i.e. in
the `rules`, `role`, and `moves` arguments) those symbols will be interned in
`GGP-RULES`.  When your player returns a move to make from `player-select-move`,
any symbols inside it must be interned in `GGP-RULES` for things to work
correctly.

This is kind of shitty, and the author is aware of that.  Suggestions for less
shitty alternatives that still feel vaguely lispy are welcome.

Reasoning
---------

The `cl-ggp.reasoner` system contains a Prolog-based GDL reasoner based on the
[Temperance][] logic programming library.

[Temperance]: https://sjl.bitbucket.io/temperance/

It's useful as a starting point if you just want to get a bot up and running
quickly.  If you want more speed or control over the reasoning process you'll
probably want to replace it with your own implementation.

You can make a reasoner for a set of GDL rules (e.g. the rules given to you by
`player-start-game`) with `(make-reasoner rules)`.

Once you've got a reasoner you can ask it for the initial state of the game with
`(initial-state reasoner)`.  This will give you back a state object — it's
currently just a list, but this may change in the future, so you should just
treat it as an opaque object.

Once you've got a state and a set of moves you can compute the next state with
`(next-state reasoner current-state moves)`.

States can be queried for their terminality, goal values, and legal moves with
`terminalp`, `goal-values-for`, and `legal-moves-for` respectively.

See the [Reasoner API Reference][reasoner] for more details.

[reasoner]: ../reference-reasoner/

Example Player
--------------

Let's create a small example player that uses the reasoner to play any GGP game
legally.  We'll start by creating a class for the player, with three slots to
keep track of the data we need to play a game:

    :::lisp
    (defclass random-player (ggp:ggp-player)
      ((role          :accessor p-role)
       (current-state :accessor p-current-state)
       (reasoner      :accessor p-reasoner)))

Now we can implement `player-start-game`.  We'll store the role we've been
assigned, and create a reasoner so we can compute our legal moves later.  We'll
ignore the deadline because we're not doing any extensive processing:

    :::lisp
    (defmethod ggp:player-start-game
        ((player random-player) rules role deadline)
      (declare (ignore deadline))
      (setf (p-role player) role
            (p-reasoner player) (ggp.reasoner:make-reasoner rules)))

Next we'll implement `player-update-game`, which will compute the current state
of the game and store it in the `current-state` slot:

    :::lisp
    (defmethod ggp:player-update-game
        ((player random-player) moves)
      (setf (p-current-state player)
            (if (null moves)
              (ggp.reasoner:initial-state (p-reasoner player))
              (ggp.reasoner:next-state (p-reasoner player)
                                       (p-current-state player)
                                       moves))))

If `moves` is null we ask the reasoner for the initial state, otherwise we
compute the next state from the current one and the moves that were made.

Now we can implement `player-select-move`.  We'll just ask the reasoner for all
our legal moves and choose one at random.  If we wanted to make a smarter
player, this is where we would search the game tree to find a good move:

    :::lisp
    (defmethod ggp:player-select-move
        ((player random-player) deadline)
      (declare (ignore deadline))
      (let ((moves (ggp.reasoner:legal-moves-for
                     (p-reasoner player)
                     (p-current-state player)
                     (p-role player))))
        (nth (random (length moves)) moves)))

Finally we can implement `player-stop-game`.  We'll just clear out the player's
slots so the data can be garbage collected:

    :::lisp
    (defmethod ggp:player-stop-game
        ((player random-player))
      (setf (p-current-state player) nil
            (p-reasoner player) nil
            (p-role player) nil))

Now we can make an instance of our player and start it up!

    :::lisp
    (defvar *random-player*
      (make-instance 'random-player
                     :name "RandomPlayer"
                     :port 4000))

    (ggp:start-player *random-player*)
