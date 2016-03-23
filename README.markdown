The GGP Protocol looks approximately like this:

    (START <MATCH ID> <ROLE> <GAME DESCRIPTION> <STARTCLOCK> <PLAYCLOCK>)
    READY

    (PLAY <MATCH ID> <PRIOR MOVES>)
    MOVE (explanation “...”) (taunt “...”)

    (STOP <MATCH ID> <PRIOR MOVES>)
    DONE

Undocumented, because lol:

    (INFO)
    ((NAME MYNAME) (STATUS ???) (SPECIES LOL))
