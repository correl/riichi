-record(tile, {
    suit,
    value
}).

-record(hand, {
    tiles=[],
    sets=[],
    seqs=[]
}).

-record(set, {
    count,
    tile,
    open=true
}).

-record(seq, {
    tiles,
    open=true
}).
