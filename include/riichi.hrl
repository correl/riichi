-record(tile, {
    suit,
    value
}).

-record(hand, {
    tiles=[],
    sets=[]
}).

-record(set, {
    count,
    tile,
    open=true
}).
