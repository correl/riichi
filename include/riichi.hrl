-type wind()   :: east | south | west | north.
-type dragon() :: green | red | white.

-record(tile, {
          suit       :: pin | man | sou | wind | dragon,
          value      :: integer() | wind() | dragon(),
          from=draw  :: draw | wind()
}).
-type tile() :: #tile{}.

-record(meld, {
          type  :: pair | chii | pon | kan,
          tiles :: [tile()]
}).
-type meld() :: #meld{}.

-record(hand, {
          tiles=[] :: [tile()],
          melds=[] :: [meld()]
}).
-type hand() :: #hand{}.
