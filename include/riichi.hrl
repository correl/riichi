-define(SIMPLES, [#tile{suit=S, value=V} || S <- [pin, man, sou], V <- lists:seq(2,8)]).
-define(TERMINALS, [#tile{suit=S, value=V} || S <- [pin, man, sou], V <- [1,9]]).
-define(DRAGONS, [#tile{suit=dragon, value=V} || V <- [green, red, white]]).
-define(WINDS, [#tile{suit=wind, value=V} || V <- [east, south, west, north]]).
-define(HONOURS, ?DRAGONS ++ ?WINDS).
-define(TILES, ?SIMPLES ++ ?TERMINALS ++ ?HONOURS).

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

-record(player, {
          name     :: string(),
          seat     :: wind(),
          hand     :: hand(),
          discards :: [tile()],
          drawn    :: none | {tsumo | ron, tile()}
}).
-type player() :: #player{}.

-type phase() :: draw | discard.

-record(game, {
          rounds=2         :: integer(),
          timeout=infinity :: integer() | infinity,
          round=east       :: wind(),
          turn=east        :: wind(),
          phase=draw       :: phase(),
          wall             :: [tile()],
          rinshan          :: [tile()],
          dora             :: [tile()],
          uradora          :: [tile()],
          players          :: [player()]
}).
-type game() :: #game{}.
