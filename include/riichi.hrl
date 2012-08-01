-define(SIMPLES, [#tile{suit=S, value=V} || S <- [pin, man, sou], V <- lists:seq(2,8)]).
-define(TERMINALS, [#tile{suit=S, value=V} || S <- [pin, man, sou], V <- [1,9]]).
-define(DRAGONS, [#tile{suit=dragon, value=V} || V <- [green, red, white]]).
-define(WINDS, [#tile{suit=wind, value=V} || V <- [east, south, west, north]]).
-define(HONOURS, ?DRAGONS ++ ?WINDS).
-define(TILES, ?SIMPLES ++ ?TERMINALS ++ ?HONOURS).

%% @type wind() = east | south | west | north
-type wind()   :: east | south | west | north.

%% @type dragon() = green | red | white
-type dragon() :: green | red | white.

%% @type tile() = {tile, Suit, Value, From}
%%     Suit  = pin | man | sou | wind | dragon
%%     Value = integer() | wind() | dragon()
%%     From  = draw | wind()
-record(tile, {
          suit       :: pin | man | sou | wind | dragon,
          value      :: integer() | wind() | dragon(),
          from=draw  :: draw | wind()
}).
-type tile() :: #tile{}.

%% @type meld() = {meld, Type, Tiles}
%%     Type = pair | chii | pon | kan
%%     Tiels = [tile()]
-record(meld, {
          type  :: pair | chii | pon | kan,
          tiles :: [tile()]
}).
-type meld() :: #meld{}.

%% @type hand() = {hand, Tiles, Melds}
%%     Tiles = [tile()]
%%     Melds = [meld()]
-record(hand, {
          tiles=[] :: [tile()],
          melds=[] :: [meld()]
}).
-type hand() :: #hand{}.

%% @type player() = {player, Name, Seat, Hand, Discards, Drawn}
%%     Name = string()
%%     Seat = wind()
%%     Hand = hand()
%%     Discards = [tile()]
%%     Drawn = none | {tsumo | ron, tile()}
-record(player, {
          name     :: string(),
          seat     :: wind(),
          hand     :: hand(),
          discards :: [tile()],
          drawn    :: none | {tsumo | ron, tile()}
}).
-type player() :: #player{}.

%% @type phase() = Phase
%%     Phase = draw | discard
-type phase() :: draw | discard.

%% @type game() = {game, Rounds, Timeout, Round, Turn, Phase, Wall, Rinshan, Dora, Uradora, Players}
%%     Rounds = integer()
%%     Timeout = integer() | infinity
%%     Round = wind()
%%     Turn = wind()
%%     Phase = phase()
%%     Wall = [tile()]
%%     Rinshan = [tile()]
%%     Dora = [tile()]
%%     Uradora = [tile()]
%%     Players = [player()]
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
