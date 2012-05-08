-module(riichi_tests).

-include("riichi.hrl").
-include_lib("eunit/include/eunit.hrl").

valid_tile_dragon_test_() ->
    [?_assert(riichi:is_valid_tile(#tile{suit=dragon, value=Value})) || Value <- [white, green, red]].
valid_tile_wind_test_() ->
    [?_assert(riichi:is_valid_tile(#tile{suit=wind, value=Value})) || Value <- [east, south, west, north]].
valid_tile_suit_test_() ->
    [?_assert(riichi:is_valid_tile(#tile{suit=Suit, value=Value})) || Suit <- [pin, sou, man], Value <- lists:seq(1,9)].

invalid_tile_test_() ->
    [?_assertNot(riichi:is_valid_tile(Tile))
        || Tile <-
            [
                #tile{suit=badsuit, value=1},
                #tile{suit=dragon, value=hidden},
                #tile{suit=wind, value=broken},
                #tile{suit=pin, value=0},
                #tile{suit=sou, value=-1},
                #tile{suit=man, value=10},
                not_a_tile
            ]
    ].

dora_dragon_test_() ->
    [?_assertEqual(riichi:dora(#tile{suit=dragon, value=Indicator}), #tile{suit=dragon, value=Dora})
        || {Indicator, Dora} <-
            [
                {white, green},
                {green, red},
                {red, white}
            ]
    ].

dora_wind_test_() ->
    [?_assertEqual(riichi:dora(#tile{suit=wind, value=Indicator}), #tile{suit=wind, value=Dora})
        || {Indicator, Dora} <-
            [
                {east, south},
                {south, west},
                {west, north},
                {north, east}
            ]
    ].

dora_suit_test_() ->
    [?_assertEqual(riichi:dora(#tile{suit=Suit, value=Indicator}), #tile{suit=Suit, value=Dora})
        ||
            {Indicator, Dora} <-
                [{1,2}, {2,3}, {3,4}, {4,5}, {5,6}, {6,7}, {7,8}, {8,9}, {9,1}],
            Suit <- [pin, sou, man]
    ].

dora_invalid_test() ->
    ?assertException(throw, {error, invalid_tile}, riichi:dora(#tile{suit=pin, value=9001})).

nearest_test_() ->
    [?_assertEqual(riichi:nearest(Val, 10), 80) || Val <- lists:seq(71,80)].

score_yaku_limit_test_() ->
    [?_assertEqual(riichi:score(30, Han, true), Score)
        || {Han, Score} <-
            [
                { 5, 2000},
                { 6, 3000},
                { 7, 3000},
                { 8, 4000},
                { 9, 4000},
                {10, 4000},
                {11, 6000},
                {12, 6000},
                {13, 8000},
                {14, 8000},
                {1000, 8000}
            ]
    ].

score_fu_limit_test() ->
    ?assertEqual(riichi:score(80, 4, true), 2000).

score_rounded_test() ->
    ?assertEqual(riichi:score(20, 1, true), 200). % 160 rounded up

score_hand_dai_san_gan_test() ->
    Hand = #hand{
        tiles = lists:flatten([
            lists:duplicate(3, #tile{suit=dragon, value=red}),
            lists:duplicate(3, #tile{suit=dragon, value=white}),
            lists:duplicate(3, #tile{suit=dragon, value=green}),
            lists:duplicate(3, #tile{suit=wind, value=east}),
            lists:duplicate(2, #tile{suit=wind, value=north})])
    },
    ?assertEqual(riichi:score_hand(Hand, 30), 8000).

score_hand_kokushi_musou_test() ->
    Hand = #hand{
        tiles = lists:flatten([
            [#tile{suit=S, value=V} || S <- [pin, sou, man], V <- [1,9]], % Terminals
            [#tile{suit=wind, value=V} || V <- [east, south, west, north]], % Winds
            [#tile{suit=dragon, value=V} || V <- [red, white, green]], % Dragons
            [#tile{suit=pin, value=1}]])
    },
    ?assertEqual(riichi:score_hand(Hand, 30), 8000).
