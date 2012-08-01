-module(riichi_yaku_tests).

-compile(export_all).

-include("../include/riichi.hrl").
-include_lib("eunit/include/eunit.hrl").

yakuhai_test_() ->
    Yakuhai = [lists:duplicate(3, #tile{suit=wind, value=east}),
               lists:duplicate(3, #tile{suit=wind, value=south})
              ] ++ [lists:duplicate(3, #tile{suit=dragon, value=V}) || V <- [red,white,green]],
    Tiles = [#tile{suit=pin, value=V} || V <- [1,2] ++ lists:seq(1,9)],
    Hands = [#hand{tiles=Tiles, melds=[#meld{type=pon, tiles=Y}]} || Y <- Yakuhai],
    Players = [#player{seat=south, hand=H} || H <- Hands],
    Games = [#game{round=east, players=[P]} || P <- Players],
    [?_assertEqual(1, yaku:yakuhai(G, P)) || G = #game{players=[P]} <- Games].

multiple_yakuhai_test() ->
    Yakuhai = [#meld{type=pon, tiles=lists:duplicate(3, #tile{suit=dragon, value=D})} || D <- [red,green,white]],
    Player = #player{seat=south, hand=#hand{melds=Yakuhai}},
    ?assertEqual(3, yaku:yakuhai(#game{}, Player)).

tanyao_test() ->
    Hand = #hand{melds=[#meld{type=pon, tiles=lists:duplicate(3, #tile{suit=pin, value=2})},
                        #meld{type=pair, tiles=lists:duplicate(2, #tile{suit=sou, value=8})},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [4,5,6]]},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [4,5,6]]},
                        #meld{type=chii, tiles=[#tile{suit=man, value=V} || V <- [6,7,8]]}]},
    ?assertEqual(true, yaku:tanyao(#game{}, #player{hand=Hand})).

pinfu_test() ->
    Hand = #hand{melds=[#meld{type=pair, tiles=lists:duplicate(2, #tile{suit=sou, value=8})},
                        #meld{type=chii, tiles=[#tile{suit=pin, value=V} || V <- [1,2,3]]},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [4,5,6]]},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [4,5,6]]},
                        #meld{type=chii, tiles=[#tile{suit=man, value=V} || V <- [6,7,8]]}]},
    ?assertEqual(true, yaku:pinfu(#game{}, #player{hand=Hand})).

chiitoitsu_test() ->
    Hand = #hand{melds=[#meld{type=pair, tiles=lists:duplicate(2, #tile{suit=pin, value=V})} || V <- lists:seq(1,7)]},
    ?assertEqual(true, yaku:chiitoitsu(#game{}, #player{hand=Hand})).

kokushi_musou_test() ->
    Hand = #hand{tiles=?TERMINALS ++ ?HONOURS -- [#tile{suit=pin, value=1}],
                melds=[#meld{type=pair, tiles=lists:duplicate(2, #tile{suit=pin, value=1})}]},
    ?assertEqual(true, yaku:kokushi_musou(#game{}, #player{hand=Hand})).
