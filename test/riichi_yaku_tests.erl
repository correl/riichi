-module(riichi_yaku_tests).

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
    ?assertEqual(true, yaku:pinfu(#game{}, #player{hand=Hand, drawn={tsumo, #tile{suit=sou, value=6}}})).

iipeikou_test() ->
    Hand = #hand{melds=[#meld{type=pair, tiles=lists:duplicate(2, #tile{suit=sou, value=8})},
                        #meld{type=chii, tiles=[#tile{suit=pin, value=V} || V <- [1,2,3]]},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [4,5,6]]},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [4,5,6]]},
                        #meld{type=chii, tiles=[#tile{suit=man, value=V} || V <- [6,7,8]]}]},
    ?assertEqual(true, yaku:iipeikou(#game{}, #player{hand=Hand, drawn={tsumo, #tile{suit=sou, value=6}}})).

ryanpeikou_not_iipeikou_test() ->
    Hand = #hand{melds=[#meld{type=pair, tiles=lists:duplicate(2, #tile{suit=sou, value=8})},
                        #meld{type=chii, tiles=[#tile{suit=pin, value=V} || V <- [1,2,3]]},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [4,5,6]]},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [4,5,6]]},
                        #meld{type=chii, tiles=[#tile{suit=pin, value=V} || V <- [1,2,3]]}]},
    ?assertEqual(false, yaku:iipeikou(#game{}, #player{hand=Hand, drawn={tsumo, #tile{suit=sou, value=6}}})).

chanta_test() ->
    Hand = #hand{melds=[#meld{type=pair, tiles=lists:duplicate(2, #tile{suit=dragon, value=red})},
                        #meld{type=chii, tiles=[#tile{suit=pin, value=V} || V <- [1,2,3]]},
                        #meld{type=pon,  tiles=[#tile{suit=sou, value=V} || V <- [1,1,1]]},
                        #meld{type=pon,  tiles=[#tile{suit=sou, value=V} || V <- [9,9,9]]},
                        #meld{type=chii, tiles=[#tile{suit=man, value=V} || V <- [7,8,9]]}]},
    ?assertEqual(true, yaku:chanta(#game{}, #player{hand=Hand, drawn={tsumo, #tile{suit=dragon, value=red}}})).

itsuu_test() ->
    Hand = #hand{melds=[#meld{type=pair, tiles=lists:duplicate(2, #tile{suit=sou, value=8})},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [1,2,3]]},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [4,5,6]]},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [7,8,9]]},
                        #meld{type=chii, tiles=[#tile{suit=man, value=V} || V <- [6,7,8]]}]},
    ?assertEqual(true, yaku:itsuu(#game{}, #player{hand=Hand, drawn={tsumo, #tile{suit=dragon, value=red}}})).

san_shoku_doujun_test() ->
    Hand = #hand{melds=[#meld{type=pair, tiles=lists:duplicate(2, #tile{suit=sou, value=8})},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [1,2,3]]},
                        #meld{type=chii, tiles=[#tile{suit=man, value=V} || V <- [1,2,3]]},
                        #meld{type=chii, tiles=[#tile{suit=pin, value=V} || V <- [1,2,3]]},
                        #meld{type=chii, tiles=[#tile{suit=man, value=V} || V <- [6,7,8]]}]},
    ?assertEqual(true, yaku:san_shoku_doujun(#game{}, #player{hand=Hand, drawn={tsumo, #tile{suit=dragon, value=red}}})).

san_shoku_douko_test() ->
    Hand = #hand{melds=[#meld{type=pair, tiles=lists:duplicate(2, #tile{suit=sou, value=8})},
                        #meld{type=pon, tiles=[#tile{suit=sou, value=V} || V <- [1,1,1]]},
                        #meld{type=pon, tiles=[#tile{suit=man, value=V} || V <- [1,1,1]]},
                        #meld{type=pon, tiles=[#tile{suit=pin, value=V} || V <- [1,1,1]]},
                        #meld{type=chii, tiles=[#tile{suit=man, value=V} || V <- [6,7,8]]}]},
    ?assertEqual(true, yaku:san_shoku_douko(#game{}, #player{hand=Hand, drawn={tsumo, #tile{suit=dragon, value=red}}})).

san_kan_tsu_test() ->
    Hand = #hand{melds=[#meld{type=pair, tiles=lists:duplicate(2, #tile{suit=sou, value=8})},
                        #meld{type=kan, tiles=[#tile{suit=sou, value=V} || V <- [1,1,1,1]]},
                        #meld{type=kan, tiles=[#tile{suit=man, value=V} || V <- [1,1,1,1]]},
                        #meld{type=kan, tiles=[#tile{suit=pin, value=V} || V <- [1,1,1,1]]},
                        #meld{type=chii, tiles=[#tile{suit=man, value=V} || V <- [6,7,8]]}]},
    ?assertEqual(true, yaku:san_kan_tsu(#game{}, #player{hand=Hand, drawn={tsumo, #tile{suit=dragon, value=red}}})).

toi_toi_test() ->
    Hand = #hand{melds=[#meld{type=pair, tiles=lists:duplicate(2, #tile{suit=sou, value=8})},
                        #meld{type=kan, tiles=[#tile{suit=sou, value=V} || V <- [1,1,1,1]]},
                        #meld{type=kan, tiles=[#tile{suit=man, value=V} || V <- [1,1,1,1]]},
                        #meld{type=pon, tiles=[#tile{suit=pin, value=V} || V <- [2,2,2]]},
                        #meld{type=pon, tiles=[#tile{suit=pin, value=V} || V <- [8,8,8]]}]},
    ?assertEqual(true, yaku:toi_toi(#game{}, #player{hand=Hand, drawn={tsumo, #tile{suit=dragon, value=red}}})).
    
san_an_kou_test() ->
    Hand = #hand{melds=[#meld{type=pair, tiles=lists:duplicate(2, #tile{suit=sou, value=8})},
                        #meld{type=kan, tiles=[#tile{suit=sou, value=V} || V <- [1,1,1,1]]},
                        #meld{type=kan, tiles=[#tile{suit=man, value=V} || V <- [1,1,1,1]]},
                        #meld{type=pon, tiles=[#tile{suit=pin, value=V} || V <- [1,1,1]]},
                        #meld{type=chii, tiles=[#tile{suit=man, value=V} || V <- [6,7,8]]}]},
    ?assertEqual(true, yaku:san_an_kou(#game{}, #player{hand=Hand, drawn={tsumo, #tile{suit=dragon, value=red}}})).

shou_san_gen_test() ->
    Hand = #hand{melds=[#meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [2,3,4]]},
                        #meld{type=pon, tiles=lists:duplicate(3, #tile{suit=dragon, value=red})},
                        #meld{type=pon, tiles=lists:duplicate(3, #tile{suit=dragon, value=green})},
                        #meld{type=kan, tiles=lists:duplicate(4, #tile{suit=pin, value=1})},
                        #meld{type=pair, tiles=lists:duplicate(2, #tile{suit=dragon, value=white})}]},
    ?assert(yaku:shou_san_gen(#game{}, #player{hand=Hand})).

honrouto_test() ->
    Hand = #hand{melds=[#meld{type=pon, tiles=lists:duplicate(3, #tile{suit=sou, value=1})},
                        #meld{type=pon, tiles=lists:duplicate(3, #tile{suit=sou, value=9})},
                        #meld{type=pon, tiles=lists:duplicate(3, #tile{suit=dragon, value=green})},
                        #meld{type=kan, tiles=lists:duplicate(4, #tile{suit=pin, value=1})},
                        #meld{type=pair, tiles=lists:duplicate(2, #tile{suit=dragon, value=white})}]},
    ?assert(yaku:honrouto(#game{}, #player{hand=Hand})).    

chiitoitsu_test() ->
    Hand = #hand{melds=[#meld{type=pair, tiles=lists:duplicate(2, #tile{suit=pin, value=V})} || V <- lists:seq(1,7)]},
    ?assertEqual(true, yaku:chiitoitsu(#game{}, #player{hand=Hand})).

honitsu_test() ->
    Hand = #hand{melds=[#meld{type=pair, tiles=lists:duplicate(2, #tile{suit=sou, value=8})},
                        #meld{type=pon, tiles=lists:duplicate(3, #tile{suit=dragon, value=red})},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [4,5,6]]},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [4,5,6]]},
                        #meld{type=pon, tiles=lists:duplicate(3, #tile{suit=sou, value=1})}]},
    ?assertEqual(true, yaku:honitsu(#game{}, #player{hand=Hand, drawn={tsumo, #tile{suit=sou, value=1}}})).

jun_chan_test() ->
    Hand = #hand{melds=[#meld{type=pair, tiles=lists:duplicate(2, #tile{suit=sou, value=9})},
                        #meld{type=chii, tiles=[#tile{suit=pin, value=V} || V <- [1,2,3]]},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [7,8,9]]},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [7,8,9]]},
                        #meld{type=chii, tiles=[#tile{suit=pin, value=V} || V <- [1,2,3]]}]},
    ?assertEqual(true, yaku:jun_chan(#game{}, #player{hand=Hand, drawn={tsumo, #tile{suit=sou, value=7}}})).

ryanpeikou_test() ->
    Hand = #hand{melds=[#meld{type=pair, tiles=lists:duplicate(2, #tile{suit=sou, value=8})},
                        #meld{type=chii, tiles=[#tile{suit=pin, value=V} || V <- [1,2,3]]},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [4,5,6]]},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [4,5,6]]},
                        #meld{type=chii, tiles=[#tile{suit=pin, value=V} || V <- [1,2,3]]}]},
    ?assertEqual(true, yaku:ryanpeikou(#game{}, #player{hand=Hand, drawn={tsumo, #tile{suit=sou, value=6}}})).

chinitsu_test() ->
    Hand = #hand{melds=[#meld{type=pair, tiles=lists:duplicate(2, #tile{suit=sou, value=8})},
                        #meld{type=pon, tiles=lists:duplicate(3, #tile{suit=sou, value=9})},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [4,5,6]]},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [4,5,6]]},
                        #meld{type=pon, tiles=lists:duplicate(3, #tile{suit=sou, value=1})}]},
    ?assertEqual(true, yaku:chinitsu(#game{}, #player{hand=Hand, drawn={tsumo, #tile{suit=sou, value=1}}})).

kokushi_musou_test() ->
    Hand = #hand{tiles=?T_TERMINALS ++ ?T_HONOURS -- [#tile{suit=pin, value=1}],
                melds=[#meld{type=pair, tiles=lists:duplicate(2, #tile{suit=pin, value=1})}]},
    ?assertEqual(true, yaku:kokushi_musou(#game{}, #player{hand=Hand})).

ryuu_iisou_test() ->
    Hand = #hand{melds=[#meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [2,3,4]]},
                        #meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [2,3,4]]},
                        #meld{type=pon, tiles=lists:duplicate(3, #tile{suit=sou, value=6})},
                        #meld{type=pon, tiles=lists:duplicate(3, #tile{suit=sou, value=8})},
                        #meld{type=pair, tiles=lists:duplicate(2, #tile{suit=dragon, value=green})}]},
    ?assert(yaku:ryuu_iisou(#game{}, #player{hand=Hand})).

dai_san_gen_test() ->
    Hand = #hand{melds=[#meld{type=chii, tiles=[#tile{suit=sou, value=V} || V <- [2,3,4]]},
                        #meld{type=pon, tiles=lists:duplicate(3, #tile{suit=dragon, value=red})},
                        #meld{type=pon, tiles=lists:duplicate(3, #tile{suit=dragon, value=green})},
                        #meld{type=kan, tiles=lists:duplicate(4, #tile{suit=dragon, value=white})},
                        #meld{type=pair, tiles=lists:duplicate(2, #tile{suit=pin, value=1})}]},
    ?assert(yaku:dai_san_gen(#game{}, #player{hand=Hand})).
