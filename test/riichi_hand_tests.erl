-module(riichi_hand_tests).

-include("riichi.hrl").
-include_lib("eunit/include/eunit.hrl").

find_sets_test() ->
    Tiles = [#tile{suit=man, value=V} || V <- lists:seq(1,8)],
    Expected = {[
                 #seq{tiles=[#tile{suit=man, value=V} || V <- lists:seq(1,3)], open=false},
                 #seq{tiles=[#tile{suit=man, value=V} || V <- lists:seq(4,6)], open=false}
                ],
                [#tile{suit=man, value=V} || V <- lists:seq(7,8)]
               },
    ?assertEqual(Expected, riichi_hand:find_seqs(Tiles)).  
find_duplicate_sets_test() ->
    Tiles = [#tile{suit=man, value=V} || V <- lists:seq(1,3), _ <- lists:seq(1,3)] ++
            [#tile{suit=man, value=4} || _ <- [1,2]],
    Expected = {[
                 #seq{tiles=[#tile{suit=man, value=V} || V <- lists:seq(1,3)], open=false}
                 || _ <- [1,2,3]
                ],
                [#tile{suit=man, value=4} || _ <- [1,2]]
               },
    ?assertEqual(Expected, riichi_hand:find_seqs(Tiles)).
