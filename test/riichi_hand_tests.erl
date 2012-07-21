-module(riichi_hand_tests).

-include("riichi.hrl").
-include_lib("eunit/include/eunit.hrl").

find_hands_test() ->
    Tiles = [#tile{value=N, suit=pin} || N <- [1,2,3], _ <- [1,2]],
    SetHand = #hand{sets=[#set{count=2, tile=#tile{value=N, suit=pin}, open=false} || N <- [3,2,1]]},
    SeqHand = #hand{sets=[#seq{tiles=[#tile{value=N, suit=pin} || N <- [1,2,3]], open=false} || _ <- [1,2]]},
    Found = [H || H <- riichi_hand:find(Tiles), H#hand.tiles =:= []],
    ?assertEqual([SetHand, SeqHand], Found).
