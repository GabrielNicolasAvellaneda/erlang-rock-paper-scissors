-module(rockpaperscissors).
-export([]).

-include_lib("eunit/include/eunit.hrl").

create_play(Player, Played) -> {Player, Played}.

%get_play(Play) -> element(2, Play). 

get_best_play(Play1 = {_, paper}, _Play2 = {_, rock}) -> Play1;
get_best_play(_Play1 = {_, rock}, Play2 = {_, paper}) -> Play2;
get_best_play(_Play1 = {_, paper}, Play2 = {_, scissors}) -> Play2;
get_best_play(Play1 = {_, scissors}, _Play2 = {_, paper}) -> Play1; 
get_best_play(Play1 = {_, rock}, _Play2 = {_, scissors}) -> Play1;
get_best_play(_Play1 = {_, scissors}, Play2 = {_, rock}) -> Play2;
get_best_play(_, _) -> undefined.

check_result(Play1, Play2) ->
	case (get_best_play(Play1, Play2)) of
		undefined -> {draw, Play1, Play2};
		BestPlay -> {won, BestPlay}
	end.

%% Unit tests
get_best_play_test() ->
	?assertEqual(get_best_play({player1, rock}, {player2, paper}), {player2, paper}),
	?assertEqual(get_best_play({player1, scissors}, {player2, paper}), {player1, scissors}),
	?assertEqual(get_best_play({player1, rock}, {player2, scissors}), {player1, rock}),
       	?assertEqual(get_best_play({player1, rock}, {player2, rock}), undefined),
	?assertEqual(get_best_play({player1, paper}, {player2, paper}), undefined),
	?assertEqual(get_best_play({player1, scissors}, {player2, scissors}), undefined).	

check_result_between_different_plays_test() ->
	Play1 = create_play(player1, paper),
	Play2 = create_play(player2, rock),
	Result = check_result(Play1, Play2),
	?assertEqual(Result, {won, Play1}).

check_result_between_equal_plays_test() ->
	Play1 = create_play(player1, scissors),
	Play2 = create_play(player2, scissors),
	Result = check_result(Play1, Play2),
	?assertEqual(Result, {draw, Play1, Play2}).

