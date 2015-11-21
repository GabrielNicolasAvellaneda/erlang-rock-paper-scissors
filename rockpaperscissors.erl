-module(rockpaperscissors).
-export([]).

-include_lib("eunit/include/eunit.hrl").

create_play(Player, Played) -> {Player, Played}.

%get_play(Play) -> element(2, Play). 

get_best_play(Play1 = {_, paper}, _Play2 = {_, rock}) -> Play1;
get_best_play(_Play1 = {_, rock}, Play2 = {_, paper}) -> Play2.

check_result(Play1, Play2) ->
	{BestPlayPlayer, BestPlayValue} = get_best_play(Play1, Play2),
	{won, BestPlayPlayer, BestPlayValue}.		

%% Unit tests
check_result_between_plays_test() ->
	Play1 = create_play(player1, paper),
	Play2 = create_play(player2, rock),
	Result = check_result(Play1, Play2),
	?assertEqual(Result, {won, player1, paper}).
