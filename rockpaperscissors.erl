-module(rockpaperscissors).
-export([]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {current_state, players=[], plays=[]}).

create_play(Player, Played) -> {Player, Played}.

create_player(Id) ->
	{Id}.

%get_play(Play) -> element(2, Play). 

get_best_play(Play1 = {_, paper}, _Play2 = {_, rock}) -> Play1;
get_best_play(_Play1 = {_, rock}, Play2 = {_, paper}) -> Play2;
get_best_play(_Play1 = {_, paper}, Play2 = {_, scissors}) -> Play2;
get_best_play(Play1 = {_, scissors}, _Play2 = {_, paper}) -> Play1; 
get_best_play(Play1 = {_, rock}, _Play2 = {_, scissors}) -> Play1;
get_best_play(_Play1 = {_, scissors}, Play2 = {_, rock}) -> Play2;
get_best_play(_, _) -> undefined.

check_result(Play1, Play2) ->
	case get_best_play(Play1, Play2) of
		undefined -> {draw, Play1, Play2};
		BestPlay -> {won, BestPlay}
	end.

new() ->
	#state{current_state=waiting_for_players}.  

state_get_players(#state{players = Players}) -> Players.
state_set_players(Players, State) -> State#state{players = Players}.
player_add(NewPlayer, Players) -> [NewPlayer | Players].
state_add_player(NewPlayer, State) ->
	Players = state_get_players(State),
	UpdatedPlayers = player_add(NewPlayer, Players),
	state_set_players(UpdatedPlayers, State).

is_players_limit_reached(Players) ->
	length(Players) >= 2.

is_on_waiting_for_players(CurrentState) ->
	CurrentState =:= waiting_for_players.

is_on_need_more_players(CurrentState) ->
	CurrentState =:= need_more_players. 

can_join_player(#state{current_state = CurrentState, players = Players}) ->
	(not is_players_limit_reached(Players)) andalso (is_on_waiting_for_players(CurrentState) orelse is_on_need_more_players(CurrentState)).

state_set_current_state(NewCurrentState, State) -> State#state{current_state = NewCurrentState}.

state_get_current_state(State) -> State#state.current_state.

%% @todo more granularity on this.
state_update_current_state(State = #state{current_state=waiting_for_players, players=Players}) ->
	case length(Players) of
		1 -> state_set_current_state(need_more_players, State)
	end;
state_update_current_state(State = #state{current_state=need_more_players, players=Players}) ->
	case length(Players) of
		2 -> state_set_current_state(game_can_be_played, State)
	end.

may_join(NewPlayer, State=#state{}) ->
	case can_join_player(State) of
		true -> UpdatedState = state_add_player(NewPlayer, State),
			FinalState = state_update_current_state(UpdatedState),
			{ok, FinalState};
		false -> {error, join_is_not_allowed}
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

may_join_should_be_allowed_test() ->
	State = new(),
	Player1 = create_player(player_1),
	Player2 = create_player(player_2),
	{Status, UpdatedState} =  may_join(Player1, State),
	?assertEqual(ok, Status),
	?assertEqual(1, length(state_get_players(UpdatedState))),
	{Status, FinalState} = may_join(Player2, UpdatedState),
	?assertEqual(2, length(state_get_players(FinalState))),
	?assertEqual(game_can_be_played, state_get_current_state(FinalState)).

may_join_should_not_bet_allowed_test() ->
	State = new(),
	Players = [create_player(player_1), create_player(player_2)],
	UpdatedState = state_set_players(Players, State),
	NewPlayer = create_player(player_3),
	?assertEqual(may_join(NewPlayer, UpdatedState), {error, join_is_not_allowed}).

