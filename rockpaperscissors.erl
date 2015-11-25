-module(rockpaperscissors).
-compile(export_all).
-export([run/0, new/0, may_join/2, may_play/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {current_state, players=[], plays=[]}).

create_play(Player, Played) -> {Player, Played}.

create_player(PlayerId) ->
	{PlayerId}.

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

is_on_game_can_be_played(CurrentState) ->
	CurrentState =:= game_can_be_played.

is_on_game_started(CurrentState) ->
	CurrentState =:= game_started.

can_join_player(PlayerId, State = #state{current_state = CurrentState, players = Players}) ->
	(not player_exists(PlayerId, State)) andalso (not is_players_limit_reached(Players)) andalso (is_on_waiting_for_players(CurrentState) orelse is_on_need_more_players(CurrentState)).

has_player_played(PlayerId, State) ->
	Plays = state_get_plays(State),
	lists:keymember(PlayerId, 1, Plays).

player_exists(PlayerId, State) ->
	Players = state_get_players(State),
	lists:member(PlayerId, Players).

can_play(PlayerId, State = #state{current_state = CurrentState}) ->
	(not has_player_played(PlayerId, State)) andalso (is_on_game_can_be_played(CurrentState) orelse is_on_game_started(CurrentState)).

state_set_current_state(NewCurrentState, State) -> State#state{current_state = NewCurrentState}.

state_get_current_state(State) -> State#state.current_state.

state_get_plays(#state{plays = Plays}) -> Plays.

state_set_plays(Plays, State=#state{}) -> State#state{plays = Plays}.

plays_add(NewPlay, Plays) -> [NewPlay | Plays].

state_add_play(NewPlay, State) ->
	Plays = state_get_plays(State),
	UpdatedPlays = plays_add(NewPlay, Plays),
	state_set_plays(UpdatedPlays, State).

%% @todo more granularity on this.
state_update_current_state(State = #state{current_state=waiting_for_players, players=Players}) when length(Players) == 1 ->
	state_set_current_state(need_more_players, State);
state_update_current_state(State = #state{current_state=need_more_players, players=Players}) when length(Players) == 2 ->
	state_set_current_state(game_can_be_played, State);
state_update_current_state(State = #state{current_state=game_can_be_played, plays=Plays}) when length(Plays) == 1 ->
	state_set_current_state(game_started, State);
state_update_current_state(State = #state{current_state=game_started, plays=Plays}) when length(Plays) == 2 ->
	[Play1, Play2] = Plays,
	Result = check_result(Play1, Play2),
	state_set_current_state({game_over, Result}, State).

may_join(NewPlayer, State=#state{}) ->
	case can_join_player(NewPlayer, State) of
		true -> %% @todo: How about to return the events to process? like {joined, player1},{state_changed, from_state, to_state}. 
			UpdatedState = state_add_player(NewPlayer, State),
			FinalState = state_update_current_state(UpdatedState),
			{ok, FinalState};

		false -> {join_is_not_allowed, State}
	end.

run() ->
	InitialState = new(),
	io:format("Type help for the list of commands.~n"),
	run(InitialState).

parse_command(CommandLine) ->
	{help, []}.

run_command(help, _Params, State) ->
	run_help_command(),
	State.

run_help_command() ->
	io:format("Commands:~n"),
	io:format("    join <user_id>~n"),
	io:format("        Example: join player_1~n~n"),
	io:format("    play <user_id> <rock, paper or scissors>~n"),
	io:format("        Example: play player_1 paper~n~n"),  
	io:format("    new~n").

%% @doc A repl for controlling the game. 
run(State) ->
	Line = io:get_line("Rock Paper Scissor > "), 
	{Command, Args} = parse_command(Line),
	UpdatedState = run_command(Command, Args, State),
	run(UpdatedState).

may_play(PlayerId, PlayValue, State=#state{}) ->
	case can_play(PlayerId, State) of
		true -> Play = create_play(PlayerId, PlayValue),  
			UpdatedState = state_add_play(Play, State), 
			FinalState = state_update_current_state(UpdatedState),
			{ok, FinalState};

		false -> {play_is_not_allowed, State} %% This can be player_already_played, or action_not_alloed.
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
	?assertEqual(may_join(NewPlayer, UpdatedState), {join_is_not_allowed, UpdatedState}).

