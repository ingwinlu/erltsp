-module(erltsp_event_console).

-behaviour(gen_event).

-export([add_handler/0, delete_handler/0]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {
    timer_counter = 0
}).

%% API
add_handler() ->
    erltsp_event:add_handler(?MODULE, []).

delete_handler() ->
    erltsp_event:delete_handler(?MODULE, []).


%% Callbacks
init([]) ->
    {ok, #state{}}.

handle_event(Mark = {timer, _, _, _, _}, State = #state{timer_counter=5}) ->
    log(Mark),
    State1 = State#state{timer_counter=0},
    {ok, State1};
handle_event({timer, _, _, _, _}, State = #state{timer_counter=Counter}) ->
    Counter1 = Counter + 1,
    State1 = State#state{timer_counter = Counter1},
    {ok, State1};
handle_event(Mark = {_, _, _, _, _}, State) ->
    log(Mark),
    {ok, State};
handle_event(_Msg, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

log(Mark) ->
    error_logger:info_msg("~s", [mark_to_console(Mark)]).

mark_to_console({Mark, Runtime, Iteration, Length, Solution}) ->
    lists:flatten(
      io_lib:format("Mark: ~p, Runtime(ms): ~p, Iteration: ~p~nSolution: ~p, ~p~n", [Mark, Runtime, Iteration, Length, Solution])
    ).
