-module(erltsp_event_console).

-behaviour(gen_event).

-export([add_handler/0, delete_handler/0]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {
}).

%% API
add_handler() ->
    erltsp_event:add_handler(?MODULE, []).

delete_handler() ->
    erltsp_event:delete_handler(?MODULE, []).


%% Callbacks
init([]) ->
    {ok, #state{}}.

handle_event(Mark = {mark, _, _, _, _, _}, State) ->
    error_logger:info_msg("~s", [mark_to_console(Mark)]),
    {ok, State};
handle_event(Msg, State) ->
    error_logger:info_msg("~p~n", [Msg]),
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

mark_to_console({mark, Runtime, Iteration, Length, Solution, Mark}) ->
    lists:flatten(
      io_lib:format("Mark: ~p, Runtime(ms): ~p, Iteration: ~p~nSolution: ~p, ~p~n", [Mark, Runtime, Iteration, Length, Solution])
    ).
