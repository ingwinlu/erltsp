-module(tsp_event_logger).

-behaviour(gen_event).

-export([add_handler/0, add_handler/1, delete_handler/0]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {
          iodevice
}).

%% API
add_handler() ->
    Timestamp = erlang:integer_to_list(os:system_time()),
    Filename = "erltsp_" ++ Timestamp,
    add_handler(Filename).

add_handler(Filename) ->
    tsp_event:add_handler(?MODULE, [Filename]).

delete_handler() ->
    tsp_event:delete_handler(?MODULE, []).


%% Callbacks
init([Filename]) ->
    LogFile = log_file(Filename),
    {ok, IoDevice} = file:open(LogFile, [raw, write]),
    ok = write_header(IoDevice),
    {ok, #state{iodevice=IoDevice}}.

handle_event(
        Msg = {mark, _, _, _, _, _},
        State = #state{iodevice=IoDevice}) ->
    ok = write_line(IoDevice, Msg),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, #state{iodevice=IoDevice}) ->
    ok = file:close(IoDevice),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% helper
log_file(Filename) ->
    {ok, [[HomeDir]]} = init:get_argument(home),
    HomeDir ++ "/" ++ Filename ++ ".csv".

write_header(IoDevice) ->
    Header = "Timestamp,Iteration,Length,Solution,Mark" ++ line_break(),
    ok = file:write(IoDevice, Header),
    ok.

write_line(IoDevice, Msg) ->
    Line = format(Msg),
    ok = file:write(IoDevice, Line),
    ok.


format({mark, Runtime, Iteration, Length, Solution, Mark}) ->
    format_runtime(Runtime)
    ++ delimiter()
    ++ format_iteration(Iteration)
    ++ delimiter()
    ++ format_length(Length)
    ++ delimiter()
    ++ format_Solution(Solution)
    ++ delimiter()
    ++ format_mark(Mark)
    ++ line_break().


delimiter() -> ",".
line_break() -> "\n".

format_runtime(Runtime) ->
    erlang:integer_to_list(Runtime).

format_iteration(Iteration) ->
    erlang:integer_to_list(Iteration).

format_length(Length) ->
    erlang:float_to_list(Length).

format_Solution(Solution) ->
    SolutionPrefixed = lists:flatten(
                         ["-" ++ erlang:integer_to_list(X)
                            || X <- Solution
                         ]
    ),
    [_Prefix | Result] = SolutionPrefixed,
    Result.

format_mark(Mark) ->
    erlang:atom_to_list(Mark).
