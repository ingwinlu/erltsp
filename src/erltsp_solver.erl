-module(erltsp_solver).

-export([start_link/2, stop/1, best/1, init/3]).
-export([system_continue/3, system_terminate/4, system_code_change/4]).

%callbacks
-callback init(Problem :: erltsp_problem:problem()) -> {ok, State0 :: term()}.
-callback iterate(State0 :: term()) -> {ok, State1 :: term()} |
                                       {stop, FinalState :: term()}.
-callback best(State :: term()) -> {ok, Best :: term()}.

-record(state, {
          problem,
          solver,
          solver_state,
          solver_start_time,
          timer_log_ref,
          iteration
}).

-define(LOG_INTERVAL, 500).
-define(SYNC_TIMEOUT, 5000).

% api
-spec start_link(Problem :: erltsp_problem:problem(),
                 Solver :: module()) ->
    {ok, Pid :: pid()}.
start_link(Problem, Solver) ->
    proc_lib:start_link(?MODULE, init, [self(), Problem, Solver]).

-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    proc_lib:stop(Pid).

best(Target) ->
    Target ! {best, self()},
    receive
        {best, Target, Result} ->
            Result
    after
        ?SYNC_TIMEOUT ->
            {error, {timeout, best, Target}}
    end.

% internal
init(Parent, Problem, Solver) ->
    StartTime = get_time(),
    LogFileName = erlang:integer_to_list(StartTime) ++
        "_" ++ erlang:atom_to_list(Solver),
    ok = erltsp_event_logger:add_handler(LogFileName),

    {ok, SolverState0} = Solver:init(Problem),

    {ok, TRef} = timer:send_interval(
                   ?LOG_INTERVAL,
                   {log, timer}
    ),

    State = #state{
        problem = Problem,
        solver = Solver,
        solver_state = SolverState0,
        solver_start_time = StartTime,
        timer_log_ref = TRef,
        iteration = 0
    },
    ok = handle_log(init, State),
    ok = proc_lib:init_ack(Parent, {ok, self()}),
    loop(Parent, State).

loop(Parent, State) ->
    Msg = receive
              Input ->
                  Input
          after
              0 ->
                  iterate
          end,
    decode_msg(Msg, Parent, State).

decode_msg(Msg, Parent, State) ->
    case Msg of
        {system, From, Req} ->
            sys:handle_system_msg(Req, From, Parent, ?MODULE,
                                  [], {state, State});
        {'EXIT', Parent, Reason} ->
            handle_exit(Reason, Msg, State);
        _ ->
            handle_msg(Msg, Parent, State)
    end.

handle_msg(Msg, Parent, State) ->
    Result = try_handle_msg(Msg, Parent, State),
    case Result of
        {'EXIT', R} -> handle_exit(R, Msg, State);
        {'EXIT', R, Stacktrace} -> handle_exit(R, Stacktrace, Msg, State);
        {stop, State1} -> handle_exit(solved, Msg, State1);
        {ok, State1} -> loop(Parent, State1)
    end.

try_handle_msg(Msg, Parent, State) ->
    try
        do_handle_msg(Msg, Parent, State)
    catch
        throw:R ->
            {'EXIT', R};
        _:R ->
            Stacktrace = erlang:get_stacktrace(),
            {'EXIT', R, {R, Stacktrace}}
    end.

do_handle_msg(iterate, _Parent, State) ->
    handle_iteration(State);
do_handle_msg({best, From}, _Parent, State) ->
    Result = handle_best(State),
    From ! {best, self(), Result},
    {ok, State};
do_handle_msg({log, Msg}, _Parent, State) ->
    ok = handle_log(Msg, State),
    {ok, State};
do_handle_msg(Msg, _Parent, State) ->
    error_logger:info_msg("unexpected msg ~p in ~p, ignoring~n",
                          [Msg, ?MODULE]
    ),
    {ok, State}.

system_continue(Parent, _, {state, State}) ->
    loop(Parent, State).

-spec system_terminate(_, _, _, _) -> no_return().
system_terminate(Reason, _Parent, _Debug, {state, State}) ->
    handle_exit(Reason, [], State).


system_code_change(Misc, _, _ ,_) ->
    {ok, Misc}.

handle_iteration(State = #state{
                    solver = Solver,
                    solver_state = SolverState,
                    iteration = Iteration}) ->
    {IterateReturn, SolverState1} = Solver:iterate(SolverState),
    State1 = State#state{solver_state = SolverState1,
                         iteration = Iteration + 1},
    {IterateReturn, State1}.

handle_best(#state{solver = Solver,
                   solver_state = SolverState}) ->
    Solver:best(SolverState).

-spec handle_exit(_,_,_) -> no_return().
handle_exit(Reason, Msg, State) ->
    handle_exit(Reason, Reason, Msg, State).

-spec handle_exit(_,_,_,_) -> no_return().
handle_exit(Reason, Report, Msg, State) ->
    ok = handle_log(stop, State),
    cleanup_state(State),
    case Reason of
        normal -> exit(normal);
        shutdown -> exit(shutdown);
        {shutdown, _} = Shutdown -> exit(Shutdown);
        _ ->
            error_info(?MODULE, Msg, Report, State),
            exit(Reason)
    end.

cleanup_state(#state{timer_log_ref=TRef}) ->
    {ok, cancel} = timer:cancel(TRef),
    ok = erltsp_event_logger:delete_handler(),
    ok.

error_info(Name, Msg, Reason, State) ->
    error_logger:info_msg("** ~p terminating~n"
                          "** Last message in was ~p~n"
                          "** When Server state == ~p~n"
                          "** Reason for termination == ~n** ~p~n",
                          [Name, Msg, State, Reason]
    ),
    ok.

handle_log(
        Mark,
        #state{solver = Solver
               , solver_state = SolverState
               , solver_start_time = StartTime
               , iteration = Iteration}) ->
    Runtime = get_time() - StartTime,
    {ok, {Length, Solution}} = Solver:best(SolverState),
    ok = erltsp_event:Mark(Runtime, Iteration, Length, Solution),
    ok.

get_time() ->
    erlang:system_time(milli_seconds).
