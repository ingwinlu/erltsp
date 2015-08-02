-module(erltsp_solver).

-export([start_link/2, stop/1, best/1, init/3]).
-export([system_continue/3, system_terminate/4, system_code_change/4]).

%callbacks
-callback init(Problem :: erltsp_problem:problem()) -> {ok, State0 :: term()}.
-callback iterate(State0 :: term()) -> {ok, State1 :: term()}.
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
    StartTime = erlang:system_time(),
    LogFileName = erlang:integer_to_list(StartTime) ++
        "_" ++ erlang:atom_to_list(Solver),
    ok = erltsp_event_logger:add_handler(LogFileName),

    {ok, SolverState0} = Solver:init(Problem),

    {ok, TRef} = timer:send_interval(
                   ?LOG_INTERVAL,
                   {log, timer_log}
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
    handle_msg(Msg, Parent, State).

handle_msg(iterate, Parent, State) ->
    handle_iteration(Parent, State);
handle_msg({best, From}, Parent, State) ->
    Result = handle_best(State),
    From ! {best, self(), Result},
    loop(Parent, State);
handle_msg({log, Msg}, Parent, State) ->
    ok = handle_log(Msg, State),
    loop(Parent, State);
handle_msg({system, From, Request}, Parent, State) ->
    sys:handle_system_msg(Request, From, Parent,
                          ?MODULE, [], {state, State});
handle_msg({'EXIT', Parent, Reason}, Parent, State) ->
    ok = handle_exit(State),
    exit(Reason);
handle_msg(Msg, Parent, State) ->
    error_logger:info_msg("unexpected msg ~p in ~p, ignoring~n",
                          [Msg, ?MODULE]
    ),
    loop(Parent, State).

system_continue(Parent, _, {state, State}) ->
    loop(Parent, State).

system_terminate(Reason, _, _, {state, State}) ->
    ok = handle_exit(State),
    exit(Reason).

system_code_change(Misc, _, _ ,_) ->
    {ok, Misc}.


handle_iteration(Parent, State = #state{
                            solver = Solver,
                            solver_state = SolverState,
                            iteration = Iteration}) ->
    {ok, SolverState1} = Solver:iterate(SolverState),
    State1 = State#state{solver_state = SolverState1,
                         iteration = Iteration + 1},
    loop(Parent, State1).

handle_best(#state{solver = Solver,
                   solver_state = SolverState}) ->
    Solver:best(SolverState).

handle_exit(State = #state{timer_log_ref=TRef}) ->
    {ok, cancel} = timer:cancel(TRef),
    ok = handle_log(stop, State),
    ok = erltsp_event_logger:delete_handler(),
    ok.

handle_log(
        Mark,
        #state{solver = Solver
               , solver_state = SolverState
               , solver_start_time = StartTime
               , iteration = Iteration}) ->
    Runtime = erlang:system_time() - StartTime,
    {ok, {Length, Solution}} = Solver:best(SolverState),
    ok = erltsp_event:mark(Runtime, Iteration, Length, Solution, Mark),
    ok.
