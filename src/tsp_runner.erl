-module(tsp_runner).
-behaviour(gen_fsm).

%% API.
-export([start_link/0, set_problem/1, set_timeout/1, set_solver/1,
         run/0, stop/0, best/0]).
-export([looper/3]).


%% gen_fsm.
-export([init/1]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([setup/2, setup/3]).
-export([running/2, running/3]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {
          problem,
          timeout,
          timeout_ref,
          solver,
          solver_pid
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec set_problem(Problem :: tsp_problem:tsp_problem()) -> ok.
set_problem(Problem) ->
    gen_fsm:send_event(?MODULE, {set_problem, Problem}).

-spec set_timeout(Timeout :: non_neg_integer()) -> ok.
set_timeout(Timeout) ->
    gen_fsm:send_event(?MODULE, {set_timeout, Timeout}).

-spec set_solver(Solver :: atom()) -> ok.
set_solver(Solver) ->
    gen_fsm:send_event(?MODULE, {set_solver, Solver}).

-spec run() -> ok | {error, Reason :: atom()}.
run() ->
    gen_fsm:sync_send_event(?MODULE, run).

-spec stop() -> {ok, State :: term()}.
stop() ->
    gen_fsm:sync_send_event(?MODULE, stop).

-spec best() -> {ok, Best :: number()}.
best() ->
    gen_fsm:sync_send_event(?MODULE, best).

%% gen_fsm.
init([]) ->
    {ok, setup, #state{}}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ignored, StateName, StateData}.


setup({set_problem, Problem}, StateData) ->
    {next_state, setup, StateData#state{problem = Problem}};
setup({set_timeout, Timeout}, StateData) ->
    {next_state, setup, StateData#state{timeout = Timeout}};
setup({set_solver, Solver}, StateData) ->
    {next_state, setup, StateData#state{solver = Solver}};
setup(_Event, StateData) ->
    {next_state, setup, StateData}.

setup(run, _From, StateData) ->
    try verify_setup(StateData) of
        ok -> 
            {reply, ok, running, handle_run(StateData)}
    catch
        throw:Other ->
            {reply, {error, Other}, setup, StateData}
    end;
setup(_Event, _From, StateData) ->
    {reply, ignored, setup, StateData}.

running(_Event, StateData) ->
    {next_state, running, StateData}.

running(stop, _From, StateData) ->
    {ok, Result, StateData1} = handle_stop(StateData),
    {reply, {ok, Result}, setup, StateData1};
running(best, _From, StateData) ->
    {ok, Best} = handle_best(StateData),
    {reply, {ok, Best}, running, StateData};
running(_Event, _From, StateData) ->
    {reply, ignored, running, StateData}.


handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% helpers
verify_setup(#state{problem=Problem,
                    timeout=Timeout,
                    solver=Solver}) ->
    ok = validate_problem(Problem),
    ok = validate_timeout(Timeout),
    ok = validate_solver(Solver),
    ok.

validate_problem(undefined) ->
    throw(undefined_problem);
validate_problem(_) ->
    ok.

validate_timeout(undefined) ->
    throw(undefined_timeout);
validate_timeout(Timeout) when Timeout =< 0 ->
    throw(negative_or_zero_timeout);
validate_timeout(_) ->
    ok.

validate_solver(undefined) ->
    throw(undefined_solver);
validate_solver(_) ->
    ok.

%% runner
handle_run(State = #state{
               timeout=Timeout,
               problem=Problem,
               solver=Solver
              }) ->
    {ok, State0} = Solver:init(Problem),
    SolverPid = spawn_link(?MODULE, looper, [self(), Solver, State0]),
    TimerRef = erlang:send_after(
                 Timeout,
                 SolverPid,
                 {self(), stop}
    ),
    State#state{timeout_ref=TimerRef, solver_pid=SolverPid}.

handle_stop(State = #state{solver_pid=Pid, timeout_ref=Timer}) ->
    Pid ! {self(), stop},
    erlang:cancel_timer(Timer),
    receive
        {ok, Pid, SolverState} ->
            {
                ok,
                SolverState,
                State#state{solver_pid=undefined,timeout_ref=undefined}
            }
    after
        10000 ->
            exit(stop_failed)
    end.

handle_best(#state{solver_pid=Pid}) ->
    Pid ! {self(), best},
    receive
        {ok, Pid, Best} ->
            {ok, Best}
    after
        10000 ->
            exit(best_failed)
    end.

looper(Runner, Solver, State) ->
    receive
        {Runner, best} ->
            {ok, Best} = Solver:best(State),
            Runner ! {ok, self(), Best},
            looper(Runner, Solver, State);
        {Runner, stop} ->
            Runner ! {ok, self(), State};
        _ ->
            looper(Runner, Solver, State)
    after
        0 -> 
            {ok, State1} = Solver:iterate(State),
            looper(Runner, Solver, State1)
    end.
