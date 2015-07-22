-module(tsp_runner).
-behaviour(gen_fsm).

%% API.
-export([start_link/0]).
-export([run/2, stop/0]).
-export([looper/1]).


%% gen_fsm.
-export([init/1]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([idle/2, idle/3]).
-export([running/2, running/3]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {
          solver_pid,
          log_interval_ref
}).

-define(LOG_INTERVAL, 10000).
-define(LOOPER_STOP_MSG, stop).
-define(LOOPER_LOG_MSG, log).

%% API.
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec run(Problem :: tsp_problem:tsp_problem(), Solver :: atom()) -> ok.
run(Problem, Solver) ->
    gen_fsm:sync_send_event(?MODULE, {run, Problem, Solver}).

-spec stop() -> {ok, SolverState :: term(), Iterations :: non_neg_integer()}.
stop() ->
    gen_fsm:sync_send_event(?MODULE, stop).

%% gen_fsm.
init([]) ->
    {ok, idle, #state{}}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ignored, StateName, StateData}.

idle(_Event, StateData) ->
    {next_state, idle, StateData}.

idle({run, Problem, Solver}, _From, StateData) ->
    ok = tsp_event_logger:add_handler(),
    {ok, Pid} = handle_run(Problem, Solver),
    {ok, TimerRef} = timer:send_interval(?LOG_INTERVAL, Pid, ?LOOPER_LOG_MSG),
    StateData1 = StateData#state{
                   solver_pid=Pid,
                   log_interval_ref=TimerRef
    },
    {reply, ok, running, StateData1};
idle(_Event, _From, StateData) ->
    {reply, ignored, idle, StateData}.

running(_Event, StateData) ->
    {next_state, running, StateData}.

running(stop, _From, #state{solver_pid=Pid, log_interval_ref=TRef}) ->
    {ok, SolverState, Iterations} = handle_stop(Pid),
    {ok, cancel} = timer:cancel(TRef),
    ok = tsp_event_logger:delete_handler(),
    {reply, {ok, SolverState, Iterations}, idle, #state{}};
running(_Event, _From, StateData) ->
    {reply, ignored, running, StateData}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% runner
-record(runner, {
          tsp_runner_pid,
          solver,
          solver_state,
          iteration = 0,
          start_time = erlang:system_time()
}).

handle_run(Problem, Solver) ->
    {ok, State0} = Solver:init(Problem),
    RunnerState = #runner{
                     tsp_runner_pid = self(),
                     solver = Solver,
                     solver_state = State0
    },
    ok = handle_log(init, RunnerState),
    SolverPid = spawn_link(?MODULE, looper, [RunnerState]),
    {ok, SolverPid}.

handle_stop(Pid) ->
    Pid ! ?LOOPER_STOP_MSG,
    receive
        {Pid, stopped, SolverState, Iteration} ->
            {ok, SolverState, Iteration}
    after
        5000 ->
            exit(handle_stop_failed)
    end.

looper(#runner{} = RunnerState) ->
    Msg = looper_get_message(),
    looper_handle_message(Msg, RunnerState).

looper_get_message() ->
    receive
        ?LOOPER_STOP_MSG ->
            stop;
        ?LOOPER_LOG_MSG ->
            log;
        _ ->
            ok
    after
        0 ->
            ok
    end.

looper_handle_message(
        stop,
        RunnerState = #runner{tsp_runner_pid=Runner,
                              solver_state=State,
                              iteration=Iteration}) ->
    ok = handle_log(stop, RunnerState),
    Runner ! {self(), stopped, State, Iteration},
    ok;
looper_handle_message(log, #runner{} = RunnerState) ->
    ok = handle_log(timer, RunnerState),
    looper_handle_message(ok, RunnerState);
looper_handle_message(
        ok,
        RunnerState = #runner{solver=Solver,
                              solver_state=State}) ->
    looper_handle_iteration(Solver:iterate(State), RunnerState).

looper_handle_iteration({ok, NewState}, RunnerState = #runner{}) ->
    Iteration1 = RunnerState#runner.iteration + 1,
    looper(RunnerState#runner{solver_state=NewState, iteration=Iteration1}).

handle_log(
        Mark,
        #runner{solver=Solver,
                solver_state=SolverState,
                iteration=Iteration,
                start_time=StartTime}) ->
    Runtime = erlang:system_time() - StartTime,
    {ok, {Length, Solution}} = Solver:best(SolverState),
    tsp_event:mark(Runtime, Iteration, Length, Solution, Mark).
