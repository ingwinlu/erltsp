-module(tsp_runner).
-behaviour(gen_fsm).

%% API.
-export([start_link/0]).
-export([run/2, stop/0]).
-export([looper/4]).


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
          solver_pid
}).

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
    {ok, Pid} = handle_run(Problem, Solver),
    StateData1 = StateData#state{
                   solver_pid=Pid
    },
    {reply, ok, running, StateData1};
idle(_Event, _From, StateData) ->
    {reply, ignored, idle, StateData}.

running(_Event, StateData) ->
    {next_state, running, StateData}.

running(stop, _From, #state{solver_pid=Pid}) ->
    {ok, SolverState, Iterations} = handle_stop(Pid),
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
handle_run(Problem, Solver) ->
    {ok, State0} = Solver:init(Problem),
    SolverPid = spawn_link(?MODULE, looper, [self(), Solver, State0, 0]),
    {ok, SolverPid}.

handle_stop(Pid) ->
    Pid ! {self(), stop},
    receive
        {Pid, stopped, SolverState, Iteration} ->
            {ok, SolverState, Iteration}
    after
        5000 ->
            exit(handle_stop_failed)
    end.

looper(Runner, Solver, State, Iteration) ->
    Msg = looper_get_message(Runner),
    looper_handle_message(Msg, Runner, Solver, State, Iteration).

looper_get_message(Runner) ->
    receive
        {Runner, stop} ->
            stop;
        _ ->
            ok
    after
        0 ->
            ok
    end.

looper_handle_message(stop, Runner, _Solver, State, Iteration) ->
    Runner ! {self(), stopped, State, Iteration},
    ok;
looper_handle_message(ok, Runner, Solver, State, Iteration) ->
    looper_handle_iteration(
        Solver:iterate(State),
        Runner,
        Solver,
        Iteration
    ).

looper_handle_iteration({ok, NewState}, Runner, Solver, Iteration) ->
    looper(Runner, Solver, NewState, Iteration+1).
