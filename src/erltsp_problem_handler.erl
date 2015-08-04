-module(erltsp_problem_handler).
-behaviour(gen_server).

%% API.
-export([start_link/0, list/0, problem/1, dir/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec list() -> {ok, List :: erltsp_problem:problem()}.
list() ->
    gen_server:call(?MODULE, list).

-spec problem(N::non_neg_integer()) -> erltsp_problem:problem().
problem(N) ->
    gen_server:call(?MODULE, {problem, N}).

-spec dir() -> filelib:file().
dir() ->
    gen_server:call(?MODULE, dir).

%% gen_server.

init([]) ->
    {ok, #state{}}.

handle_call(list, _From, State) ->
    {ok, ProblemFiles} = handle_list_problems(),
    {reply, {ok, ProblemFiles}, State};
handle_call({problem, N}, _From, State) ->
    Return = handle_problem(N),
    {reply, Return, State};
handle_call(dir, _From, State) ->
    {reply, problem_dir(), State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_list_problems() ->
    Wildcard = filename:join(problem_dir(), "*.tspp"),
    ProblemFiles = filelib:wildcard(Wildcard),
    {ok, ProblemFiles}.

priv_dir() ->
    Ebin = filename:dirname(code:which(?MODULE)),
    Root = filename:dirname(Ebin),
    filename:join(Root, "priv").

problem_dir() ->
    filename:join(priv_dir(), "problems").

handle_problem(N) ->
    {ok, ProblemFiles} = handle_list_problems(),
    File = lists:nth(N, ProblemFiles),
    erltsp_problem:from_file(File).
