# erltsp

Erlang implementation of an tsp solver

## Usage

`erltsp` uses a fsm to conveniently run different solvers on a Problem.
All Functions that are supposed to be run by a user in a shell are broken out into the `erltsp` module itself.
An Example:

`make run`
```
(erltsp@127.0.0.1)1> Problem = erltsp:load_problem("/home/winlu/gitrepos/erltsp/test/erltsp_SUITE_data/n30_ts225.4.tspp").
<snip>
(erltsp@127.0.0.1)2> erltsp:all_solvers().
[tsp_solver_evo_single]
(erltsp@127.0.0.1)3> erltsp:solver_run(Problem, tsp_solver_evo_single).
ok
(erltsp@127.0.0.1)4> {ok, State, Iterations} = erltsp:solver_stop().
<snip>
```

## Solvers
Name                     | Description
-------------------------|-------------------------------------------
`tsp_solver_evo_single`  | Evolutionary Algorithm, Single Process
