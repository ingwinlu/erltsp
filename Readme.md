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
(erltsp@127.0.0.1)3> {ok, Pid} = erltsp:solver_run(Problem, tsp_solver_evo_single).
{ok,<0.51.0>}
(erltsp@127.0.0.1)4> erltsp:solver_best(Pid).
{ok,{63898.67762819369,
     [29,21,17,16,7,1,6,24,23,8,11,9,22,4,14,2,5,12,25,19,20,30,
           15,3,10|...]}}
(erltsp@127.0.0.1)5> erltsp:solver_stop(Pid).
ok
```

## Solvers
Name                           | Description
-------------------------------|-------------------------------------------
`tsp_solver_evo_single`        | Evolutionary Algorithm, Single Process
`tsp_solver_bb_single_simple`  | Simple Branch&Bound, Single Process
