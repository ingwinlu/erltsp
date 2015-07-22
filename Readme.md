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
(erltsp@127.0.0.1)2> erltsp:set_problem(Problem).
ok
(erltsp@127.0.0.1)3> erltsp:all_solvers().
[tsp_solver_evo_single]
(erltsp@127.0.0.1)4> erltsp:set_solver(tsp_solver_evo_single).
ok
(erltsp@127.0.0.1)5> erltsp:solver_run().
ok
(erltsp@127.0.0.1)6> erltsp:solver_best().
{ok,{62681.88506523915,
     [30,12,9,22,4,14,2,5,15,3,10,27,28,13,18,26,29,21,17,16,7,1,
           6,24,23|...]}}
(erltsp@127.0.0.1)7> erltsp:solver_stop().
{ok, {state, <snip>}}
```

## Solvers
Name                     | Description
-------------------------|-------------------------------------------
`tsp_solver_evo_single`  | Evolutionary Algorithm, Single Process
