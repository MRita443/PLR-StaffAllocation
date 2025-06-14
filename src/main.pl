:- use_module(constraints).
:- use_module(data).
:- use_module(optimization).
:- use_module(solver).
:- use_module(utils).

main :- 
    allocate_staff(Allocations),

    write(Allocations).
