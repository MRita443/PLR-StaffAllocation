% Staff Allocation Solver - Main Entry Point
% To run: ?- [main], main.

:- use_module(solver).
:- use_module(utils).

main :-
    writeln('--- Finding Optimal Staff Allocation ---'),
    (   allocate_staff(AllocationMatrix, ObjectiveValue)
    ->  format('~nSolution found!~n~n', []),
        format('Objective Value: ~w~n~n', [ObjectiveValue]),
        writeln('Allocation Matrix (Staff x Activities):'),
        pretty_print(AllocationMatrix)
    ;   writeln('~nNo solution found')
    ).
