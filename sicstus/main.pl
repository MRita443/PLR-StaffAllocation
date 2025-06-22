% Staff Allocation Solver - Main Entry Point
% To run: ?- [main], main.

:- use_module(library(clpfd)).

:- use_module(solver).
:- use_module(utils).

main(LabelingOptions) :-
    % Reset statistics
    statistics(runtime, [_,_]),
    fd_statistics(resumptions, Resumptions1),
    fd_statistics(entailments, Entailments1),
    fd_statistics(prunings, Prunings1),
    fd_statistics(backtracks, Backtracks1),
    fd_statistics(constraints, Constraints1),
    
    (   allocate_staff(LabelingOptions, _, ObjectiveValue)
    ->  % Get final statistics
        statistics(runtime, [_,Time]),
        fd_statistics(resumptions, Resumptions2),
        fd_statistics(entailments, Entailments2),
        fd_statistics(prunings, Prunings2),
        fd_statistics(backtracks, Backtracks2),
        fd_statistics(constraints, Constraints2),
        
        % Calculate differences
        TotalResumptions is Resumptions2 - Resumptions1,
        TotalEntailments is Entailments2 - Entailments1,
        TotalPrunings is Prunings2 - Prunings1,
        TotalBacktracks is Backtracks2 - Backtracks1,
        TotalConstraints is Constraints2 - Constraints1,
        
        format('RESULT:~w,~w,~w,~w,~w,~w,~w,~w~n', [
            LabelingOptions, 
            ObjectiveValue, 
            Time, 
            TotalResumptions, 
            TotalEntailments, 
            TotalPrunings, 
            TotalBacktracks, 
            TotalConstraints
        ])
    ;   format('RESULT:~w,FAIL,0,0,0,0,0,0~n', [LabelingOptions])
    ).
