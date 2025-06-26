/** <module> Problem setup and main solver entry point. */
:- module(solver, [allocate_staff/3]).

:- use_module(library(clpfd)).
:- use_module(library(lists), [append/2, maplist/2, same_length/2]).

:- use_module(data_utils).
:- use_module(constraints).
:- use_module(optimization).

%!  allocate_staff(+LabelingOptions, -AllocationMatrix, -ObjectiveValue)
%
%   Finds an optimal staff allocation.
allocate_staff(LabelingOptions, AllocationMatrix, ObjectiveValue) :-
    get_activity_ids(ActivityIDs),
    get_staff_ids(StaffIDs),

    length(StaffIDs, NumStaffMembers),
    length(AllocationMatrix, NumStaffMembers),
    maplist(same_length(ActivityIDs), AllocationMatrix),

    append(AllocationMatrix, AllocationVars),
    domain(AllocationVars, 0, 1),

    apply_hard_constraints(AllocationMatrix, StaffIDs, ActivityIDs),

    find_optimal_solution(LabelingOptions, AllocationMatrix, StaffIDs, ActivityIDs, ObjectiveValue).
