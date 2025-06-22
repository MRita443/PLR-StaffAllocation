/** <module> Problem setup and main solver entry point. */
:- module(solver, [allocate_staff/2]).

:- use_module(library(clpfd)).
:- use_module(library(lists), [append/2, length/2, maplist/2, same_length/2]).

:- use_module(data).
:- use_module(constraints).
:- use_module(optimization).

%!  allocate_staff(-AllocationMatrix, -ObjectiveValue)
%
%   Finds an optimal staff allocation.
allocate_staff(AllocationMatrix, ObjectiveValue) :-
    findall(ID, staff(ID, _, _, _), StaffIDs),
    findall(ID, activity(ID, _, _, _, _), ActivityIDs),

    length(StaffIDs, NumStaffMembers),
    length(AllocationMatrix, NumStaffMembers),
    maplist(same_length(ActivityIDs), AllocationMatrix),

    append(AllocationMatrix, AllocationVars),
    domain(AllocationVars, 0, 1),

    apply_hard_constraints(AllocationMatrix, StaffIDs, ActivityIDs),

    find_optimal_solution(AllocationMatrix, StaffIDs, ActivityIDs, ObjectiveValue).
