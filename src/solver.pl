:- module(solver, [allocate_staff/1]).

:- use_module(library(clpfd)).
:- use_module(library(lists)).

:- use_module(data).
:- use_module(constraints).
:- use_module(optimization).

% allocate_staff(-Allocations)
allocate_staff(Allocations) :-
    % Get all staff members
    findall(ID, staff(ID, _, _), StaffList),
    length(StaffList, NumStaff),

    % Get all activities
    findall(ID, activity(ID, _, _, _, _), Activities),

    % Create allocation matrix (Staff x Activities)
    length(Allocations, NumStaff),
    maplist(same_length(Activities), Allocations),

    % Flatten the matrix for constraints
    append(Allocations, Vars),
    domain(Vars, 0, 1), % 0 = not assigned, 1 = assigned

    % Apply constraints
    constrain_availability(Allocations, StaffList, Activities),
    constrain_min_staff(Allocations, Activities),

    optimize_allocation(Allocations).
