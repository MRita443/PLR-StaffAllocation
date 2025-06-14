:- module(optimization, [optimize_allocation/1]).

:- use_module(library(clpfd)).
:- use_module(library(lists)).

:- use_module(data).

% optimize_allocation(+Allocations)
optimize_allocation(Allocations) :-
    findall(Dept, staff(_, _, Dept), StaffDepts),
    findall(Dept, activity(_, _, _, _, Dept), ActivityDepts),

    % Calculate objective components
    department_score(Allocations, StaffDepts, ActivityDepts, U1),

    % Weights for soft constraints
    % TODO: Adjust based on priority
    Alpha1 #= 3,
    % Alpha2 #= 2,
    % Alpha3 #= 1,

    Objective #= Alpha1 * U1,

    % Flatten allocation matrix for labeling
    append(Allocations, Vars),
    
    % Search for optimal solution
    labeling([maximize(Objective), ff], Vars).

% department_score(+Allocations, +StaffDepts, +ActivityDepts, -Score)
department_score([], [], _, 0).
department_score([StaffRow | Allocations], [StaffDept | StaffDepts], ActivityDepts, Score) :-
    department_score(StaffRow, StaffDept, ActivityDepts, 0, S1),
    department_score(Allocations, StaffDepts, ActivityDepts, S2),
    Score #= S1 + S2.

% department_score(+StaffRow, +StaffDept, +ActivityDepts, +Acc, -Score)
department_score([], _, [], Acc, Acc).
department_score([Var | Vars], StaffDept, [ActivityDept | ActivityDepts], Acc, Score) :-
    NewAcc #= Acc + Var * (StaffDept #= ActivityDept),
    department_score(Vars, StaffDept, ActivityDepts, NewAcc, Score).
