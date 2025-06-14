:- module(constraints, [constrain_availability/3, constrain_min_staff/2]).

:- use_module(library(clpfd)).
:- use_module(library(lists)).

:- use_module(data).
:- use_module(utils).

% constrain_availability(+Allocations, +StaffList, +Activities)
constrain_availability(Allocations, StaffList, Activities) :-
    maplist(apply_staff_constraints(Activities), StaffList, Allocations).

% apply_staff_constraints(+Activities, +StaffID, +StaffRow)
apply_staff_constraints(Activities, StaffID, StaffRow) :-
    availability(StaffID, AvailableActivities),
    maplist(validate_assignment(AvailableActivities), Activities, StaffRow),
    enforce_no_overlap(Activities, StaffRow).

% validate_assignment(+AvailableActivities, +ActivityID, +Var)
validate_assignment(AvailableActivities, ActivityID, Var) :-
    ( member(ActivityID, AvailableActivities) -> true ; Var #= 0 ).

% enforce_no_overlap(+Activities, +StaffRow)
enforce_no_overlap(Activities, StaffRow) :-
    pairs_keys_values(Pairs, Activities, StaffRow),
    ( foreach(A1-V1, Pairs), param(Pairs) do
        ( foreach(A2-V2, Pairs), param(A1-V1) do 
            A1 @< A2, activities_overlap(A1, A2) -> V1 + V2 #=< 1   % Prevent double assignment
            ; true
        )
    ).

% activities_overlap(+ActivityID1, +ActivityID2)
activities_overlap(A1, A2) :-
    activity(A1, _, Start1, Duration1, _),
    activity(A2, _, Start2, Duration2, _),
    Start1 < Start2 + Duration2,
    Start2 < Start1 + Duration1.

% constrain_min_staff(+Allocations, +Activities)
constrain_min_staff(Allocations, Activities) :-
    transpose(Allocations, ActivityColumns),
    maplist(ensure_min_staff, Activities, ActivityColumns).

% ensure_min_staff(+ActivityID, +StaffVars)
ensure_min_staff(ActivityID, StaffVars) :-
    activity(ActivityID, MinRequired, _, _, _),
    sum(StaffVars, #>=, MinRequired).
