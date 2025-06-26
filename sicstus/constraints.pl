/** <module> Hard constraints for the staff scheduling problem. */
:- module(constraints, [apply_hard_constraints/3]).

:- use_module(library(clpfd)).
:- use_module(library(lists), [transpose/2]).

:- use_module(data_utils).
:- use_module('../data_pl/activities').


%!  apply_hard_constraints(+AllocationMatrix, +StaffIDs, +ActivityIDs)
%
%   Applies all mandatory constraints to the allocation matrix.
apply_hard_constraints(AllocationMatrix, StaffIDs, ActivityIDs) :-
    constrain_staff_rules(AllocationMatrix, StaffIDs, ActivityIDs),
    constrain_activity_rules(AllocationMatrix).

% constrain_staff_rules(+AllocationMatrix, +StaffIDs, +ActivityIDs)
constrain_staff_rules([], [], _).
constrain_staff_rules([StaffAllocations | RestMatrix], [StaffID | RestIDs], ActivityIDs) :-
    enforce_staff_availability(StaffID, ActivityIDs, StaffAllocations),
    enforce_no_overlapping_activities(ActivityIDs, StaffAllocations),
    constrain_staff_rules(RestMatrix, RestIDs, ActivityIDs).

% enforce_staff_availability(+StaffID, +ActivityIDs, +StaffAllocations)
enforce_staff_availability(StaffID, ActivityIDs, StaffAllocations) :-
    create_availability_mask(StaffID, ActivityIDs, AvailabilityMask),
    post_availability_constraint(StaffAllocations, AvailabilityMask).

% post_availability_constraint(+AllocVars, +AvailabilityMask)
post_availability_constraint([], []).
post_availability_constraint([AllocVar | RestVars], [IsAvailable | RestMask]) :-
    AllocVar #=< IsAvailable, % Ensure allocation only if available
    post_availability_constraint(RestVars, RestMask).

% enforce_no_overlapping_activities(+ActivityIDs, +StaffAllocations)
enforce_no_overlapping_activities(ActivityIDs, StaffAllocations) :-
    create_cumulative_tasks(ActivityIDs, StaffAllocations, Tasks),
    cumulative(Tasks, [limit(1)]).

% create_cumulative_tasks(+ActivityIDs, +AllocVars, -Tasks)
create_cumulative_tasks([], [], []).
create_cumulative_tasks([ActID | RestActIDs], [AllocVar | RestVars], [Task | RestTasks]) :-
    activity(ActID, _, StartTime, Duration, _),
    EndTime #= StartTime + Duration,
    Task = task(StartTime, Duration, EndTime, AllocVar, ActID),
    create_cumulative_tasks(RestActIDs, RestVars, RestTasks).

% constrain_activity_rules(+AllocationMatrix)
constrain_activity_rules(AllocationMatrix) :-
    transpose(AllocationMatrix, ActivityColumns), % Get Activity x Staff matrix
    enforce_min_staff_per_activity(ActivityColumns).

% enforce_min_staff_per_activity(+ActivityColumns)
enforce_min_staff_per_activity(ActivityColumns) :-
    get_min_staff_list(MinRequirements),
    post_min_staff_constraints(ActivityColumns, MinRequirements).

% post_min_staff_constraints(+ActivityColumns, +MinStaffRequirements)
post_min_staff_constraints([], []).
post_min_staff_constraints([ActivityColumn | RestCols], [MinStaff | RestReqs]) :-
    sum(ActivityColumn, #>=, MinStaff), % Sum of allocations must meet minimum staff requirement
    post_min_staff_constraints(RestCols, RestReqs).
