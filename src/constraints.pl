/** <module> Hard constraints for the staff scheduling problem. */
:- module(constraints, [apply_hard_constraints/3]).

:- use_module(library(clpfd)).
:- use_module(library(lists), [transpose/2]).
:- use_module(data).

%!  apply_hard_constraints(+AllocationMatrix, +StaffIDs, +ActivityIDs)
%
%   Applies all mandatory constraints to the allocation matrix.
apply_hard_constraints(AllocationMatrix, StaffIDs, ActivityIDs) :-
    constrain_staff_rules(AllocationMatrix, StaffIDs, ActivityIDs),
    constrain_activity_rules(AllocationMatrix, ActivityIDs).

constrain_staff_rules([], [], _).
constrain_staff_rules([StaffAllocations | RestMatrix], [StaffID | RestIDs], ActivityIDs) :-
    enforce_staff_availability(StaffID, ActivityIDs, StaffAllocations),
    enforce_no_overlapping_activities(ActivityIDs, StaffAllocations),
    constrain_staff_rules(RestMatrix, RestIDs, ActivityIDs).

enforce_staff_availability(StaffID, ActivityIDs, StaffAllocations) :-
    create_availability_mask(StaffID, ActivityIDs, AvailabilityMask),
    post_availability_constraint(StaffAllocations, AvailabilityMask).

create_availability_mask(_, [], []).
create_availability_mask(StaffID, [ActivityID | RestIDs], [IsAvailable | RestMask]) :-
    (   available(StaffID, ActivityID) -> IsAvailable = 1 ; IsAvailable = 0   ),
    create_availability_mask(StaffID, RestIDs, RestMask).

post_availability_constraint([], []).
post_availability_constraint([AllocVar | RestVars], [IsAvailable | RestMask]) :-
    AllocVar #=< IsAvailable,
    post_availability_constraint(RestVars, RestMask).

enforce_no_overlapping_activities(ActivityIDs, StaffAllocations) :-
    create_cumulative_tasks(ActivityIDs, StaffAllocations, Tasks),
    cumulative(Tasks, [limit(1)]).

create_cumulative_tasks([], [], []).
create_cumulative_tasks([ActID | RestActIDs], [AllocVar | RestVars], [Task | RestTasks]) :-
    activity(ActID, _, StartTime, Duration, _),
    EndTime #= StartTime + Duration,
    Task = task(StartTime, Duration, EndTime, AllocVar, ActID),
    create_cumulative_tasks(RestActIDs, RestVars, RestTasks).

constrain_activity_rules(AllocationMatrix, ActivityIDs) :-
    transpose(AllocationMatrix, ActivityColumns),
    enforce_min_staff_per_activity(ActivityColumns, ActivityIDs).

enforce_min_staff_per_activity(ActivityColumns, ActivityIDs) :-
    get_activity_min_reqs(ActivityIDs, MinRequirements),
    post_min_staff_constraints(ActivityColumns, MinRequirements).

get_activity_min_reqs([], []).
get_activity_min_reqs([ActivityID | RestIDs], [MinStaff | RestReqs]) :-
    activity(ActivityID, MinStaff, _, _, _),
    get_activity_min_reqs(RestIDs, RestReqs).

post_min_staff_constraints([], []).
post_min_staff_constraints([ActivityColumn | RestCols], [MinStaff | RestReqs]) :-
    sum(ActivityColumn, #>=, MinStaff),
    post_min_staff_constraints(RestCols, RestReqs).
