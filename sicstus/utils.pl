/** <module> General utility predicates. */
:- module(utils, [writeln/1, pretty_print/1, intersection/3, print_allocations_by_day/3, selRandomValue/4, multiply_lists/3, max_nonzero_diff/3]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(data_utils).
:- use_module('../data_pl/activities').

% writeln(+Term)
writeln(Term) :-
    write(Term),
    nl.

% pretty_print(+List)
pretty_print([]).
pretty_print([Row | Rows]) :-
    writeln(Row),
    pretty_print(Rows).

% intersection(+List1, +List2, -Intersection)
intersection([], _, []).
intersection([H | T], List2, [H | Intersection]) :-
    member(H, List2), !,
    intersection(T, List2, Intersection).
intersection([_ | T], List2, Intersection) :-
    intersection(T, List2, Intersection).

% multiply_lists(+List1, +List2, -Result)
multiply_lists([], [], []).
multiply_lists([X|Xs], [Y|Ys], [Z|Zs]) :-
    Z #= X * Y,
    multiply_lists(Xs, Ys, Zs).

% max_nonzero_diff(+X, +List, -MaxDiff)
% max diff between X and non-zero elements in List
max_nonzero_diff(_, [], 0).
max_nonzero_diff(X, [Y|Ys], MaxDiff) :-
    Diff #= abs(X - Y),
    Y #\= 0 #<=> NonZero,
    max_nonzero_diff(X, Ys, RestMaxDiff),
    Diff #> RestMaxDiff #<=> Larger,
    bool_and([NonZero, Larger], Keep),
    if_then_else(Keep, Diff, RestMaxDiff, MaxDiff).

% selRandomValue(+Var, +Rest, +BB0, -BB1)
selRandomValue(Var, _, BB0, BB1):-
    fd_set(Var, Set), fdset_to_list(Set, List),
    random_member(Value, List),
    ( first_bound(BB0, BB1), Var #= Value ;
      later_bound(BB0, BB1), Var #\= Value ).

% print_allocations_by_day(+StaffIDs, +ActivityIDs, +AllocationMatrix)
print_allocations_by_day(StaffIDs, ActivityIDs, AllocationMatrix) :-
    findall(day(Day, ID),
        (
            activity(ID, _, StartBlock, _, _),
            Day is (StartBlock // 96) + 1  % Calculate day from start time
        ),
        DayActivities),
    group_by_day(DayActivities, GroupedDays),
    print_days(GroupedDays, StaffIDs, ActivityIDs, AllocationMatrix).

% group_by_day(+DayActivities, -GroupedDays)
group_by_day(DayActivities, GroupedDays) :-
    sort(DayActivities, Sorted), % Sort to gather same days together
    group_by_day_(Sorted, GroupedDays).

% group_by_day_(+DayActivities, -GroupedDays)
group_by_day_([], []).
group_by_day_([day(Day, ID) | Rest], [day(Day, IDs) | GroupedRest]) :-
    gather_same_day(Day, Rest, IDsRest, Remaining),
    append([ID], IDsRest, IDs),
    group_by_day_(Remaining, GroupedRest).

% gather_same_day(+Day, +List, -SameDayIDs, -Rest)
% Extracts all activities of the same Day from List
gather_same_day(_, [], [], []).
gather_same_day(Day, [day(Day, ID) | Rest], [ID | Same], Remaining) :-
    !,
    gather_same_day(Day, Rest, Same, Remaining).
gather_same_day(Day, [Other | Rest], [], [Other | Rest]) :-
    Day \= Other.

% print_days(+GroupedDays, +StaffIDs, +FullActivityIDs, +AllocationMatrix)
print_days([], _, _, _).
print_days([day(Day, ActivityIDs) | Rest], StaffIDs, FullActivityIDs, AllocationMatrix) :-
    format("Day ~w:~n", [Day]),
    print_activities(ActivityIDs, StaffIDs, FullActivityIDs, AllocationMatrix),
    nl,
    print_days(Rest, StaffIDs, FullActivityIDs, AllocationMatrix).

% print_activities(+ActivityIDs, +StaffIDs, +FullActivityIDs, +AllocationMatrix)
print_activities([], _, _, _).
print_activities([ActivityID | Rest], StaffIDs, FullActivityIDs, AllocationMatrix) :-
    nth0(ActivityIndex, FullActivityIDs, ActivityID), % Find activity index
    findall(StaffID,
        (
            nth0(StaffIndex, StaffIDs, StaffID),
            nth0(StaffIndex, AllocationMatrix, StaffRow),
            nth0(ActivityIndex, StaffRow, 1) % Assigned staff marked as 1
        ),
        AssignedStaff),
    format("  ~w: ~w~n", [ActivityID, AssignedStaff]),
    print_activities(Rest, StaffIDs, FullActivityIDs, AllocationMatrix).

