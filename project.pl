:-use_module(library(clpfd)).
:-use_module(library(lists)).

% ############## Test Data ##############

% staff(name, experience, department)
staff(alice, 1, logistics).
staff(john, 0, logistics).
staff(jane, 1, program).

% activity(slug, min_staff, start_time, duration, department)
activity(coffee_break, 2, 10.5, 2, logistics).
activity(workshop_1, 1, 9, 2, program).

% available(staffName, activitySlug)
available(alice, coffee_break).
available(alice, workshop_1).
available(john, coffee_break).
available(jane, workshop_1).

% preference(staffName, activitySlug, preference) -> [0, 1] float values
% Ommitted preferences defaults to 0
preference(alice, coffee_break, 0.5).
preference(alice, workshop_1, 0.7).
preference(john, coffee_break, 0.1).
preference(john, workshop_1, 0.0).
preference(jane, coffee_break, 0.35).
preference(jane, workshop_1, 1.0).

% availability([staff x activity]) -> 0 unavailable, 1 available
/* availability([
    [1, 1],  % Staff 1 is available for both activities
    [1, 0],  % Staff 2 is only available for activity 1
    [0, 1]   % Staff 3 is only available for activity 2
]). */

% preferences([staff x activity]) -> [0, 1] float values for preference
% preferences([
%     [0.5, 0.7],  
%     [0.1, 0],  
%     [0.35, 1]
% ]).

% ############## Utils ##############

% get_staff_list(-StaffList)
get_staff_list(StaffList) :-
    findall(staff(Name, Exp, Dept), staff(Name, Exp, Dept), StaffList).

% get_activity_list(-ActivityList)
get_activity_list(ActivityList) :-
    findall(activity(Slug, MinStaff, Start, Dur, Dept), activity(Slug, MinStaff, Start, Dur, Dept), ActivityList).

% get_staff_num(-Num)
get_staff_num(Num) :-
    get_staff_list(StaffList),
    length(StaffList, Num).

% get_activity_num(-Num)
get_activity_num(Num) :-
    get_activity_list(ActivityList),
    length(ActivityList, Num).

% index_to_staff(?Index, ?Name)
index_to_staff(Index, Name) :-
    get_staff_list(StaffList),
    nth0(Index, StaffList, staff(Name, _, _)).

% index_to_activity(?Index, ?Slug)
index_to_activity(Index, Slug) :-
    get_activity_list(ActivityList),
    nth0(Index, ActivityList, activity(Slug, _, _, _, _)).

% ############## Input Conversion ##############
% ### Convert availability ###

% build_availability_matrix(-AvailabilityMatrix)
build_availability_matrix(AvailabilityMatrix) :-
    get_staff_list(StaffList),
    get_activity_list(ActivityList),
    build_avail_rows(ActivityList, StaffList, AvailabilityMatrix).

% build_avail_rows(+Activities, +StaffList, -AvailabilityMatrix)
build_avail_rows(_, [], []).
build_avail_rows(Activities, [Staff|Rest], [Row|Rows]) :-
    build_avail_row(Activities, Staff, Row),
    build_avail_rows(Activities, Rest, Rows).

% build_avail_row(+Activities, +Staff, -Row)
build_avail_row([], _, []).
build_avail_row([Activity|RestActivities], staff(Name, _, _), [Val|RestVals]) :-
    is_available(Name, Activity, Val),
    build_avail_row(RestActivities, staff(Name, _, _), RestVals).

% is_available(+Name, +Activity, -Val)
is_available(Name, activity(Slug, _, _, _, _), 1) :-
    available(Name, Slug), !.
is_available(_, _, 0).


% ### Convert preferences ####

% build_preferences_matrix(-PreferencesMatrix)
build_preferences_matrix(PreferencesMatrix) :-
    get_staff_list(StaffList),
    get_activity_list(ActivityList),
    build_pref_rows(ActivityList, StaffList, PreferencesMatrix).

% build_pref_rows(+Activities, +StaffList, -PreferencesMatrix)
build_pref_rows(_, [], []).
build_pref_rows(Activities, [Staff|Rest], [Row|Rows]) :-
    build_pref_row(Activities, Staff, Row),
    build_pref_rows(Activities, Rest, Rows).

% build_pref_row(+Activities, +Staff, -Row)
build_pref_row([], _, []).
build_pref_row([Activity|RestActivities], staff(Name, _, _), [Pref|RestPrefs]) :-
    get_preference(Name, Activity, Pref),
    build_pref_row(RestActivities, staff(Name, _, _), RestPrefs).

% get_preference(+Name, +Activity, -Pref)
get_preference(Name, activity(Slug, _, _, _, _), Pref) :-
    preference(Name, Slug, Pref), !.
get_preference(_, _, 0.0).


% ############## Constraints ##############

allocate(Allocation):- 
    get_activity_num(NumActivities),
    get_staff_num(NumStaff),

    length(Allocation, NumStaff),
    constraint_num_cols(Allocation, NumActivities),

    append(Allocation, FlatAllocation),
    domain(FlatAllocation, 0, 1),

    ensure_available(Allocation),

    labeling([], FlatAllocation).

% constraint_num_cols(+Matrix, +Num)
constraint_num_cols([], _).
constraint_num_cols([Row|Rest], Num) :-
    length(Row, Num),
    constraint_num_cols(Rest, Num).

% ensure_available(+Allocation)
ensure_available(Allocation) :-
    build_availability_matrix(AvailabilityMatrix),
    ensure_available_rows(Allocation, AvailabilityMatrix).

% ensure_available_rows(+Allocation, +AvailabilityMatrix)
ensure_available_rows([], []).
ensure_available_rows([AllocationRow|RestAllocations], [AvailabilityRow|RestAvailabilities]) :-
    ensure_available_row(AllocationRow, AvailabilityRow),
    ensure_available_rows(RestAllocations, RestAvailabilities).
    
% ensure_available_row(+AllocationRow, +AvailabilityRow)
ensure_available_row([], []).
ensure_available_row([Allocation|RestAllocations], [Availability|RestAvailabilities]) :-
    Allocation #=< Availability,
    ensure_available_row(RestAllocations, RestAvailabilities).

