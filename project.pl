:-use_module(library(clpfd)).
:-use_module(library(lists)).

% ############## Test Data ##############

% staff(+name, +experience, +skills)
% experience is an integer value [1, 5], skills is a list of skills
staff(alice, 4, [logistics]).
staff(john, 2, [logistics]).
staff(jane, 3, [program, cocktails]).

% activity(slug, min_staff, start_time, duration, skills)
activity(coffee_break, 2, 10.5, 2, [logistics, cocktails]).
activity(workshop_1, 1, 9, 2, [program]).

% available(staffName, activitySlug)
available(alice, coffee_break).
available(alice, workshop_1).
available(john, coffee_break).
available(jane, workshop_1).

% preference(staffName, activitySlug, preference) -> [0, 5] integer values
% Ommitted preferences defaults to 3
preference(alice, coffee_break, 3).
preference(alice, workshop_1, 4).
preference(john, coffee_break, 1).
preference(john, workshop_1, 0).
preference(jane, coffee_break, 2).
preference(jane, workshop_1, 5).

% availability([staff x activity]) -> 0 unavailable, 1 available
/* availability([
    [1, 1],  % Staff 1 is available for both activities
    [1, 0],  % Staff 2 is only available for activity 1
    [0, 1]   % Staff 3 is only available for activity 2
]). */

% preferences([staff x activity]) -> [0, 5] integer values for preference
% preferences([
%     [3, 4],  
%     [1, 0],  
%     [2, 5]
% ]).

% ############## Utils ##############

% get_staff_list(-StaffList)
get_staff_list(StaffList) :-
    findall(staff(Name, Exp, Skills), staff(Name, Exp, Skills), StaffList).

% get_activity_list(-ActivityList)
get_activity_list(ActivityList) :-
    findall(activity(Slug, MinStaff, Start, Dur, Skills), activity(Slug, MinStaff, Start, Dur, Skills), ActivityList).

% get_staff_num(-Num)
get_staff_num(Num) :-
    get_staff_list(StaffList),
    length(StaffList, Num).

% get_activity_num(-Num)
get_activity_num(Num) :-
    get_activity_list(ActivityList),
    length(ActivityList, Num).

% index_to_staff_name(?Index, ?Name)
index_to_staff_name(Index, Name) :-
    get_staff_list(StaffList),
    nth1(Index, StaffList, staff(Name, _, _)).

% index_to_staff(?Index, ?Staff)
index_to_staff(Index, Staff) :-
    get_staff_list(StaffList),
    nth1(Index, StaffList, Staff).

% index_to_activity_slug(?Index, ?Slug)
index_to_activity_slug(Index, Slug) :-
    get_activity_list(ActivityList),
    nth1(Index, ActivityList, activity(Slug, _, _, _, _)).

% index_to_activity(?Index, ?Activity)
index_to_activity(Index, Activity) :-
    get_activity_list(ActivityList),
    nth1(Index, ActivityList, Activity).

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
get_preference(_, _, 3).

% ### Convert skills ####

% Indicates how many skills a staff member has that align with an activity
% build_skills_matrix(-SkillsMatrix)
build_skills_matrix(SkillsMatrix) :-
    get_staff_list(StaffList),
    get_activity_list(ActivityList),
    build_skills_rows(ActivityList, StaffList, SkillsMatrix).

% build_skills_rows(+ActivityList, +StaffList, -Matrix)
build_skills_rows(_, [], []).
build_skills_rows(Activities, [Staff|RestStaff], [Row|Rows]) :-
    build_skills_row(Activities, Staff, Row),
    build_skills_rows(Activities, RestStaff, Rows).

% build_skills_row(+ActivityList, +Staff, -Row)
build_skills_row([], _, []).
build_skills_row([Activity|Rest], Staff, [NumSkills|RestNums]) :-
    calc_skills_alignment(Staff, Activity, NumSkills),
    build_skills_row(Rest, Staff, RestNums).

% calc_skills_alignment(+Staff, +Activity, -SkillsAlign)
calc_skills_alignment(staff(_, _, StaffSkills), activity(_, _, _, _, ActivitySkills), SkillsAlign) :-
    intersection(StaffSkills, ActivitySkills, CommonSkills),
    length(CommonSkills, SkillsAlign).

% ############## Constraints ##############

allocate(StaffToActivity, ActivityToStaff, Utility):- 
    get_activity_num(NumActivities),
    get_staff_num(NumStaff),

    length(StaffToActivity, NumStaff),
    constraint_num_cols(StaffToActivity, NumActivities),

    append(StaffToActivity, FlatAllocation),
    domain(FlatAllocation, 0, 1),

    transpose(StaffToActivity, ActivityToStaff),

    ensure_available(StaffToActivity),

    calc_utility(StaffToActivity, ActivityToStaff, SkillsAlign, PrefAlign, ExpDiversity),

    Utility #= SkillsAlign + PrefAlign + ExpDiversity,

    labeling([maximize(Utility)], FlatAllocation),
    
    write('Skills Alignment: '), write(SkillsAlign), nl,
    write('Preferences Alignment: '), write(PrefAlign), nl,
    write('Experience Diversity: '), write(ExpDiversity), nl,
    write('Total Utility: '), write(Utility), nl.

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

% calc_utility(+StaffToActivity, +ActivityToStaff, -SkillsAlign, -PrefAlign, -ExpDiversity)
calc_utility(StaffToActivity, ActivityToStaff, SkillsAlign, PrefAlign, ExpDiversity) :-
    build_preferences_matrix(PreferencesMatrix),
    build_skills_matrix(SkillsMatrix),
    findall(Exp, staff(_, Exp, _), ExpList),
    calc_utility_staff(StaffToActivity, PreferencesMatrix, SkillsMatrix, SkillsAlign, PrefAlign),
    calc_utility_activity(ActivityToStaff, ExpList, ExpDiversity).

% calc_utility_staff(+ActivityToStaff, +PreferencesMatrix, +SkillsMatrix, -SkillsAlign, -PrefAlign)
calc_utility_staff([], [], [], 0, 0).
calc_utility_staff([AllocationRow|RestAllocations], [PreferencesRow|RestPreferences], [SkillsRow|RestSkills], SkillsAlign, PrefAlign) :-
    scalar_product(PreferencesRow, AllocationRow, #=, PrefAlignRow),
    scalar_product(SkillsRow, AllocationRow, #=, SkillsAlignRow),
    calc_utility_staff(RestAllocations, RestPreferences, RestSkills, RestSkillsAlign, RestPrefAlign),
    SkillsAlign #= SkillsAlignRow + RestSkillsAlign,
    PrefAlign #= PrefAlignRow + RestPrefAlign.

% calc_utility_activity(+ActivityToStaff, +ExperienceList, -ExpDiversity) :
calc_utility_activity([], _, 0).
calc_utility_activity([AllocationRow|RestAllocations], ExpList, ExpDiversity) :-
    multiply_lists(ExpList, AllocationRow, ExperienceRow), % Zero out entries where staff is not allocated
    maximum(MaxExp, ExperienceRow),
    max_nonzero_diff(MaxExp, ExperienceRow, ExpDiversityRow), % Calculate diversity as the difference between max and min experience
    calc_utility_activity(RestAllocations, ExpList, RestExpDiversity),
    ExpDiversity #= ExpDiversityRow + RestExpDiversity.

% ############## Utilities ##############

% maz_nonzero_diff(+X, +List, -MaxDiff)
% max diff between X and non-zero elements in List
max_nonzero_diff(_, [], 0).
max_nonzero_diff(X, [Y|Ys], MaxDiff) :-
    Diff #= abs(X - Y),
    Y #\= 0 #<=> NonZero,
    max_nonzero_diff(X, Ys, RestMaxDiff),
    Diff #> RestMaxDiff #<=> Larger,
    bool_and([NonZero, Larger], Keep),
    if_then_else(Keep, Diff, RestMaxDiff, MaxDiff).

% multiply_lists(+List1, +List2, -Result)
multiply_lists([], [], []).
multiply_lists([X|Xs], [Y|Ys], [Z|Zs]) :-
    Z #= X * Y,
    multiply_lists(Xs, Ys, Zs).

% intersection(+List1, +List2, -Intersection)
intersection([], _, []).
intersection([H|T], L, [H|R]) :-
    member(H, L), !,
    intersection(T, L, R).
intersection([H|T], L, R) :-
    \+ member(H, L), !,
    intersection(T, L, R).



