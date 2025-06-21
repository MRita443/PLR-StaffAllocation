:- module(data_utils, [get_staff_list/1, get_activity_list/1, get_staff_num/1, get_activity_num/1, index_to_staff_name/2,
                        index_to_staff/2, index_to_activity_slug/2, index_to_activity/2, build_availability_matrix/1,
                        build_preferences_matrix/1, build_skills_matrix/1, build_overlap_matrix/1]).
:- use_module(data).
:- use_module(utils).
:- use_module(library(clpfd)).

% ############## Data Utils ##############

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

% ### Convert activity overlap ####

% Square matrix where (I,J) is 1 if activity I and activity J overlap, 0 otherwise
% build_overlap_matrix(-OverlapMatrix)
build_overlap_matrix(OverlapMatrix) :-
    get_activity_list(ActivityList),
    build_overlap_rows(ActivityList, ActivityList, OverlapMatrix).

% build_overlap_rows(+Activities, +AllActivities, -Rows)
build_overlap_rows([], _, []).
build_overlap_rows([A|Rest], AllActivities, [Row|Rows]) :-
    build_overlap_row(A, AllActivities, Row),
    build_overlap_rows(Rest, AllActivities, Rows).

% build_overlap_row(+Activity, +AllActivities, -Row)
build_overlap_row(_, [], []).
build_overlap_row(Activity, [Other|Rest], [Val|RestVals]) :-
    activities_overlap(Activity, Other, Val),
    build_overlap_row(Activity, Rest, RestVals).

% activities_overlap(+Activity1, +Activity2)
% True if two activities overlap in time.
activities_overlap(activity(Slug, _, _, _, _), activity(Slug, _, _, _, _), 0) :- !.
activities_overlap(activity(_, _, Start1, Dur1, _), activity(_, _, Start2, Dur2, _), Val) :-
    End1 #= Start1 + Dur1,
    End2 #= Start2 + Dur2,
    Start1 #< End2 #<=> C1,
    Start2 #< End1 #<=> C2,
    bool_and([C1, C2], Val).
