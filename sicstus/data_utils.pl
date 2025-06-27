/** <module> Data manipulation functions */
:- module(data_utils, [
    create_availability_mask/3,
    create_utility_matrices/4,
    get_preference_score/3,
    get_experience_list/1,
    get_min_staff_list/1,
    get_staff_ids/1,
    get_activity_ids/1
]).

:- use_module(utils).
:- use_module('../data_pl/staff').
:- use_module('../data_pl/activities').
:- use_module('../data_pl/availability').

% create_availability_mask(+StaffID, +ActivityIDs, -AvailabilityMask)
create_availability_mask(_, [], []).
create_availability_mask(StaffID, [ActivityID | RestIDs], [IsAvailable | RestMask]) :-
    (   available(StaffID, ActivityID) -> IsAvailable = 1 ; IsAvailable = 0   ),
    create_availability_mask(StaffID, RestIDs, RestMask).

% create_utility_matrices(+StaffIDs, +ActivityIDs, -SkillMatrix, -PrefMatrix)
create_utility_matrices([], _, [], []).
create_utility_matrices([StaffID | RestStaff], ActivityIDs, [SkillRow | RestSkillRows], [PrefRow | RestPrefRows]) :-
    staff(StaffID, _, StaffSkills), % Get skills of curr staff member
    create_utility_rows(ActivityIDs, StaffID, StaffSkills, SkillRow, PrefRow),
    create_utility_matrices(RestStaff, ActivityIDs, RestSkillRows, RestPrefRows).

% create_utility_rows(+ActivityIDs, +StaffID, +StaffSkills, -SkillRow, -PrefRow)
create_utility_rows([], _, _, [], []).
create_utility_rows([ActivityID | RestActivities], StaffID, StaffSkills, [SkillScore | RestSkills], [PrefScore | RestPrefs]) :-
    activity(ActivityID, _, _, _, RequiredSkills), % Get skills of activity
    intersection(StaffSkills, RequiredSkills, MatchedSkills), % Get common skills between staff and activity
    length(MatchedSkills, SkillScore),

    get_preference_score(StaffID, ActivityID, PrefScore),

    create_utility_rows(RestActivities, StaffID, StaffSkills, RestSkills, RestPrefs).

% get_preference_score(+StaffID, +ActivityID, -Score)
get_preference_score(StaffID, ActivityID, Score) :-
    preference(StaffID, ActivityID, Score), !.
get_preference_score(_, _, 3). % Default preference score.

% get_experience_list(-ExperienceList)
get_experience_list(ExperienceList) :-
    findall(Experience, staff(_, Experience, _), ExperienceList).

% get_min_staff_list(-MinStaffList)
get_min_staff_list(MinStaffList) :-
    findall(MinStaff, activity(_, MinStaff, _, _, _), MinStaffList).

% get_activity_ids(-ActivityIDs)
get_activity_ids(ActivityIDs) :-
    findall(ID, activity(ID, _, _, _, _), ActivityIDs).

% get_staff_ids(-StaffIDs)
get_staff_ids(StaffIDs) :-
    findall(ID, staff(ID, _, _), StaffIDs).
