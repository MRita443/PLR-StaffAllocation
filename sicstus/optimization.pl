/** <module> Objective function definition and optimization. */
:- module(optimization, [find_optimal_solution/5]).

:- use_module(library(clpfd)).
:- use_module(library(lists), [append/2, transpose/2]).
:- use_module(library(random)).

:- use_module(data).
:- use_module(utils).

%!  find_optimal_solution(+LabelingOptions, +AllocationMatrix, +StaffIDs, +ActivityIDs, -ObjectiveValue)
%
%   Defines and maximizes the objective function.
find_optimal_solution(LabelingOptions, AllocationMatrix, StaffIDs, ActivityIDs, ObjectiveValue) :-
    create_utility_matrices(StaffIDs, ActivityIDs, SkillsMatrix, PreferenceMatrix),

    append(AllocationMatrix, FlatAllocations),
    append(SkillsMatrix, FlatSkillsMatrix),
    append(PreferenceMatrix, FlatPreferenceMatrix),

    scalar_product(FlatSkillsMatrix, FlatAllocations, #=, SkillsUtility),
    scalar_product(FlatPreferenceMatrix, FlatAllocations, #=, PreferenceUtility),

    calculate_total_experience_diversity(AllocationMatrix, StaffIDs, ExperienceDiversity),

    sum(FlatAllocations, #=, TotalAssignments),

    SkillsWeight      #= 7,
    PreferenceWeight  #= 10,
    ExperienceWeight  #= 5,
    AssignmentPenalty #= 1,

    ObjectiveValue #= SkillsWeight     * SkillsUtility +
                     PreferenceWeight * PreferenceUtility +
                     ExperienceWeight * ExperienceDiversity -
                     AssignmentPenalty* TotalAssignments,

    append([maximize(ObjectiveValue)], LabelingOptions, FinalLabelingOptions),
    labeling(FinalLabelingOptions, FlatAllocations).

create_utility_matrices([], _, [], []).
create_utility_matrices([StaffID | RestStaff], ActivityIDs, [SkillRow | RestSkillRows], [PrefRow | RestPrefRows]) :-
    staff(StaffID, _, StaffSkills),
    create_utility_rows(ActivityIDs, StaffID, StaffSkills, SkillRow, PrefRow),
    create_utility_matrices(RestStaff, ActivityIDs, RestSkillRows, RestPrefRows).

create_utility_rows([], _, _, [], []).
create_utility_rows([ActivityID | RestActivities], StaffID, StaffSkills, [SkillScore | RestSkills], [PrefScore | RestPrefs]) :-
    activity(ActivityID, _, _, _, RequiredSkills),
    intersection(StaffSkills, RequiredSkills, MatchedSkills),
    length(MatchedSkills, SkillScore),
    get_preference_score(StaffID, ActivityID, PrefScore),
    create_utility_rows(RestActivities, StaffID, StaffSkills, RestSkills, RestPrefs).

get_preference_score(StaffID, ActivityID, Score) :-
    preference(StaffID, ActivityID, Score), !.
get_preference_score(_, _, 3). % Default preference score.

calculate_total_experience_diversity(AllocationMatrix, StaffIDs, TotalExperienceDiversity) :-
    collect_staff_experience(StaffIDs, StaffExperience),
    transpose(AllocationMatrix, ActivityColumns),
    sum_activity_diversity(ActivityColumns, StaffExperience, TotalExperienceDiversity).

collect_staff_experience([], []).
collect_staff_experience([StaffID | RestStaff], [Experience | RestExperience]) :-
    staff(StaffID, Experience, _),
    collect_staff_experience(RestStaff, RestExperience).

sum_activity_diversity([], _, 0).
sum_activity_diversity([ActivityCol | RestCols], StaffExperience, TotalDiversity) :-
    calculate_activity_diversity(ActivityCol, StaffExperience, ActivityDiversity),
    sum_activity_diversity(RestCols, StaffExperience, RestDiversity),
    TotalDiversity #= ActivityDiversity + RestDiversity.

calculate_activity_diversity(ActivityColumn, StaffExperience, DiversityScore) :-
    BigM = 10000, % A sufficiently large number.
    build_max_experience_terms(ActivityColumn, StaffExperience, MaxTerms),
    build_min_experience_terms(ActivityColumn, StaffExperience, BigM, MinTerms),

    maximum(MaxExperience, MaxTerms),
    minimum(MinExperience, MinTerms),

    sum(ActivityColumn, #=, NumAssigned),
    (NumAssigned #= 0) #=> (DiversityScore #= 0),
    (NumAssigned #> 0) #=> (DiversityScore #= MaxExperience - MinExperience).

build_max_experience_terms([], [], []).
build_max_experience_terms([AllocVar | RestAlloc], [Exp | RestExp], [Term | RestTerms]) :-
    Term #= AllocVar * Exp,
    build_max_experience_terms(RestAlloc, RestExp, RestTerms).

build_min_experience_terms([], [], _, []).
build_min_experience_terms([AllocVar | RestAlloc], [Exp | RestExp], BigM, [Term | RestTerms]) :-
    Term #= (1 - AllocVar) * BigM + Exp,
    build_min_experience_terms(RestAlloc, RestExp, BigM, RestTerms).

% selRandomValue(+Var, +Rest, +BB0, -BB1)
selRandomValue(Var, _, BB0, BB1):-
    fd_set(Var, Set), fdset_to_list(Set, List),
    random_member(Value, List),
    ( first_bound(BB0, BB1), Var #= Value ;
      later_bound(BB0, BB1), Var #\= Value ).
