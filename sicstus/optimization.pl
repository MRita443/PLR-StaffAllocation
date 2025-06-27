/** <module> Objective function definition and optimization. */
:- module(optimization, [find_optimal_solution/5]).

:- use_module(library(clpfd)).
:- use_module(library(lists), [append/2, transpose/2]).
:- use_module(library(random)).

:- use_module(data_utils).
:- use_module(utils).

%!  find_optimal_solution(+LabelingOptions, +AllocationMatrix, +StaffIDs, +ActivityIDs, -ObjectiveValue)
%
%   Defines and maximizes the objective function.
find_optimal_solution(LabelingOptions, AllocationMatrix, StaffIDs, ActivityIDs, ObjectiveValue) :-
    create_utility_matrices(StaffIDs, ActivityIDs, SkillsMatrix, PreferenceMatrix),

    append(AllocationMatrix, FlatAllocations),
    append(SkillsMatrix, FlatSkillsMatrix),
    append(PreferenceMatrix, FlatPreferenceMatrix),

    % Calculate skills and preference utilities
    scalar_product(FlatSkillsMatrix, FlatAllocations, #=, SkillsUtility),
    scalar_product(FlatPreferenceMatrix, FlatAllocations, #=, PreferenceUtility),

    % Calculate experience diversity
    calculate_total_experience_diversity(AllocationMatrix, ExperienceDiversity),

    % Calculate total assignments
    sum(FlatAllocations, #=, TotalAssignments),

    SkillsWeight      #= 7,
    PreferenceWeight  #= 10,
    ExperienceWeight  #= 5,
    AssignmentPenalty #= 1,

    ObjectiveValue #= SkillsWeight     * SkillsUtility +
                     PreferenceWeight  * PreferenceUtility +
                     ExperienceWeight  * ExperienceDiversity -
                     AssignmentPenalty * TotalAssignments,

    append([maximize(ObjectiveValue)], LabelingOptions, FinalLabelingOptions),
    labeling(FinalLabelingOptions, FlatAllocations).

% calculate_total_experience_diversity(+AllocationMatrix, -TotalExperienceDiversity)
calculate_total_experience_diversity(AllocationMatrix, TotalExperienceDiversity) :-
    get_experience_list(StaffExperience), % Get all staff experience
    transpose(AllocationMatrix, ActivityColumns), % Get Activity x Staff matrix
    sum_activity_diversity(ActivityColumns, StaffExperience, TotalExperienceDiversity).

% sum_activity_diversity(+ActivityColumns, +StaffExperience, -TotalDiversity)
sum_activity_diversity([], _, 0).
sum_activity_diversity([ActivityCol | RestCols], StaffExperience, TotalDiversity) :-
    calculate_activity_diversity(ActivityCol, StaffExperience, ActivityDiversity),
    sum_activity_diversity(RestCols, StaffExperience, RestDiversity),
    TotalDiversity #= ActivityDiversity + RestDiversity.

% calculate_activity_diversity(+ActivityColumn, +StaffExperience, -DiversityScore)
calculate_activity_diversity(ActivityColumn, StaffExperience, DiversityScore) :-
    build_experience_terms(ActivityColumn, StaffExperience, ExperienceTerms),

    maximum(MaxExperience, ExperienceTerms),
    minimum(MinExperience, ExperienceTerms),

    sum(ActivityColumn, #=, NumAssigned),
    (NumAssigned #= 0) #=> (DiversityScore #= 0),
    (NumAssigned #> 0) #=> (DiversityScore #= MaxExperience - MinExperience).

% build_experience_terms(+AllocationColumn, +StaffExperience, -ExperienceTerms)
build_experience_terms([], [], []).
build_experience_terms([AllocVar | RestAlloc], [Exp | RestExp], [Exp | RestFiltered]) :-
    AllocVar #= 1,
    build_experience_terms(RestAlloc, RestExp, RestFiltered).
build_experience_terms([AllocVar | RestAlloc], [_ | RestExp], RestFiltered) :-
    AllocVar #= 0, % Filter experience terms for unassigned staff
    build_experience_terms(RestAlloc, RestExp, RestFiltered).
