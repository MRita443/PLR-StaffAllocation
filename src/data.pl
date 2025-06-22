/** <module> Data for the Staff Scheduling problem */
:- module(data, [
    staff/4,
    activity/5,
    preference/3,
    available/2
]).

% staff(ID, Name, ExperienceLevel, Skills)
staff(1, 'Alice', 1, [teaching, logistics]).
staff(2, 'Bob', 3, [programming, teaching]).
staff(3, 'Charlie', 5, [ci_cd, devops]).

% activity(ID, MinStaff, StartTime, Duration, RequiredSkills)
activity(a, 1, 10, 2, [programming, teaching]).
activity(b, 2, 11, 1, [logistics]).
activity(c, 1, 15, 2, [ci_cd, devops, teaching]).

% preference(StaffID, ActivityID, PreferenceScore)
preference(1, b, 4).
preference(2, a, 5).
preference(3, c, 2).
preference(2, c, 1).

% available(StaffID, ActivityID)
available(1, a).
available(1, b).
available(2, a).
available(2, b).
available(3, a).
available(3, b).
available(3, c).

