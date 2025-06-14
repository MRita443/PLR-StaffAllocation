:- module(data, [staff/3, department/2, activity/5, availability/2]).

% staff(ID, Name, DepartmentID) 
staff(1, 'Alice', 1).
staff(2, 'Bob', 1).
staff(3, 'Charlie', 2).

% department(ID, Name)
department(1, 'HR').
department(2, 'Logistics').

% activity(ID, MinStaff, StartTime, Duration, DepartmentID)
activity(a, 1, 10, 1, 1).
activity(b, 2, 11, 2, 2).
activity(c, 1, 15, 2, 2).

% availability(StaffID, ListOfAvailableActivities)
availability(1, [a, b]).
availability(2, [a, b]).
availability(3, [a, b, c]).
