% AEM: 3770, Φυσέκης Θωμάς
% AEM: 3620, Στέργιος Μουμτζής
/*
% Define the valid_schedule predicate
valid_schedule(A, B, C) :-
    % Check that no lesson appears more than once in the schedule
    setof(L, (member(L, A); member(L, B); member(L, C)), Ls),
    length(Ls, N),
    length(A, Na),
    length(B, Nb),
    length(C, Nc),
    % Check that all three weeks have the correct number of unique lessons
    N =:= Na + Nb + Nc.

% Check that each group has the right number of lessons
length(A,3),
length(B,3),
length(C,2).
% Check that the schedule satisfies the constraints
valid_schedule(A, B, C).
*/

%oad the attends.pl file
:- consult('attends.pl').

schedule(A, B, C) :-
    % Get the list of all lessons from attends.pl without duplicates
    findall(Lesson, attends(_, Lesson), Lessons),
    list_to_set(Lessons, UniqueLessons),
    % Try all possible schedules using the unique lessons
    schedule_(UniqueLessons, A, B, C).

% Define the schedule_ predicate for backtracking
schedule_([], [], [], []).
schedule_([L|Ls], A, B, C) :-
    % Try adding the lesson to the first week
    schedule_(Ls, A1, B, C),
    length(A1, Na),
    Na < 3,
    \+ member(L, A1),
    A = [L|A1].
schedule_([L|Ls], A, B, C) :-
    % Try adding the lesson to the second week
    schedule_(Ls, A, B1, C),
    length(B1, Nb),
    Nb < 3,
    \+ member(L, B1),
    B = [L|B1].
schedule_([L|Ls], A, B, C) :-
    % Try adding the lesson to the third week
    schedule_(Ls, A, B, C1),
    length(C1, Nc),
    Nc < 2,
    \+ member(L, C1),
    C = [L|C1].



% Define the schedule_errors predicate
schedule_errors(A, B, C, E) :-
    findall(Students, attends(Students,A), StudentsList),
    write(StudentsList).%not gonna work,the A has 3 elements not 1
    %count_dissatisfied_students(A, B, C, E).

% Define the count_dissatisfied_students predicate
count_dissatisfied_students(A, B, C, E) :-
    findall(S, (attends(S, L), (member(L, A); member(L, B); member(L, C))), Students),
    findall(Student, (member(Student, Students), count_occurrences(Student, Students, Occurrences))).
