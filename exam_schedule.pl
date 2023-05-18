% AEM: 3770, Φυσέκης Θωμάς
% AEM: 3620, Στέργιος Μουμτζής

% Load the attends.pl file
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



schedule_errors(A, B, C, E) :-
    get_students(A, StudentsA),
    print_students(StudentA).


% Helper predicate to retrieve students attending a lesson
get_students([], []).
get_students([Lesson|Rest], Students) :-
    %here we find all the students for the first lesson
    findall(Student,attends(Student, Lesson), LessonStudents),
    get_students(Rest,RestStudents),
    append(LessonStudents, RestStudents, Students).


hi(A):-
    count_elements([479,480,481,483,484,491,479,508,513,515,517],A).

count_elements([], 0).  % Base case: empty list has zero occurrences

count_elements([H | T], Count) :-
    count_elements(T, TailCount),  % Recursively count occurrences in the tail T
    count_element(H, T, ElementCount),  % Count occurrences of H in T
    Count is TailCount + ElementCount.  % Total count is sum of tail count and element count

count_element(_, [], 0).  % Base case: no more elements to count

count_element(X, [X | T], Count) :-
    count_element(X, T, TailCount),  % Recursively count remaining occurrences of X
    Count is TailCount + 1.  % Increment the count by 1

count_element(X, [H | T], Count) :-
    X \= H,  % X is different from the current head H
    count_element(X, T, Count).  % Recursively count remaining occurrences of X


print_students([]).
print_students([H|T]) :-
    writeln(H),
    print_students(T).