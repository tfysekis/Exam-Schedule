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
    schedule(A,B,C),
    get_students(A, StudentsA),
    get_students(B, StudentsB),
    get_students(C, StudentsC),
    count_elements(StudentsA, CountA),
    count_elements(StudentsB, CountB),
    count_elements(StudentsC, CountC),
    E is CountA + CountB + CountC.


minimal_schedule_errors(A,B,C,E) :-
    findall(E, schedule_errors(A,B,C,E), DissatisfiedStudents),
    min_list(DissatisfiedStudents, MinError),
    schedule_errors(A,B,C,MinError),
    E is MinError.

% Helper predicate to retrieve students attending a lesson
get_students(Lessons, Students) :-
    maplist(find_students, Lessons, StudentsList),
    flatten(StudentsList, Students).

find_students(Lesson, Students) :-
    findall(Student, attends(Student, Lesson), Students).

count_elements(List, Count) :-
    foldl(update_count, List, [], Counted),
    count_extra_elements(Counted, Count).

update_count(Element, Counted, NewCounted) :-
    (   select([Element, N], Counted, Rest)
    ->  N1 is N + 1,
        NewCounted = [[Element, N1] | Rest]
    ;   NewCounted = [[Element, 1] | Counted]
    ).

count_extra_elements(Counted, ExtraCount) :-
    include(exceeds_threshold, Counted, Exceeding),
    length(Exceeding, ExtraCount).

exceeds_threshold([_, N]) :-
    N > 2.


/*
schedule_errors(A, B, C, E) :-
    get_students(A, StudentsA),
    get_students(B, StudentsB),
    get_students(C, StudentsC),
    count_elements(StudentsA,CountA),
    count_elements(StudentsB,CountB),
    count_elements(StudentsC,CountC),
    E is CountA + CountB + CountC.


% Helper predicate to retrieve students attending a lesson
get_students([], []).
get_students([Lesson|Rest], Students) :-
    %here we find all the students for the first lesson
    findall(Student,attends(Student, Lesson), LessonStudents),
    get_students(Rest,RestStudents),
    append(LessonStudents, RestStudents, Students).

count_elements(List, Count) :-
    count_elements(List, [], Count).

% Predicate to count elements in the list
count_elements([], Counted, Count) :-
    count_extra_elements(Counted, Count).  % Count the extra elements in Counted
count_elements([H|T], Counted, Count) :-
    (   member([H, N], Counted)  % Check if H is already counted
    ->  N1 is N + 1,
        select([H, N], Counted, [H, N1], NewCounted)  % Increment the count of H
    ;   NewCounted = [[H, 1]|Counted]  % Add H to the Counted list with count 1
    ),
    count_elements(T, NewCounted, Count).  % Recursively process the tail of the list

% Predicate to count the extra elements that appear more than two times
count_extra_elements(Counted, ExtraCount) :-
    count_extra_elements(Counted, 0, ExtraCount).

count_extra_elements([], ExtraCount, ExtraCount).  % Base case: no more elements to process
count_extra_elements([[_, N]|T], Acc, ExtraCount) :-
    (   N > 2  % Check if the count is more than two
    ->  Acc1 is Acc + 1  % Increment the count of extra elements
    ;   Acc1 is Acc
    ),
    count_extra_elements(T, Acc1, ExtraCount).  % Recursively process the remaining elements
*/