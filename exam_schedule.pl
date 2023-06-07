% AEM: 3770, Φυσέκης Θωμάς
% AEM: 3620, Στέργιος Μουμτζής

% Load the attends.pl file
:- consult('attends.pl').
:- use_module(library(clpfd)).

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


% Calculate the number of schedule errors for given courses
schedule_errors(A, B, C, E) :-
    % Get the students attending lesson
    get_students(A, StudentsA), 
    get_students(B, StudentsB), 
    get_students(C, StudentsC), 
    % Count the number of students 
    count_elements(StudentsA, CountA), 
    count_elements(StudentsB, CountB), 
    count_elements(StudentsC, CountC), 
    E is CountA + CountB + CountC. 

% Helper predicate to retrieve students attending a lesson
get_students(Lessons, Students) :-
    % Find students for each lesson in Lessons
    maplist(find_students, Lessons, StudentsList), 
    flatten(StudentsList, Students). 

find_students(Lesson, Students) :-
     % Find all students attending the given lesson
    findall(Student, attends(Student, Lesson), Students).

count_elements(List, Count) :-
    % Count the occurrences of each element in the list
    foldl(update_count, List, [], Counted),
    % Count the elements that exceed a certain threshold
    count_extra_elements(Counted, Count). 

update_count(Element, Counted, NewCounted) :-
    (   select([Element, N], Counted, Rest) % If the element is already in the list
    ->  N1 is N + 1, % Increment the count by 1
        NewCounted = [[Element, N1] | Rest] % Update the list with the incremented count
    ;   NewCounted = [[Element, 1] | Counted] % Add the element with a count of 1 to the list
    ).

count_extra_elements(Counted, ExtraCount) :-
    % Filter the elements that exceed a certain threshold
    include(exceeds_threshold, Counted, Exceeding), 
    % Count the filtered elements
    length(Exceeding, ExtraCount). 

exceeds_threshold([_, N]) :-
    N > 2. % Predicate to check if the count exceeds the threshold (2 in this case)


minimal_schedule_errors(A, B, C, E) :-
    E = 0,
    (
        % Create schedules with the minimum dissatisfied students
        schedule(A, B, C),
        schedule_errors(A, B, C, E)
    ;
        % If there is not a program with 0 dissatisfied students,
        % then increase the number by 1 until it finds a program
        % with the next less dissatisfied students
        \+ (schedule(A, B, C), schedule_errors(A, B, C, E)),
        ENew #= E + 1,
        minimal_schedule_errors(A, B, C, ENew)
    ).
    


score_schedule(A,B,C,S) :-
%Score calculation from the students that attend only one lesson per week
    score_by_students_with_one_lesson_per_week(A,ScoreS1),
    score_by_students_with_one_lesson_per_week(B,ScoreS2),
    score_by_students_with_one_lesson_per_week(C,ScoreS3),
    Score1 is ScoreS1 + ScoreS2 + ScoreS3,

%Score calculation from the students that give a lesson on Mon-Wed
    score_week1(A,Score_W1),
    score_week1(B,Score_W2),
    score_week1(C,Score_W3),
    Score2 is Score_W1 + Score_W2 + Score_W3,

%Score calculation from the students that give a lesson on Mon-Fri
    score_week2(A, Score2_W1),
    score_week2(B, Score2_W2),
    Score3 is Score2_W1 + Score2_W2,

%Score removal calculation from the students that give 3 lessons per week
    score_removal(A,Score_removal1),
    score_removal(B,Score_removal2),
    ScoreR is Score_removal1 + Score_removal2,

    S is Score1 + Score2 + Score3 - ScoreR.

%If a student gives three lessons in a certain week then this
%week gets a score of -7 for this student
score_removal(Lessons, Score) :-
    first(Lessons, First),
    second(Lessons, Second),
    last(Lessons, Last),

    findall(Student, attends(Student, First), StudentsFirst),
    findall(Student, attends(Student, Second), StudentsSecond),
    findall(Student, attends(Student, Last), StudentsLast),

    intersection(StudentsFirst,StudentsSecond, Common),
    intersection(Common, StudentsLast, CommonFinal),

    length(CommonFinal, Count),
    Score is Count * 7.
    
%case 1: If a student is examined in the same week Monday-Wednesday, then the program takes
%score +1 for this student and for this week
score_week1(Lessons, Score) :-
    first(Lessons, First),
    second(Lessons, Second),
    findall(Student, attends(Student, First), StudentsFirst),
    findall(Student, attends(Student, Second), StudentsSecond),
    intersection(StudentsFirst, StudentsSecond, Common),
    length(Common, Count),
    Score is Count.

%case 2: if the second week falls on Monday-
%Friday then the program scores +3 for that student and for that week.
score_week2(Lessons, Score) :-
    last(Lessons, Last),
    first(Lessons, First),
    findall(Student, attends(Student, First), StudentsFirst),
    findall(Student, attends(Student, Last), StudentsLast),
    intersection(StudentsFirst, StudentsLast, Common),
    length(Common,Count),
    Score is Count * 3.

%predicates to take the first and the second item from a list
first([First | _], First).
second([_, Second | _], Second).

%case 3: If a student is examined in only one lesson in a given week then that week gets a score of +7 for
%this student.
score_by_students_with_one_lesson_per_week(Lessons, Score) :-
    get_students(Lessons, Students),
    count_single_occurences(Students, Count),
    Score is Count * 7.

%predicate that counts the students where they are tested in a lesson only once per week
count_single_occurences(Students, Count) :-
    findall(Student, (select(Student, Students, Rest), \+ memberchk(Student, Rest)), SingleOccurrences),
    length(SingleOccurrences, Count).


% Find the maximum score among all possible schedules
maximum_score_schedule(A, B, C, E, S) :-
    % Find all scores of possible schedules
    findall(Score, (
        minimal_schedule_errors(X, Y, Z, _), % Generate possible schedules
        score_schedule(X, Y, Z, Score) % Calculate score for each schedule
    ), Scores),
    % Find the maximum score from the list of scores
    max_list(Scores, S),
    % Find the minimal schedule errors for the given courses
    minimal_schedule_errors(A, B, C, E).