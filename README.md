# Exam-Schedule

This exercise focuses on generating an optimal exam schedule for postgraduate students taking 8 courses over a span of 2.5 weeks. The goal is to ensure that no student has more than two exams in the same week, and to minimize the number of students with such conflicts. Additionally, the schedule is evaluated based on the number of days between exams for each student.

## Problem Description

Suppose we have a predicate called attends/2, which stores facts about postgraduate students and the courses they are attending. Each student declares four courses they are taking. Here are some examples:

```
attends(476, im216).
attends(478, im216).
attends(484, im216).
attends(487, im216).
attends(491, im216).
...
attends(478, im209).
attends(480, im209).
attends(481, im209).
attends(484, im209).
attends(485, im209).
...
```

The objective is to construct an exam schedule that satisfies the following criteria:

1. The 8 courses are examined over 2.5 weeks according to the schedule:
    - 1st week: Monday, Wednesday, Friday
    - 2nd week: Monday, Wednesday, Friday
    - 3rd week: Monday, Wednesday
2. No student should be examined in more than two lessons in the same week. If it is not possible to avoid such conflicts, the goal is to minimize the number of students with more than two exams in the same week.

3. If multiple schedules have the same number of students with more than two exams in a week, the preference is given to the schedule with fewer days between exams for each student.

## Predicates

The following predicates are provided to solve the problem:

- **`schedule(A, B, C)`: Generates a random exam schedule for the three weeks. The variables A, B, and C represent the courses scheduled for each week.**

- **`schedule_errors(A, B, C, E)`: Given a schedule, calculates the number of dissatisfied students (those with more than two exams in the same week) and returns the count in the variable E.**

- **`minimal_schedule_errors(A, B, C, E)`: Generates a minimal schedule with the fewest dissatisfied students possible. If multiple schedules have the same optimal number of dissatisfied students, it backtracks to find all possible schedules.**

- **`score_schedule(A, B, C, S)`: Given a schedule, calculates the score of the program based on the number of days between exams for each student.**

- **`maximum_score_schedule(A, B, C, E, S)`: Generates a schedule with the maximum score based on the given criteria. If multiple schedules have the same maximum score, it backtracks to find all possible schedules.**
