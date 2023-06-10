implement main
    open core, file, stdio

domains
    data = date(integer, integer, integer).
    gender = male; female.

class facts - organization
    employee : (integer IdWorker, string NameWorker, gender Pol, data BData, data StartWork).
    works : (integer EmployeeId, integer PositionId, data StartWork).
    salary : (integer IdSal, integer Sal).
    department : (integer IdDep, string NameDep).
    position : (integer IdPos, string Status, string Descrip, integer DepartmentId).
    submission : (integer IDhigherDep, integer IDbelowDep).

class facts
    k : (real KolvoYear) single.
    s : (real Amount) single.

clauses
%текущее значение переменной
    k(0).
    s(0).

class predicates
    subordination_of_departments : () nondeterm.
    print_female_names : ().
    department_employee : (string Name) nondeterm.
    department_employeer_male : (string Name) nondeterm.
    employee : (integer IdSal) nondeterm.
    department_sal : (string Name) nondeterm.
    age_of_work : (integer EmployeeId) nondeterm.
    hierarchy_of_department_employees : (string Name) nondeterm.
    total_budget : (real* List) -> real Sum.
    amount_of_salaries : () -> real Sum.
    max : (real* List, real Max [out]) nondeterm.
    maximum_salary : () -> real Max determ.
    min : (real* List, real Min [out]) nondeterm.
    minimum_salary : () -> real Min determ.

class predicates  %Вспомогательные предикаты
    list : (main::organization*) nondeterm.
    employee_in_department : (string Department) -> main::organization* Employee_in_department nondeterm.
    print_list : (main::organization*) nondeterm.

clauses
    employee_in_department(Department) = Employee_in_department :-
        department(DepartmentId, Department),
        position(IdPos, _, _, DepartmentId),
        works(EmployeeId, IdPos, _),
        Employee_in_department =
            [ employee(EmployeeId, NameWorker, Pol, BData, StartWork) || employee(EmployeeId, NameWorker, Pol, BData, StartWork) ].
    list([H | T]) :-
        write(H),
        nl,
        list(T).

    department_employeer_male(Name) :-
        department(Id, Name),
        position(PositionId, _, _, Id),
        works(EmployeeId, PositionId, _),
        employee(EmployeeId, Surname, male, _, _),
        write(Surname),
        nl,
        fail.
    department_employeer_male(Name) :-
        department(_, Name).

    subordination_of_departments() :-
        submission(IDhigherDep, IDbelowDep),
        department(IDhigherDep, Name),
        department(IDbelowDep, NName),
        write(NName, " obeys ", Name, " ➦ "),
        nl,
        write(" ➥ "),
        fail.
    subordination_of_departments() :-
        department(_, _).

    age_of_work(EmployeeId) :-
        employee(EmployeeId, _, _, date(Y1, _, _), date(Y2, _, _)),
        k(KolvoYear),
        assert(k(KolvoYear + Y2 - Y1)).
    age_of_work(EmployeeId) :-
        employee(EmployeeId, Name, _, _, _),
        k(KolvoYear),
        write(Name, " - ", KolvoYear, " years", "\n").

    hierarchy_of_department_employees(Name) :-
        department(DepartmentId, Name),
        employee(IDemployee, NName, _, _, _),
        works(IDemployee, PositionId, _),
        position(PositionId, "boss", _, DepartmentId),
        write("Director 👨🏻‍💼 - ", NName),
        nl,
        fail.
    hierarchy_of_department_employees(Name) :-
        department(DepartmentId, Name),
        employee(IDemployee, NName, _, _, _),
        works(IDemployee, PositionId, _),
        position(PositionId, "employee", _, DepartmentId),
        write("Worker  - ", NName),
        nl,
        fail.
    hierarchy_of_department_employees(_) :-
        department(_, _).

    employee(IDemployee) :-
        employee(IDemployee, Name, _, HBData, StartWork),
        write("Employee - ", Name, ".", " Date of birth: ", HBData, ". Start of work: ", StartWork),
        nl,
        fail.
        %Если сотрудник с заданным идентификатором не найден
    employee(IDemployee) :-
        employee(IDemployee, _, _, _, _),
        nl.

    department_employee(Name) :-
        department(Id, Name),
        position(PositionId, _, _, Id),
        works(EmployeeId, PositionId, _),
        employee(EmployeeId, Surname, _, _, _),
        write(Surname),
        nl,
        fail.
    department_employee(Name) :-
        department(_, Name).

    department_sal(Name) :-
        department(DepartmentId, Name),
        works(EmployeeId, PositionId, _),
        position(PositionId, _, _, DepartmentId),
        salary(EmployeeId, X),
        s(Amount),
        assert(s(Amount + X)), %результат сохраняется в переменную Sum
        fail.
    department_sal(Name) :-
        department(_, Name),
        s(Amount),
        write(Amount, "💲\n").

    print_female_names() :-
        employee(_, Name, female, _, _),
        write(Name),
        nl,
        fail.
    print_female_names().

    total_budget([]) = 0.
    total_budget([Z | T]) = total_budget(T) + Z.
    amount_of_salaries() = Sum :-
        Sum = total_budget([ Budget || salary(_, Budget) ]).

    max([Max], Max). %если список состоит только из одного элемента
    max([S1, S2 | T], Max) :-
        S1 >= S2,
        max([S1 | T], Max).
    max([S1, S2 | T], Max) :-
        S1 <= S2,
        max([S2 | T], Max).

    maximum_salary() = MaxSal :-
        max([ Budget || salary(_, Budget) ], Max),
        MaxSal = Max,
        !.

    min([Min], Min). %если список состоит только из одного элемента
    min([S1, S2 | T], Min) :-
        S1 <= S2,
        min([S1 | T], Min).
    min([S1, S2 | T], Min) :-
        S1 >= S2,
        min([S2 | T], Min).

    minimum_salary() = MinSal :-
        min([ Budget || salary(_, Budget) ], Min),
        MinSal = Min,
        !.

    print_list([]).
    print_list([X | Xs]) :-
        write(X),
        nl,
        print_list(Xs).

    run() :-
        file::consult("../databaseMargo.txt", organization),
        fail.

    run() :-
        write("\nThe staff of Analytics Department: \n"),
        List = employee_in_department("Analytics Department"),
        list(List),
        nl,
        fail.

    run() :-
        nl,
        write("List of men of Analytics department:\n"),
        department_employeer_male("Analytics Department"),
        fail.

    run() :-
        nl,
        write("Hierarchy of subordination of departments:\n"),
        subordination_of_departments(),
        fail.

    run() :-
        nl,
        nl,
        write("The age of the employee since the beginning of work. \n"),
        age_of_work(7),
        fail.

    run() :-
        nl,
        nl,
        write("Hierarchy of Analytics department employees:\n"),
        hierarchy_of_department_employees("Analytics Department"),
        fail.

    run() :-
        nl,
        write("Output of information about an employee by ID."),
        nl,
        employee(3),
        fail.

    run() :-
        write("List of employees of the Analytics department:\n"),
        department_employee("Analytics Department"),
        nl,
        fail.

    run() :-
        write("__________💵💰__________\n"),
        fail.

    run() :-
        nl,
        write("Total salary of the Legal Suppor department: "),
        department_sal("Legal Support Department"),
        fail.

    run() :-
        write("\nThe total budget of the company: ", amount_of_salaries(), "💲"),
        nl,
        nl,
        fail.
    run() :-
        write("The maximum salary of an employee: ", maximum_salary(), "💲"),
        nl,
        nl,
        fail.
    run() :-
        write("The minimum salary of an employee: ", minimum_salary(), "💲"),
        nl,
        fail.

    run() :-
        nl,
        write("Female employees: ", "\n"),
        print_female_names,
        fail.

    run().

end implement main

goal
    console::run(main::run).
