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
    department_employee : (string Name) nondeterm.
    department_employeer_male : (string Name) nondeterm.
    employee : (integer IdSal) nondeterm.
    department_sal : (string Name) nondeterm.
    age_of_work : (integer EmployeeId) nondeterm.
    hierarchy_of_department_employees : (string Name) nondeterm.

clauses

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
        write("Director 유 - ", NName),
        nl,
        fail.
    hierarchy_of_department_employees(Name) :-
        department(DepartmentId, Name),
        employee(IDemployee, NName, _, _, _),
        works(IDemployee, PositionId, _),
        position(PositionId, "employee", _, DepartmentId),
        write("Worker - ", NName),
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
        assert(s(Amount + X)), %результат сохраняется в переменную Amount 
        fail.
    department_sal(Name) :-
        department(_, Name),
        s(Amount),
        write(Amount, "$\n").


    run() :-
        file::consult("../databaseMargo.txt", organization),
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
        fail.

    run() :-
        nl,
        write("Total salary of the Legal Suppor department: "),
        department_sal("Legal Support Department"),
        fail.

    run().

end implement main

goal
    console::run(main::run).
