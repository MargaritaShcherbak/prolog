% Вариант 7
% Предметная область: Сотрудники компании Яндекс

% Факты
% Определение сотрудников
% Сотрудник (id, имя, пол, дата рождения, дата приема на работу)
сотрудник(1, "Крылов Максим Иванович", м, date(1970, 1, 21), date(2005, 1, 1)).
сотрудник(2, "Долганов Николай Владимирович", м, date(1985, 12, 2), date(2023, 5, 11)).
сотрудник(3, "Егорова Елена Ивановна", ж, date(1980, 3, 13), date(2012, 10, 1)).
сотрудник(4, "Иванов Дмитрий Дмитриевич", м, date(1990, 6, 15), date(2015, 9, 15)).
сотрудник(5, "Шатохина Алиса Викторовна", ж, date(2000, 3, 12), date(2022, 11, 18)).
сотрудник(6, "Махорин Глеб Петрович", м, date(1995, 12, 28), date(2023, 4, 23)).
сотрудник(7, "Кашкина Анастасия Сергеевна", ж, date(1985, 8, 23), date(2010, 6, 19)).
сотрудник(8, "Валов Дмитрий Андреевич", м, date(1965, 10, 30), date(2000, 1, 1)).
сотрудник(9, "Нечаева Полина Романовна", ж, date(1999, 12, 12), date(2023, 5, 6)).


% Определение отделов
% Отдел (id, название)
отдел(11, "Отдел разработки").
отдел(22, "Отдел маркетинга").
отдел(33, "Отдел продаж").
отдел(44, "Отдел аналитики").
отдел(55, "Отдел качества").
отдел(66, "Отдел IT-инфраструктуры").
отдел(77, "Отдел дизайна").
отдел(88, "Отдел правовой поддержки").


% Определение должностей
% Должность (id, название, описание, id_отдела)
должность(111, "Разработчик", "Разработка программного обеспечения", 11).
должность(222, "Менеджер по маркетингу", "Продвижение продуктов компании на рынке", 22).
должность(333, "Менеджер по продажам", "Продажа продуктов компании", 33).
должность(444, "Системный администратор", "Обслуживание и поддержка ИТ-систем компании", 66).
должность(555, "UX/UI дизайнер", "Разработка дизайна пользовательского интерфейса", 77).
должность(666, "Юрист", "Правовая поддержка бизнеса компании", 88).
должность(777, "Аналитик данных", "Анализ данных и разработка статистических моделей", 44).
должность(888, "Инженер по тестированию", "Тестирование программного обеспечения", 11).
должность(999, "Эксперт по качеству ", "Оценка качества предоставляемых услу и  выполнения заказов", 55).


% Определение занятости сотрудников на должностях
% Занимает (id_сотрудника, id_должности, дата начала)
занимает(1, 111, date(2020, 6, 13)).
занимает(2, 222, date(2017, 5, 17)).
занимает(3, 333, date(2018, 3, 1)).
занимает(4, 444, date(2022, 11, 18)).
занимает(5, 555, date(2021, 9, 19)).
занимает(6, 666, date(2019, 7, 22)).
занимает(7, 777, date(2019, 4, 24)).
занимает(8, 888, date(2021, 1, 10)).
занимает(9, 999, date(2020, 11, 10)).


% Правила
% Дополнительно. Определение отдела по id
отдел_по_id(Id, Название_отдела) :- отдел(Id, Название_отдела).
    
% Дополнительно. Определение должности по id
должность_по_id(Id, Название_должности, Описание, Id_отдела) :- должность(Id, Название_должности, Описание, Id_отдела).
    

% Правило, позволяющее сгенерировать список сотрудников заданного отдела
% Вывод сотрудник(а)/(ов) по id отдела
генерация_сотрудников(ID_отдела) :-
    сотрудник(ID, Имя, Пол, ДР, Дата_приема),
    занимает(ID, ID_должности, _),
    должность(ID_должности, _, _, ID_отдела),
    отдел(ID_отдела, _),
    write("ID: "), write(ID), nl,
    write("Имя: "), write(Имя), nl,
    write("Пол: "), write(Пол), nl,
    write("Дата рождения: "), write(ДР), nl,
    write("Дата приема на работу: "), write(Дата_приема), nl, nl, fail.
генерация_сотрудников(_).

% Вывод сотрудник(а)/(ов) по названию отдела в " " 
список_сотрудников_отдела(Отдел) :-
    отдел(Id_отдела, Отдел),
    должность(Id_должности, _, _, Id_отдела),
    занимает(Id_сотрудника, Id_должности, _),
    сотрудник(Id_сотрудника, Имя, _, _, _),
    write(Имя), nl, fail. 
список_сотрудников_отдела(_).


% Дополнительно. Поиск всех сотрудников в отделе
в_отделе(Отдел, Сотрудник) :-
    должность(Должность, _, _, Отдел),
    занимает(Сотрудник, Должность, _).
    
% Дополнительно. Поиск всех сотрудников на должности
на_должности(Должность, Сотрудник) :-
    занимает(Сотрудник, Должность, _).

% Дополнительно. Вывод списка сотрудников
список_сотрудников :-
    сотрудник(Id, Имя, Пол, _, _),
    write(Id), write(", "),
    write(Имя), write(", "),
    write(Пол), write(", "), nl, fail. %цикл
список_сотрудников.


% Подчинение (id_отдела_вышестоящего, id_отдела_подчиненного)
подчинение(88, 77).
подчинение(77, 66).
подчинение(66, 55).
подчинение(55, 44).
подчинение(44, 33).
подчинение(33, 22).
подчинение(22, 11).
подчинение(88, 33).
подчинение(88, 66).
подчинение(77, 22).


% Правило, определяющее иерархию подчинения отделов
подчинение_отделов(Отдел, Подчиненный_отдел) :- 
    подчинение(Отдел_id, Подчиненный_отдел_id),
    отдел(Отдел_id, Отдел),
    отдел(Подчиненный_отдел_id, Подчиненный_отдел).
    

% Дополнительно. Правило, выводящее информацию
информация(ID_отдела) :-
    отдел(ID_отдела, Название), write("Отдел: "), write(Название), nl,
    должность(ID_должности, Название_должности, _, ID_отдела), write("  Должность: "), write(Название_должности), nl,
    занимает(ID_сотрудника, ID_должности, _),
    сотрудник(ID_сотрудника, Имя, _, _, _), write("    Сотрудник: "), write(Имя), nl, nl, fail.
информация(_).


%Зарплата(Id, з/п)
%salary=integer.
%зарплата(id, salary).
зарплата(111, 10000).
зарплата(222, 20000).
зарплата(333, 30000).
зарплата(444, 40000).
зарплата(555, 50000).
зарплата(666, 60000).
зарплата(777, 70000).
зарплата(888, 80000).
зарплата(999, 90000).


% Дополнительно.  Правило, которое выводит годовую зарплату сотрудника
годовая_зарплата(Id_сотрудника, Зп_год) :- занимает(Id_сотрудника, Id_должности, _), зарплата(Id_должности, Зп_мес), Зп_год is Зп_мес * 12.

% Дополнительно. Суточная з/п
суточная_зп(Id_сотрудника, Зп_сутки) :- занимает(Id_сотрудника, Id_должности, _), зарплата(Id_должности, Зп_мес), Зп_сутки is Зп_мес / 30.

% Дополнительно. Максимальная з/п среди сотрудников
макс_зп(Max) :-
    зарплата(_, Max),
    not((зарплата(_, Salary), Salary > Max)).


% Запросы
% Вывести всех сотрудников мужского пола: 
сотрудник(_,X,Y,_,_,), Y=м.

% Вывести всех сотрудников, год рождения которых больше 1980
сотрудник(_,X,_,ДР,_), ДР = date(Год, _, _), Год >= 1981.

% Вывести сотрудников отдела разработки
отдел(X, "Отдел разработки"), должность(Y, _, _, X), занимает(Z, Y, _), сотрудник(Z, Имя, Пол, _, _).


%kolvo=integer.
%стаж(id, kolvo_лет).
стаж(111, 4).
стаж(222, 13).
стаж(333, 2).
стаж(444, 8).
стаж(555, 10).
стаж(666, 45).
стаж(777, 12).
стаж(888, 7).
стаж(999, 66).


% Дополнительно. Правило, вычисляющее, сколько еще месяцев с копейками нужно работать сотрудникам, чтобы получить свой первый миллион
первый_лям(ID, Months) :-
    зарплата(ID, Salary),
    стаж(ID, MonthsDone),
    MoneyDone is Salary * MonthsDone,
    MoneyRemaining is 1000000 - MoneyDone,
    MoneyRemaining > 0, %для тех, кто еще не заработал свой 1 лимон
    Months is (MoneyRemaining / Salary).


% Определение руководителя отдела (id_начальника, id_отдела)
руководитель_отдела(1, 22).
руководитель_отдела(3, 33).
руководитель_отдела(1, 55).
руководитель_отдела(5, 66).
руководитель_отдела(6, 88).
руководитель_отдела(9, 11).
руководитель_отдела(7, 44).

% Правило, определяющее руководителя отдела
начальники :-  
        write("Руководители_отделов: "),  nl,
    (
        руководитель_отдела(PersonId, DepId),
        отдел(DepId, Dep),
        сотрудник(PersonId, Person, _, _, _),
        write(Dep),
        write(" = "),
        write(Person), nl, nl, fail
    ), true.
начальники.


/*
length([], 0).
length([_|S], L):- length(S, Sl), L is Sl+1.
*/


% Правило, вычисляющее среднюю з/п сотрудников в отделе
avg_list(List, Avg) :-
	length(List, Len),
	sum_list(List, Sum),
	Avg is Sum / Len.

средняя_зп_в_отделе(Dep, AvgSal) :-
	отдел(DepId, Dep),
	findall(DolzId, должность(DolzId, _, _, DepId), DolzIds),
      findall(Зарплата, (
		зарплата(Id_должности, Зарплата),
		member(Id_должности, DolzIds)
	), Список_зарплат),
	avg_list(Список_зарплат, AvgSal).
	
% Запрос
% средняя_зп_в_отделе(X,Y)
%?- findall( (X,Y), средняя_зп_в_отделе(X,Y), Answers), write(Answers).

