-module(parallel_sum).
        -export([start/0, calculate_sum/4]).

        % Основная функция
        start() ->
        % Определяем параметры
        Start = 0.0,
        End = 3.14,
        Step = 0.0000001,

        % Первая группа потоков
        Pid1 = spawn(parallel_sum, calculate_sum, [Start, End, Step, fun (X) -> math:cos(X) + 45 * X * X end]),
        Pid2 = spawn(parallel_sum, calculate_sum, [Start, End, Step, fun (X) -> 1 / (X + 1) end]),

        % Получаем результаты первой группы
        {ok, Result1} = receive_result(Pid1),
        {ok, Result2} = receive_result(Pid2),

        % Вторая группа потоков
        Pid3 = spawn(parallel_sum, calculate_sum, [Start, End, Step, fun (X) -> 34 * X + 0.098 end]),
        Pid4 = spawn(parallel_sum, calculate_sum, [Start, End, Step, fun (X) -> 17 * X + 9.8 end]),

        % Получаем результаты второй группы
        {ok, Result3} = receive_result(Pid3),
        {ok, Result4} = receive_result(Pid4),

        % Вычисляем среднее значение
        Average = (Result1 + Result2 + Result3 + Result4) / 4,

        % Выводим результаты
        io:format("Sum 1 (cos(x) + 45x^2): ~p~n", [Result1]),
        io:format("Sum 2 (1/(x+1)): ~p~n", [Result2]),
        io:format("Sum 3 (34x + 0.098): ~p~n", [Result3]),
        io:format("Sum 4 (17x + 9.8): ~p~n", [Result4]),
        io:format("Average of all sums: ~p~n", [Average]),
        ok.

        % Функция для вычисления суммы значений функции
        calculate_sum(Start, End, Step, Func) ->
        Sum = calculate_loop(Start, End, Step, Func, 0.0),
        self() ! {ok, Sum}.

        % Рекурсивное вычисление суммы
        calculate_loop(Current, End, Step, Func, Acc) when Current > End ->
        Acc;
        calculate_loop(Current, End, Step, Func, Acc) ->
        NewAcc = Acc + Func(Current),
        calculate_loop(Current + Step, End, Step, Func, NewAcc).

        % Получение результата от процесса
        receive_result(Pid) ->
        receive
        {ok, Result} -> {ok, Result}
        after 5000 ->
        {error, timeout}
        end.


        1> c(parallel_sum).
        {ok,parallel_sum}
        2> parallel_sum:start().
        Sum 1 (cos(x) + 45x^2): 468.78520376210845
        Sum 2 (1/(x+1)): 2.4846874791468295
        Sum 3 (34x + 0.098): 169.28334
        Sum 4 (17x + 9.8): 67.94167
        Average of all sums: 177.62347581081383
        ok
