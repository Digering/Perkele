% parallel_sum.erl
        -module(parallel_sum).
        -export([main/0]).

        main() ->
        Start = 0,
        End = 3.14,
        Step = 0.0000001,

        % Параллельный запуск двух потоков для первых функций
        Pid1 = spawn(fun() -> calculate_sum(Start, End, Step, fun(X -> math:cos(X) + 45 * math:pow(X, 2) end), self() end),
        Pid2 = spawn(fun() -> calculate_sum(Start, End, Step, fun(X -> 1 / (X + 1) end), self() end),

        % Получение результатов из двух потоков
        receive
        {result, Sum1} -> io:format("Сумма функции 1 (y = cos(x) + 45x^2): ~p~n", [Sum1])
        end,
        receive
        {result, Sum2} -> io:format("Сумма функции 2 (y = 1 / (x + 1)): ~p~n", [Sum2])
        end,

        % Запуск третьего потока для y = 34x + 0.098
        Pid3 = spawn(fun() -> calculate_sum(Start, End, Step, fun(X -> 34 * X + 0.098 end), self() end),

        % Получение результата из третьего потока
        receive
        {result, Sum3} -> io:format("Сумма функции 3 (y = 34x + 0.098): ~p~n", [Sum3])
        end,

        % Вычисление среднего значения
        Average = (Sum1 + Sum2 + Sum3) / 3,
        io:format("Среднее значение: ~p~n", [Average]).

        calculate_sum(Start, End, Step, Fun, Parent) ->
        Sum = calculate_sum_loop(Start, End, Step, Fun, 0),
        Parent ! {result, Sum}.

        calculate_sum_loop(X, End, Step, Fun, Acc) when X > End ->
        Acc;
        calculate_sum_loop(X, End, Step, Fun, Acc) ->
        NewAcc = Acc + Fun(X),
        calculate_sum_loop(X + Step, End, Step, Fun, NewAcc).
