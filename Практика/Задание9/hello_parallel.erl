-module(hello_parallel).
        -export([start/0]).

        start() ->
        % Запускаем 5 параллельных процессов
        lists:foreach(fun(N) -> spawn(fun() -> print_hello(N) end) end, lists:seq(1, 5)).

        % Функция для вывода сообщения с задержкой
        print_hello(N) ->
        lists:foreach(fun(_) ->
        io:format("Hello from ~p!~n", [N]),
        timer:sleep(1000) % Задержка 1 секунда
        end, lists:seq(1, 5)).


        hello_parallel:start().
