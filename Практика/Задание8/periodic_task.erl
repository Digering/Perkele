-module(periodic_task).
        -behaviour(gen_server).

        %% API
        -export([start/2, stop/1]).

        %% gen_server callbacks
        -export([init/1, handle_cast/2, handle_info/2, terminate/2]).

        %% Запуск сервера с периодическим выполнением функции
        start(Period, Func) ->
        gen_server:start_link({local, ?MODULE}, ?MODULE, {Period, Func}, []).

        %% Остановка процесса
        stop(Pid) ->
        gen_server:cast(Pid, stop).

        %% Инициализация состояния
        init({Period, Func}) ->
        %% Запускаем таймер на первый вызов функции
        TimerRef = self() ! {execute_task, Func},
        ProcessState = {Period, Func, TimerRef},
        {ok, ProcessState}.

        %% Обработка cast-сообщений
        handle_cast(stop, {Period, Func, TimerRef}) ->
        %% Останавливаем процесс
        timer:cancel(TimerRef),
        {stop, normal, {Period, Func, TimerRef}}.

        %% Обработка сообщений таймера
        handle_info({execute_task, Func}, {Period, Func, TimerRef}) ->
        %% Выполняем переданную функцию
        Func(),
        %% Планируем следующий вызов через указанный период
        NewTimerRef = erlang:send_after(Period, self(), {execute_task, Func}),
        {noreply, {Period, Func, NewTimerRef}}.

        %% Завершение процесса
        terminate(_Reason, _State) ->
        ok.


        % Запуск процесса с периодом 1000 миллисекунд (1 секунда) для вывода "Hello!"
        {ok, Pid} = periodic_task:start(1000, fun() -> io:format("Hello!~n") end),

        % Остановка процесса через 5 секунд
        timer:sleep(5000),
        periodic_task:stop(Pid).
