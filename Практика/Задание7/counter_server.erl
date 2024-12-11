-module(counter_server).
        -behaviour(gen_server).

        %% API
        -export([start_link/0, add/1, get/0, reset/0]).

        %% gen_server callbacks
        -export([init/1, handle_cast/2, handle_call/3]).

        %% Запуск сервера
        start_link() ->
        gen_server:start_link({local, ?MODULE}, ?MODULE, 0, []).

        %% Добавление к счетчику (cast)
        add(Value) ->
        gen_server:cast(?MODULE, {add, Value}).

        %% Получение значения счетчика (call)
        get() ->
        gen_server:call(?MODULE, get).

        %% Сброс счетчика (cast)
        reset() ->
        gen_server:cast(?MODULE, reset).

        %% Инициализация сервера (счетчик изначально равен 0)
        init(InitialState) ->
        {ok, InitialState}.

        %% Обработка cast-сообщений

        %% Увеличение счетчика
        handle_cast({add, Value}, State) ->
        NewState = State + Value,
        {noreply, NewState};

        %% Сброс счетчика
        handle_cast(reset, _State) ->
        {noreply, 0}.

        %% Обработка call-сообщений

        %% Получение значения счетчика
        handle_call(get, _From, State) ->
        {reply, State, State}.

        % Запуск сервера
        {ok, Pid} = counter_server:start_link(),

        % Увеличение счетчика
        counter_server:add(5),
        counter_server:add(3),

        % Получение значения счетчика
        IO:format("Current counter value: ~p~n", [counter_server:get()]),  % Должно вывести 8

        % Сброс счетчика
        counter_server:reset(),

        % Проверка значения после сброса
        IO:format("Counter value after reset: ~p~n", [counter_server:get()])  % Должно вывести 0
