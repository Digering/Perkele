-module(stack_server).
        -behaviour(gen_server).

        %% API
        -export([start_link/0, add/1, get/0]).

        %% gen_server callbacks
        -export([init/1, handle_cast/2, handle_call/3]).

        %% Запуск сервера
        start_link() ->
        gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

        %% Добавление элемента в стек (cast)
        add(Item) ->
        gen_server:cast(?MODULE, {add, Item}).

        %% Получение элемента из стека (call)
        get() ->
        gen_server:call(?MODULE, get).

        %% Инициализация сервера (пустой стек)
        init([]) ->
        {ok, []}.

        %% Обработка cast-сообщений (добавление элемента в стек)
        handle_cast({add, Item}, State) ->
        {noreply, [Item | State]}.

        %% Обработка call-сообщений (получение элемента из стека)
        handle_call(get, _From, []) ->
        {reply, empty, []};
        handle_call(get, _From, [Top | Rest]) ->
        {reply, Top, Rest}.


        % Запуск сервера
        {ok, Pid} = stack_server:start_link(),

        % Добавление элементов в стек
        stack_server:add(1),
        stack_server:add(2),
        stack_server:add(3),

        % Получение элементов из стека
        IO:format("~p~n", [stack_server:get()]), % 3
        IO:format("~p~n", [stack_server:get()]), % 2
        IO:format("~p~n", [stack_server:get()]), % 1
        IO:format("~p~n", [stack_server:get()])  % empty
