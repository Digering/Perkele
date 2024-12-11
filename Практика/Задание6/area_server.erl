-module(area_server).
        -behaviour(gen_server).

        %% API
        -export([start_link/0, area/1]).

        %% gen_server callbacks
        -export([init/1, handle_call/3]).

        %% Запуск сервера
        start_link() ->
        gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

        %% Вычисление площади
        area(Figure) ->
        gen_server:call(?MODULE, {area, Figure}).

        %% Инициализация сервера
        init([]) ->
        {ok, []}.

        %% Обработка call-сообщений (вычисление площади)
        handle_call({area, {circle, Radius}}, _From, State) ->
        Area = math:pi() * Radius * Radius,
        {reply, Area, State};

        handle_call({area, {square, Side}}, _From, State) ->
        Area = Side * Side,
        {reply, Area, State};

        handle_call({area, {trapezoid, Base1, Base2, Height}}, _From, State) ->
        Area = 0.5 * (Base1 + Base2) * Height,
        {reply, Area, State}.

        % Запуск сервера
        {ok, Pid} = area_server:start_link(),

        % Вычисление площади разных фигур
        IO:format("Circle area: ~p~n", [area_server:area({circle, 4.5})]),  % Площадь круга с радиусом 4.5
        IO:format("Square area: ~p~n", [area_server:area({square, 12})]),    % Площадь квадрата со стороной 12
        IO:format("Trapezoid area: ~p~n", [area_server:area({trapezoid, 2, 4, 3.4})])  % Площадь трапеции с основаниями 2, 4 и высотой 3.4
