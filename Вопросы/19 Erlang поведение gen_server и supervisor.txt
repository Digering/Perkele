В Erlang поведение (`behaviour`) — это шаблон для создания процесса, который выполняет стандартные операции и обрабатывает ошибки определённым образом. Два популярных поведения в Erlang — это **gen_server** и **supervisor**. Оба они входят в фреймворк Erlang OTP и обеспечивают удобный способ создания отказоустойчивых и масштабируемых приложений.

### 1. **Поведение gen_server**

**gen_server** (генератор серверов) — это поведение для создания серверов, которые обрабатывают запросы и выполняют долгосрочные операции. Он предоставляет структуру для обработки синхронных и асинхронных вызовов, а также управления состоянием.

#### Структура gen_server
- **Синхронные вызовы** (`handle_call`): Для обработки запросов от клиента, которые требуют ответа.
- **Асинхронные вызовы** (`handle_cast`): Для обработки запросов без ответа (например, операции, которые не требуют возвращаемого значения).
- **Обработка ошибок**: В случае ошибки в процессе, **gen_server** может перезапустить процесс с помощью супервизора.

#### Пример: базовый сервер с gen_server

```erlang
-module(counter).
-behaviour(gen_server).

%% API
-export([start_link/0, increment/0, get_value/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

increment() ->
    gen_server:cast(?MODULE, increment).

get_value() ->
    gen_server:call(?MODULE, get_value).

init([]) ->
    {ok, 0}.  % Начальное состояние

handle_call(get_value, _From, State) ->
    {reply, State, State};  % Ответ на запрос get_value

handle_cast(increment, State) ->
    {noreply, State + 1}.  % Увеличение значения на 1

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

**Ключевые части кода**:
- `start_link/0`: Запускает сервер как процесс.
- `increment/0`: Отправляет асинхронный запрос на увеличение значения.
- `get_value/0`: Запрашивает синхронный ответ (текущее значение).

#### Основные callback-функции:
- `init/1`: Инициализация состояния сервера.
- `handle_call/3`: Обработка синхронных запросов, например, получение значения.
- `handle_cast/2`: Обработка асинхронных запросов, например, обновление состояния.
- `terminate/2`: Завершение работы сервера (обычно для очистки ресурсов).
- `code_change/3`: Обработка изменений версии кода (для системы обновлений).

---

### 2. **Поведение supervisor**

**supervisor** (супервизор) — это поведение для создания процессов-наблюдателей, которые управляют другими процессами и следят за их состоянием. Если наблюдаемый процесс выходит из строя, супервизор может его перезапустить, обеспечивая отказоустойчивость системы.

Супервизор не решает проблему обработки сообщений, а именно управляет жизненным циклом других процессов. Например, он может перезапустить процессы, которые выходят из строя.

#### Структура supervisor
- **Стратегии перезапуска**:
  - `one_for_one`: Перезапускает только тот процесс, который завершился с ошибкой.
  - `one_for_all`: Перезапускает все дочерние процессы, если один завершился с ошибкой.
  - `rest_for_one`: Перезапускает процесс и все дочерние процессы, которые были запущены после него.
  - `simple_one_for_one`: Оптимизирована для динамического создания дочерних процессов.

#### Пример: супервизор для процесса

```erlang
-module(counter_supervisor).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % Определение дочерних процессов (workers)
    ChildSpec = {counter, {counter, start_link, []}, permanent, 5000, worker, [counter]},
    {ok, {{one_for_one, 5, 10}, [ChildSpec]}}.
```

**Ключевые части кода**:
- `start_link/0`: Запуск супервизора как процесса.
- `init/1`: Определяет дочерние процессы и стратегии их перезапуска.

#### Основные callback-функции:
- `init/1`: Инициализация супервизора и описание дочерних процессов, которые он будет управлять.
- Возвращает конфигурацию для стратегии перезапуска и список дочерних процессов.

---

### 3. **Работа с супервизорами и gen_server вместе**

Часто **gen_server** и **supervisor** используются в связке: супервизор следит за состоянием серверных процессов, и в случае сбоя перезапускает их. Это повышает отказоустойчивость системы.

#### Пример: запуск серверов с супервизором

```erlang
-module(counter).
-behaviour(gen_server).

-export([start_link/0, increment/0, get_value/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

increment() ->
    gen_server:cast(?MODULE, increment).

get_value() ->
    gen_server:call(?MODULE, get_value).

init([]) ->
    {ok, 0}.  % Начальное состояние

handle_call(get_value, _From, State) ->
    {reply, State, State};

handle_cast(increment, State) ->
    {noreply, State + 1}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

```erlang
-module(counter_supervisor).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpec = {counter, {counter, start_link, []}, permanent, 5000, worker, [counter]},
    {ok, {{one_for_one, 5, 10}, [ChildSpec]}}.
```

**Процесс работы**:
1. **Супервизор** запускает процесс **counter** с помощью поведения **gen_server**.
2. Если процесс **counter** завершится с ошибкой, супервизор автоматически перезапустит его, сохраняя отказоустойчивость.

---

### Заключение

- **gen_server** — это поведение для создания серверных процессов с асинхронными и синхронными вызовами. Оно предоставляет упрощённый интерфейс для работы с состоянием и ошибками.
- **supervisor** — это поведение для создания супервизоров, которые следят за другими процессами и перезапускают их при сбоях, обеспечивая отказоустойчивость системы.

Совместное использование этих двух поведений помогает создавать надёжные, отказоустойчивые и масштабируемые системы в Erlang.