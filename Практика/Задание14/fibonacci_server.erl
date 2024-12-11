-module(fibonacci_server).
        -behaviour(gen_server).

        %% API
        -export([start_link/0, get/0, reset/0]).

        %% gen_server callbacks
        -export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

        %% Starts the Fibonacci server
        start_link() ->
        gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

        %% API Functions
        get() ->
        gen_server:call(?MODULE, get).

        reset() ->
        gen_server:cast(?MODULE, reset).

        %% gen_server callbacks
        init([]) ->
        %% Initial state {Fn-2, Fn-1}
        {ok, {0, 1}}.

        handle_call(get, _From, {F0, F1}) ->
        %% Calculate the next Fibonacci number
        Next = F0 + F1,
        {reply, F1, {F1, Next}}.

        handle_cast(reset, _State) ->
        %% Reset the state to the initial Fibonacci values
        {noreply, {0, 1}}.

        handle_info(_Msg, State) ->
        %% No unexpected messages expected
        {noreply, State}.

        terminate(_Reason, _State) ->
        %% Cleanup if necessary
        ok.

        code_change(_OldVsn, State, _Extra) ->
        %% Handle code upgrades
        {ok, State}.

        %% Example usage:
        %% 1> fibonacci_server:start_link().
        %% 2> fibonacci_server:get().
        %% 3> fibonacci_server:get().
        %% 4> fibonacci_server:reset().
