-module(natural_number_server).
        -behaviour(gen_server).

        %% API
        -export([start_link/0, get/0, reset/0]).

        %% gen_server callbacks
        -export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

        %% Starts the Natural Number server
        start_link() ->
        gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

        %% API Functions
        get() ->
        gen_server:call(?MODULE, get).

        reset() ->
        gen_server:cast(?MODULE, reset).

        %% gen_server callbacks
        init([]) ->
        %% Initial state
        {ok, 1}.

        handle_call(get, _From, Current) ->
        %% Return the current number and increment the state
        {reply, Current, Current + 1}.

        handle_cast(reset, _State) ->
        %% Reset the state to the initial value
        {noreply, 1}.

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
        %% 1> natural_number_server:start_link().
        %% 2> natural_number_server:get().
        %% 3> natural_number_server:get().
        %% 4> natural_number_server:reset().
