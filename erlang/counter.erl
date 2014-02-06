-module(counter).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, inc/1, dec/1, set_counter/1, get_counter/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, 0, []).

inc(N) ->
    gen_server:cast(?SERVER, {inc, N}).

dec(N) ->
    gen_server:cast(?SERVER, {dec, N}).

set_counter(N)  ->
    gen_server:cast(?SERVER, {set_counter, N}).

get_counter() ->
    gen_server:call(?SERVER, {get_counter}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-record(state,{amount}).

init(Amount) ->
    {ok, #state{amount=Amount}}.

handle_call({get_counter}, _From, State) ->
    {reply, State#state.amount, State}.

handle_cast({inc, N}, State) ->
    {noreply, State#state{amount=State#state.amount + N}};
handle_cast({dec, N}, State) ->
    {noreply, State#state{amount=State#state.amount - N}};
handle_cast({set_counter, N}, State) ->
    {noreply,State#state{amount=N}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% EUnit tests
%% ------------------------------------------------------------------
one_thread() ->
    ok = counter:set_counter(0),
    ok = counter:inc(10),
    ok = counter:inc(5),
    ok = counter:dec(8),
    ?assertEqual(7, counter:get_counter()).

for(N,N,F) -> F();
for(I,N,F) -> [F()|for(I+1,N,F)].

wait(N) when N >= 10 -> true;
wait(N) ->
    receive 
        _ -> wait(N+1)
    end.

many_thread() ->
    ok = counter:set_counter(0),
    Pid = self(),
    for(0,9,fun() ->
                spawn(fun() ->
                    ok = counter:inc(10),
                    Pid ! done
                    end)
            end),
    wait(0),
    ?assertEqual(100, counter:get_counter()).

counter_test_() ->
   {setup,
    fun() -> counter:start_link() end,
    [?_test(one_thread()),
     ?_test(many_thread())]}.
