
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% MCP23017 interface. Keep register cached
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(mcp).
-behaviour(gen_server).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start_link/0, init/1]).
-export([setup/2, output/2, output_pins/1, pullup/2]).

-record(state, {iodir = 0, gppu = 0, gpio = 0}).

-define(GPIO,  16#12).
-define(IODIR, 16#00).
-define(GPPU,  16#0C).

-define(GPIO_OUT, 0).
-define(GPIO_IN,  1).

write_iodir(Value) ->
	i2c:write_word(?IODIR, Value).

write_gppu(Value) ->
	i2c:write_word(?GPPU, Value).

write_gpio(Value) ->
	i2c:write_word(?GPIO, Value).

set_pin(Word, Pin, Value) when Value =:= 1 ->
	Word bor (1 bsl Pin);
set_pin(Word, Pin, Value) when Value =:= 0 ->
	Word band bnot(1 bsl Pin).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
	write_iodir(0),
	write_gppu(0),
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast({setup, Pin, Value}, State) ->
	IODir = set_pin(State#state.iodir, Pin, Value),
	write_iodir(IODir),
	{noreply, State#state{iodir = IODir}};

handle_cast({output, Pin, Value}, State) ->
	Gpio = set_pin(State#state.gpio, Pin, Value),
	write_gpio(Gpio),
	{noreply, State#state{gpio = Gpio}};

handle_cast({output_pins, Pins}, State) ->
	Fun = fun(K, V, AccIn) -> set_pin(AccIn, K, V) end,
	Gpio = maps:fold(Fun, State#state.gpio, Pins),
	write_gpio(Gpio),
	{noreply, #state{gpio = Gpio}};

handle_cast({pullup, Pin, Enabled}, State) ->
	Gppu = set_pin(State#state.gppu, Pin, Enabled),
	write_gppu(Gppu),
	{noreply, State#state{gppu = Gppu}}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

setup(Pin, Value) ->
	gen_server:cast(?MODULE, {setup, Pin, Value}).

output(Pin, Value) ->
	gen_server:cast(?MODULE, {output, Pin, Value}).

output_pins(Pins) ->
	gen_server:cast(?MODULE, {output_pins, Pins}).

pullup(Pin, Enabled) ->
	gen_server:cast(?MODULE, {pullup, Pin, Enabled}).
