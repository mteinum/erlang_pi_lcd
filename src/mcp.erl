
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% MCP23017 interface. Keep register cached
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(mcp).
-compile(export_all).

-record(state, {iodir = 0, gppu = 0, gpio = 0}).

-define(GPIO,  16#12).
-define(IODIR, 16#00).
-define(GPPU,  16#0C).

-define(GPIO_OUT, 0).
-define(GPIO_IN,  1).

word_to_byte_array(Word) ->
	[Word band 16#ff, Word bsr 8].

write_word(Register, W) ->
	i2c:write_list(Register, word_to_byte_array(W)).

write_iodir(Value) ->
	write_word(?IODIR, Value).

write_gppu(Value) ->
	write_word(?GPPU, Value).

write_gpio(Value) ->
	write_word(?GPIO, Value).

set_pin(Word, Pin, Value) when Value =:= 1 ->
	Word bor (1 bsl Pin);
set_pin(Word, Pin, Value) when Value =:= 0 ->
	Word band bnot(1 bsl Pin).

start() ->
	spawn(?MODULE, init, []).

init() ->
	register(?MODULE, self()),
	write_iodir(0),
	write_gppu(0),
	loop(#state{}).

loop(State) ->
	receive
		{setup, _From, Pin, Value} ->
			IODir = set_pin(State#state.iodir, Pin, Value),
			write_iodir(IODir),
			loop(State#state{iodir = IODir});

		{output, _From, Pin, Value} ->
			Gpio = set_pin(State#state.gpio, Pin, Value),
			write_gpio(Gpio),
			loop(State#state{gpio = Gpio});

		{output_pins, _From, Pins} ->
			Fun = fun(K, V, AccIn) -> set_pin(AccIn, K, V) end,
			Gpio = maps:fold(Fun, State#state.gpio, Pins),
			write_gpio(Gpio),
			loop(State#state{gpio = Gpio});

		{pullup, _From, Pin, Enabled} ->
			Gppu = set_pin(State#state.gppu, Pin, Enabled),
			write_gppu(Gppu),
			loop(State#state{gppu = Gppu})

	end
.

setup(Pin, Value) ->
	?MODULE ! {setup, self(), Pin, Value}.

output(Pin, Value) ->
	?MODULE ! {output, self(), Pin, Value}.

output_pins(Pins) ->
	?MODULE ! {output_pins, self(), Pins}.

pullup(Pin, Enabled) ->
	?MODULE ! {pullup, self(), Pin, Enabled}.

