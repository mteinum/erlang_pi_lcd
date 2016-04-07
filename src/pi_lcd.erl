-module(pi_lcd).
-compile(export_all).

%% Char LCD plate GPIO numbers.

-define(LCD_PLATE_RS,   15).
-define(LCD_PLATE_RW,   14).
-define(LCD_PLATE_EN,   13).
-define(LCD_PLATE_D4,   12).
-define(LCD_PLATE_D5,   11).
-define(LCD_PLATE_D6,   10).
-define(LCD_PLATE_D7,    9).
-define(LCD_PLATE_RED,   6).
-define(LCD_PLATE_GREEN, 7).
-define(LCD_PLATE_BLUE,  8).

%% # Commands
-define(LCD_CLEARDISPLAY        , 16#01).
-define(LCD_RETURNHOME          , 16#02).
-define(LCD_ENTRYMODESET        , 16#04).
-define(LCD_DISPLAYCONTROL      , 16#08).
-define(LCD_CURSORSHIFT         , 16#10).
-define(LCD_FUNCTIONSET         , 16#20).
-define(LCD_SETCGRAMADDR        , 16#40).
-define(LCD_SETDDRAMADDR        , 16#80).

%% Entry flags
-define(LCD_ENTRYRIGHT          , 16#00).
-define(LCD_ENTRYLEFT           , 16#02).
-define(LCD_ENTRYSHIFTINCREMENT , 16#01).
-define(LCD_ENTRYSHIFTDECREMENT , 16#00).

%% Control flags
-define(LCD_DISPLAYON           , 16#04).
-define(LCD_DISPLAYOFF          , 16#00).
-define(LCD_CURSORON            , 16#02).
-define(LCD_CURSOROFF           , 16#00).
-define(LCD_BLINKON             , 16#01).
-define(LCD_BLINKOFF            , 16#00).

%% Move flags
-define(LCD_DISPLAYMOVE         , 16#08).
-define(LCD_CURSORMOVE          , 16#00).
-define(LCD_MOVERIGHT           , 16#04).
-define(LCD_MOVELEFT            , 16#00).

%% Function set flags
-define(LCD_8BITMODE            , 16#10).
-define(LCD_4BITMODE            , 16#00).
-define(LCD_2LINE               , 16#08).
-define(LCD_1LINE               , 16#00).
-define(LCD_5x10DOTS            , 16#04).
-define(LCD_5x8DOTS             , 16#00).

-define(GPIO_IN,   1).
-define(GPIO_OUT,  0).
-define(GPIO_HIGH, 1).
-define(GPIO_LOW,  0).

% # Char LCD plate button names.
-define(SELECT                  , 0).
-define(RIGHT                   , 1).
-define(DOWN                    , 2).
-define(UP                      , 3).
-define(LEFT                    , 4).

% lcd state
-record(state, {
	displaycontrol = ?LCD_DISPLAYON bor ?LCD_CURSOROFF bor ?LCD_BLINKOFF,
	displayfunction = ?LCD_4BITMODE bor ?LCD_1LINE bor ?LCD_2LINE bor ?LCD_5x8DOTS,
	displaymode = ?LCD_ENTRYLEFT bor ?LCD_ENTRYSHIFTDECREMENT }).

init_button(Button) ->
	mcp:setup(Button, ?GPIO_IN),
	mcp:pullup(Button, 1).

start() ->
	spawn(?MODULE, init, []).

init() ->
	register(?MODULE, self()),
	% Set LCD R/W pin to low for writing only.
	mcp:setup(?LCD_PLATE_RW, ?GPIO_OUT),
	mcp:output(?LCD_PLATE_RW, ?GPIO_LOW),

	% Set buttons as inputs with pull-ups enabled.
	[init_button(Button) || Button <- [?SELECT, ?RIGHT, ?DOWN, ?UP, ?LEFT]],
	
	% Setup all pins as outputs.
	[mcp:setup(Pin, ?GPIO_OUT) || Pin <- [
     								?LCD_PLATE_RS,
                                    ?LCD_PLATE_EN,
                                    ?LCD_PLATE_D4,
                                    ?LCD_PLATE_D5,
                                    ?LCD_PLATE_D6,
                                    ?LCD_PLATE_D7]],
	% Initialize the display.
	write8(16#33),
    write8(16#32),
    % Initialize display control, function, and mode registers
    State = #state{},

    write8(?LCD_DISPLAYCONTROL bor State#state.displaycontrol),
    write8(?LCD_FUNCTIONSET bor State#state.displayfunction),
    write8(?LCD_ENTRYMODESET bor State#state.displaymode),

	write8(?LCD_CLEARDISPLAY),
	wait(300),

    mcp:setup(?LCD_PLATE_RED, ?GPIO_OUT),
    mcp:setup(?LCD_PLATE_GREEN, ?GPIO_OUT),
    mcp:setup(?LCD_PLATE_BLUE, ?GPIO_OUT),

    % mcp:output_pins(rgb_to_pins(1,1,1))
    mcp:output_pins(#{
    	?LCD_PLATE_RED => 0,
    	?LCD_PLATE_GREEN => 0,
    	?LCD_PLATE_BLUE => 0
    	}),

    loop(State)
.

flip(Value, FlagOn, _FlagOff, Enable) when Enable =:= 1 ->
	Value bor FlagOn;
flip(Value, _FlagOn, FlagOff, Enable) when Enable =:= 0 ->
	Value band bnot(FlagOff).

loop(State) ->
	receive
		{clear, _From} ->
			write8(?LCD_CLEARDISPLAY),
			wait(300),
			loop(State);

		{enable_display, _From, Enable} ->
			DisplayControl = flip(State#state.displaycontrol, ?LCD_DISPLAYON, ?LCD_DISPLAYOFF, Enable),
			write8(?LCD_DISPLAYCONTROL bor DisplayControl),
			loop(State#state{displaycontrol=DisplayControl});

		{show_cursor, _From, Show} ->
			DisplayControl = flip(State#state.displaycontrol, ?LCD_CURSORON, ?LCD_CURSOROFF, Show),
			write8(?LCD_DISPLAYCONTROL bor DisplayControl),
			loop(State#state{displaycontrol=DisplayControl});

		{message, _From, Text} ->
			[write8(C, 1) || C <- Text],
			loop(State)
	end.

is_bit_set(Value, Bit) ->
	((Value bsr Bit) band 1).

write8(Value) ->
	write8(Value, 0).

write8(Value, CharMode) ->
	wait(10),
	% set char_mode = False
	mcp:output(?LCD_PLATE_RS, CharMode),
	% write upper 4 bits
	mcp:output_pins(#{
		?LCD_PLATE_D4 => is_bit_set(Value, 4),
		?LCD_PLATE_D5 => is_bit_set(Value, 5),
		?LCD_PLATE_D6 => is_bit_set(Value, 6),
		?LCD_PLATE_D7 => is_bit_set(Value, 7)
		}),
	pulse_enable(),
	% write lower 4 bits
	mcp:output_pins(#{
		?LCD_PLATE_D4 => is_bit_set(Value, 0),
		?LCD_PLATE_D5 => is_bit_set(Value, 1),
		?LCD_PLATE_D6 => is_bit_set(Value, 2),
		?LCD_PLATE_D7 => is_bit_set(Value, 3)
		}),
	pulse_enable()
.

pulse_enable() ->
	mcp:output(?LCD_PLATE_EN, 0),
	wait(1),
	mcp:output(?LCD_PLATE_EN, 1),
	wait(1),
	mcp:output(?LCD_PLATE_EN, 0),
	wait(1)
.

clear() ->
	?MODULE ! {clear, self()}.

enable_display(Enable) ->
	?MODULE ! {enable_display, self(), Enable}.

show_cursor(Show) ->
	?MODULE ! {show_cursor, self(), Show}.

message(Text) ->
	?MODULE ! {message, self(), Text}.

wait(Ms) ->
	receive
	after Ms ->
		true
	end.
