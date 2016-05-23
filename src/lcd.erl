-module(lcd).
-behaviour(gen_server).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start_link/0, init/1]).
-export([clear/0,
		 enable_display/1,
		 show_cursor/1,
		 message/1,
		 home/0,
		 set_cursor/2,
		 blink/1,
		 move_left/0,
		 move_right/0,
		 autoscroll/1,
		 backlight/1,
	 create_char/2
		 ]).

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
-define(LCD_4BITMODE            , 16#00).
-define(LCD_2LINE               , 16#08).
-define(LCD_1LINE               , 16#00).
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

% LCD_ROW_OFFSETS         = (0x00, 0x40, 0x14, 0x54)

lcd_row_offset(Row) when Row =:= 1 -> 16#00;
lcd_row_offset(Row) when Row =:= 2 -> 16#40.

% lcd state
-record(state, {
	displaycontrol = ?LCD_DISPLAYON bor ?LCD_CURSOROFF bor ?LCD_BLINKOFF,
	displayfunction = ?LCD_4BITMODE bor ?LCD_1LINE bor ?LCD_2LINE bor ?LCD_5x8DOTS,
	displaymode = ?LCD_ENTRYLEFT bor ?LCD_ENTRYSHIFTDECREMENT }).

init_button(Button) ->
	mcp:setup(Button, ?GPIO_IN),
	mcp:pullup(Button, 1).

init_pin(Pin) ->
	mcp:setup(Pin, ?GPIO_OUT)
.

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
	% Set LCD R/W pin to low for writing only.
	mcp:setup(?LCD_PLATE_RW, ?GPIO_OUT),
	mcp:output(?LCD_PLATE_RW, ?GPIO_LOW),
	% Set buttons as inputs with pull-ups enabled.
	[init_button(Button) || Button <- [?SELECT, ?RIGHT, ?DOWN, ?UP, ?LEFT]],
	% Setup all pins as outputs.
	[init_pin(Pin) || Pin <- [?LCD_PLATE_RS, ?LCD_PLATE_EN, ?LCD_PLATE_D4, ?LCD_PLATE_D5, ?LCD_PLATE_D6, ?LCD_PLATE_D7]],
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

	{ok, State}.

write_char(H) when H =:= 10 ->
	write8(?LCD_SETDDRAMADDR bor (0 + lcd_row_offset(2)));
write_char(H)  ->
	write8(H, 1).

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast({clear}, State) ->
	write8(?LCD_CLEARDISPLAY),
	wait(300),
	{noreply, State};

handle_cast({enable_display, Enable}, State) ->
	DisplayControl = flip(State#state.displaycontrol, ?LCD_DISPLAYON, Enable),
	write8(?LCD_DISPLAYCONTROL bor DisplayControl),
	{noreply, State#state{displaycontrol=DisplayControl}};

handle_cast({show_cursor, Show}, State) ->
	DisplayControl = flip(State#state.displaycontrol, ?LCD_CURSORON, Show),
	write8(?LCD_DISPLAYCONTROL bor DisplayControl),
	{noreply, State#state{displaycontrol=DisplayControl}};

handle_cast({message, Text}, State) ->
	[write_char(Ch) || Ch <- Text],
	{noreply, State};

handle_cast({set_cursor, Col, Row}, State) ->
	write8(?LCD_SETDDRAMADDR bor (Col + lcd_row_offset(Row))),
	{noreply, State};

handle_cast({blink, On}, State) ->
	DisplayControl = flip(State#state.displaycontrol, ?LCD_BLINKON, On),
	write8(?LCD_DISPLAYCONTROL bor DisplayControl),
	{noreply, State#state{displaycontrol=DisplayControl}};

handle_cast({move_left}, State) ->
	write8(?LCD_CURSORSHIFT bor ?LCD_DISPLAYMOVE bor ?LCD_MOVELEFT),
	{noreply, State};

handle_cast({move_right}, State) ->
	write8(?LCD_CURSORSHIFT bor ?LCD_DISPLAYMOVE bor ?LCD_MOVERIGHT),
	{noreply, State};

handle_cast({autoscroll, On}, State) ->
	DisplayMode = flip(State#state.displaymode, ?LCD_ENTRYSHIFTINCREMENT, On),
	write8(?LCD_ENTRYMODESET bor DisplayMode),
	{noreply, State#state{displaymode=DisplayMode}};

handle_cast({backlight, Off}, State) ->
	mcp:output_pins(#{ ?LCD_PLATE_RED => Off, ?LCD_PLATE_GREEN => Off, ?LCD_PLATE_BLUE => Off }),
	{noreply, State};

handle_cast({create_char, Pos, Char}, State) ->
    write8(?LCD_SETCGRAMADDR bor Pos bsl 3),
    [ write8(B, 1) || B <- tuple_to_list(Char) ],
    {noreply, State};

handle_cast({home}, State) ->
	write8(?LCD_CLEARDISPLAY),
	{noreply, State}.
	
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

flip(Value, Flag, Enable) when Enable =:= 1 ->
	Value bor Flag;
flip(Value, Flag, Enable) when Enable =:= 0 ->
	Value band bnot(Flag).

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
	gen_server:cast(?MODULE, {clear}).

enable_display(Enable) ->
	gen_server:cast(?MODULE, {enable_display, Enable}).

show_cursor(Show) ->
	gen_server:cast(?MODULE, {show_cursor, Show}).

blink(On) ->
	gen_server:cast(?MODULE, {blink, On}).

message(Text) ->
	gen_server:cast(?MODULE, {message, Text}).

home() ->
	gen_server:cast(?MODULE, {home}).

set_cursor(Col, Row) ->
	gen_server:cast(?MODULE, {set_cursor, Col, Row}).

move_left() ->
	gen_server:cast(?MODULE, {move_left}).

move_right() ->
	gen_server:cast(?MODULE, {move_right}).

autoscroll(On) ->
	gen_server:cast(?MODULE, {autoscroll, On}).

backlight(Off) ->
	gen_server:cast(?MODULE, {backlight, Off}).


%% @doc Fill one of the first 8 CGRAM locations with custom characters.
%% The location parameter should be between 0 and 7 and pattern should
%% provide an array of 8 bytes containing the pattern. E.g. you can easyly
%% design your custom character at http://www.quinapalus.com/hd44780udg.html
%% To show your custom character use eg. lcd.message('\x01')
%% @end
-spec create_char(integer(), tuple()) -> ok.
create_char(Pos, Char) when Pos < 8 andalso is_tuple(Char) andalso size(Char) =:= 8 ->
    gen_server:cast(?MODULE, {create_char, Pos, Char}).

wait(Ms) ->
	receive
	after Ms ->
		true
	end.
