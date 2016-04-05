-module(i2c).
-export([start/0, init/0, write8/2, write_list/2, read8/1, test/0]).

-define(CMD_WRITE_BYTE_DATA,  1).
-define(CMD_READ_BYTE_DATA,   2).
-define(CMD_WRITE_BLOCK_DATA, 3).

start() ->
	case erl_ddll:load_driver("ebin", i2c_drv) of
		ok -> ok;
		{error, already_loaded} -> ok;
		{error, Message} -> exit(erl_ddll:format_error(Message))
	end,
	spawn(?MODULE, init, []).

init() ->
	register(i2c, self()),

	Port = open_port({spawn_driver, i2c_drv}, [use_stdio, binary]),

	loop(Port)
.

loop(Port) ->
	receive
		{write8, _From, Pin, Value} ->
			port_control(Port, ?CMD_WRITE_BYTE_DATA, [Pin, Value]),
			loop(Port);

		{write_list, From, Register, Value} ->
			port_control(Port, ?CMD_WRITE_BLOCK_DATA, [Register, Value]),
			From ! {self(), ok},
			loop(Port);

		{read8, From, Pin} ->
			<<Result:8>> = port_control(Port, ?CMD_READ_BYTE_DATA, [Pin]),
			From ! {self(), Result},
			loop(Port);

		{'EXIT', Port, Reason} ->
	    	io:format("~p ~n", [Reason]),
	    	exit(port_terminated)
	end.

write8(Pin, Value) ->
	i2c ! {write8, self(), Pin, Value}.

write_list(Register, Value) ->
	i2c ! {write_list, self(), Register, Value},
	receive
		{_From, ok} -> ok
	end.

read8(Pin) ->
	i2c ! {read8, self(), Pin},
	receive
		{_From, Result} ->
			Result
	end.

% 16#12 = GPIO
% 16#00 = IODIR
% 16#0C = GPPU

test() ->
	% MCP230xxBase
	write_list(16#00, [0, 0]),  % write_iodir
	write_list(16#0C, [0, 0]),  % write_gppu
	% Adafruit_CharLCDPlate
	write_list(16#00, [0, 0]),  % _mcp.setup(LCD_PLATE_RW, GPIO.OUT) (iodir)
	write_list(16#12, [0, 0]),  % _mcp.output(LCD_PLATE_RW, GPIO.LOW) (gpio)
	write_list(16#00, [1, 0]),  % _mcp.setup(button, GPIO.IN), SELECT
	write_list(16#0C, [1, 0]),  % _mcp.pullup(button, True), SELECT
	write_list(16#00, [3, 0]),  % right
	write_list(16#0C, [3, 0]),  % right
	write_list(16#00, [7, 0]),  % down
	write_list(16#0C, [7, 0]),  % down
	write_list(16#00, [15, 0]), % up
	write_list(16#0C, [15, 0]), % up
	write_list(16#00, [31, 0]), % left
	write_list(16#0C, [31, 0]), % left
	% Adafruit_CharLCD
	write_list(16#00, [31, 0]), % rs gpio.setup out
	write_list(16#00, [31, 0]), % en
	write_list(16#00, [31, 0]), % d4
	write_list(16#00, [31, 0]), % d5
	write_list(16#00, [31, 0]), % d6 
	write_list(16#00, [31, 0]), % d7
	% # Initialize the display.
	% self.write8(0x33)
	wait(10),
	write_list(16#12, [0, 0]),  % mode
	write_list(16#12, [0, 24]), % pins
	write_list(16#12, [0, 24]), % pulse
	wait(1),
	write_list(16#12, [0, 56]), % pulse
	wait(1),
	write_list(16#12, [0, 24]), % pulse
	wait(1),
	write_list(16#12, [0, 24]), % pins
	write_list(16#12, [0, 24]), % pulse
	wait(1),
	write_list(16#12, [0, 56]), % pulse
	wait(1),
	write_list(16#12, [0, 24]), % pulse
	wait(1),

	% self.write8(0x32)
	wait(10),
	write_list(16#12, [0, 24]), % mode
	write_list(16#12, [0, 24]), % pins
	write_list(16#12, [0, 24]), % pulse
	wait(1),
	write_list(16#12, [0, 56]), % pulse
	wait(1),
	write_list(16#12, [0, 24]), % pulse
	wait(1),
	write_list(16#12, [0, 8]),  % pins
	write_list(16#12, [0, 8]),  % pulse
	wait(1),
	write_list(16#12, [0, 40]), % pulse
	wait(1),
	write_list(16#12, [0, 8]),  % pulse
	wait(1),

	% self.write8(LCD_DISPLAYCONTROL | self.displaycontrol)
	wait(10),
	write_list(16#12, [0, 8]), % mode
	write_list(16#12, [0, 0]), % pins
	write_list(16#12, [0, 0]), % pulse
	wait(1),
	write_list(16#12, [0, 32]), % pulse
	wait(1),
	write_list(16#12, [0, 0]), % pulse
	wait(1),
	write_list(16#12, [0, 6]), % pins
	write_list(16#12, [0, 6]), % pulse
	wait(1),
	write_list(16#12, [0, 38]), % pulse
	wait(1),
	write_list(16#12, [0, 6]), % pulse
	wait(1),

	% self.write8(LCD_FUNCTIONSET | self.displayfunction)
	wait(10),
	write_list(16#12, [0, 6]), % mode
	write_list(16#12, [0, 8]), % pins
	write_list(16#12, [0, 8]), % pulse
	wait(1),
	write_list(16#12, [0, 40]), % pulse
	wait(1),
	write_list(16#12, [0, 8]), %pulse
	wait(1),
	write_list(16#12, [0, 2]), % pins
	write_list(16#12, [0, 2]), % pulse
	wait(1),
	write_list(16#12, [0, 34]), %pulse 
	wait(1),
	write_list(16#12, [0, 2]), % pulse
	wait(1),

	% self.write8(LCD_FUNCTIONSET | self.displayfunction)
	wait(10),
	write_list(16#12, [0, 2]), % mode
	write_list(16#12, [0, 0]), % pins
	write_list(16#12, [0, 0]), % pulse
	wait(1),
	write_list(16#12, [0, 32]), % pulse
	wait(1),
	write_list(16#12, [0, 0]), % pulse
	wait(1),
	write_list(16#12, [0, 12]), %pins
	write_list(16#12, [0, 12]), % pulse
	wait(1),
	write_list(16#12, [0, 44]), % pulse
	wait(1),
	write_list(16#12, [0, 12]), % pulse
	wait(1),

	% self.write8(LCD_CLEARDISPLAY)
	wait(10),
	write_list(16#12, [0, 12]), % mode
	write_list(16#12, [0, 0]), % pins
	write_list(16#12, [0, 0]), % pulse
	wait(1),
	write_list(16#12, [0, 32]), % pulse
	wait(1),
	write_list(16#12, [0, 0]), % pulse
	wait(1),
	write_list(16#12, [0, 16]), % pins
	write_list(16#12, [0, 16]), % pulse
	wait(1),
	write_list(16#12, [0, 48]), % pulse
	wait(1),
	write_list(16#12, [0, 16]), % pulse
	wait(1),

	% Adafruit_RGBCharLCD

	write_list(16#00, [31, 0]), % gpio.setup(red, GPIO.OUT)
	write_list(16#00, [31, 0]), % green
	write_list(16#00, [31, 0]), % blue
	write_list(16#12, [0, 16]), % self._gpio.output_pins(self._rgb_to_pins(initial_color))
	% show cursor and blink
% 	wait(1000),
% 	write_list(16#12, [0, 16]),
% 	write_list(16#12, [0, 0]),
% 	write_list(16#12, [0, 0]),
% 	wait(1),
% 	write_list(16#12, [0, 32]),
% 	wait(1),
% 	write_list(16#12, [0, 0]),
% 	wait(1),
% 	write_list(16#12, [0, 14]),
% 	write_list(16#12, [0, 14]),
% 	wait(1),
% 	write_list(16#12, [0, 46]),
% 	wait(1),
% 	write_list(16#12, [0, 14]),
% 	wait(1),
	% lcd blink (True)
% 	wait(1000),
% 	write_list(16#12, [0, 14]), % output(self._rs, char_mode)
% 	write_list(16#12, [0, 0]),  % # Write upper 4 bits.
% 	write_list(16#12, [0, 0]),  % pulse 1
% 	wait(1),
% 	write_list(16#12, [0, 32]), % pulse 2
% 	wait(1),
% 	write_list(16#12, [0, 0]),  % pulse 3
% 	wait(1),
% 	write_list(16#12, [0, 30]), % # Write lower 4 bits.
% 	write_list(16#12, [0, 30]), % pulse 1
% 	wait(1),
% 	write_list(16#12, [0, 62]), % pulse 2
% 	wait(1),
% 	write_list(16#12, [0, 30]),  % pulse 3
	wait(1)
.

wait(Ms) ->
	receive
	after Ms ->
		true
	end.
