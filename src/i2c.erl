-module(i2c).
-export([start/0, init/0, write_list/2]).

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
	loop(Port).

loop(Port) ->
	receive
		{write_list, From, Register, Value} ->
			port_control(Port, ?CMD_WRITE_BLOCK_DATA, [Register, Value]),
			From ! {self(), ok},
			loop(Port);

		{'EXIT', Port, Reason} ->
	    	io:format("~p ~n", [Reason]),
	    	exit(port_terminated)
	end.

write_list(Register, Value) ->
	i2c ! {write_list, self(), Register, Value},
	receive
		{_From, ok} -> ok
	end.