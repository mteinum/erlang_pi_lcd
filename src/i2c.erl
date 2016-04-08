-module(i2c).
-behaviour(gen_server).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start_link/1, init/1, write_list/2]).

-define(CMD_WRITE_BLOCK_DATA, 3).
-define(CMD_OPEN, 4).

start_link(Device) ->
	case erl_ddll:load_driver("ebin", i2c_drv) of
		ok -> ok;
		{error, already_loaded} -> ok;
		{error, Message} -> exit(erl_ddll:format_error(Message))
	end,
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Device], []).

init(Device) ->
	Port = open_port({spawn_driver, i2c_drv}, [use_stdio, binary]),
	port_control(Port, ?CMD_OPEN, Device),
	{ ok, Port }.

handle_call({write_list, Register, Value}, _From, State) ->
	port_control(State, ?CMD_WRITE_BLOCK_DATA, [Register, Value]),
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.	

write_list(Register, Value) ->
	gen_server:call(?MODULE, {write_list, Register, Value}).
