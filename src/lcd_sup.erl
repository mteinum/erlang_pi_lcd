-module(lcd_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link(lcd_sup, []).

init(_Args) ->
	{ok, Device} = application:get_env(lcd_app, i2c_device),
	Flags = #{strategy => one_for_all, intensity => 1, period => 5},
	ChildSpecs = [#{id => i2c,
	                start => {i2c, start_link, [Device]},
	                restart => permanent,
	                shutdown => brutal_kill,
	                type => worker,
	                modules => [i2c]
	                },
	              #{id => mcp,
	                start => {mcp, start_link, []},
	                restart => permanent,
	                shutdown => brutal_kill,
	                type => worker,
	                modules => [mcp]
	                },
	              #{id => lcd,
	                start => {lcd, start_link, []},
	                restart => permanent,
	                shutdown => brutal_kill,
	                type => worker,
	                modules => [lcd]
	              }],
	{ok, {Flags, ChildSpecs}}
.