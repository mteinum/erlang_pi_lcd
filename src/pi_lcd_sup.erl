-module(pi_lcd_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link(pi_lcd_sup, []).

init(_Args) ->
	Flags = #{strategy => one_for_all, intensity => 1, period => 5},
	ChildSpecs = [#{id => i2c,
	                start => {i2c, start_link, []},
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
	              #{id => pi_lcd,
	                start => {pi_lcd, start_link, []},
	                restart => permanent,
	                shutdown => brutal_kill,
	                type => worker,
	                modules => [pi_lcd]
	              }],
	{ok, {Flags, ChildSpecs}}
.