-module(pi_lcd_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
	pi_lcd_sup:start_link().

stop(_State) ->
	ok.