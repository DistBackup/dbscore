-module(start_monitor).
-export([start/0]).
-import(monitor, [init_monitor/1]).


start() -> monitor:init_monitor([]).