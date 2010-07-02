-module(demo).
-compile(export_all).

start() ->
    websocket_server:start([]),
    pop:start(),
    websocket_server:set_listener(whereis(pop)).

stop() ->
    websocket_server:stop(),
    pop:stop().



    
