-module(demo).
-compile(export_all).

start() ->
    websocket_server:start([{population_limit, 100}]),
    pop:start().


stop() ->
    websocket_server:stop(),
    pop:stop().



    
