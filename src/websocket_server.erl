-module(websocket_server).
-behaviour(gen_server).
-export([start_link/1, 
	 start/1, 
	 stop/0,
	 set_listener/1,
	 send/1
	]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% TODO:
%% Support multiple client connections. See 
%% file://localhost/Users/bjnortier/Downloads/R13B04/kernel/gen_tcp.html?i=91#shutdown/2
%% for a description on how to have multiple workers on a socket

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 init/1,
	 terminate/2]).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Args) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

set_listener(ListenerPid) ->
    gen_server:cast(?MODULE, {set_listener, ListenerPid}).

send(Message) ->
    gen_server:cast(?MODULE, {send, Message}).

stop() ->
    catch(gen_server:call(?MODULE, stop)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {sockets, connected, recv_buffer, listener_pid}).

init(Args) ->
    State = #state{sockets = create_socket(), 
		   recv_buffer = zero,
		   connected = false,
		   listener_pid = proplists:get_value(listener_pid, Args)
		  },

    {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_call(stop, _From, State) ->
    {stop, normal, State};
handle_call(Event, _From, State) ->
    io:format("Unknown call: ~p~n", [Event]),
    {reply, [], State}.

handle_cast({set_listener, ListenerPid}, State) ->
    {noreply, State#state{ listener_pid = ListenerPid } };
handle_cast({send, Message}, State)  when State#state.connected ->
    {_Listen, Socket} = State#state.sockets,
    gen_tcp:send(Socket, [0,Message,255]),
    {noreply, State};
handle_cast({send, _Message}, State) when (not State#state.connected) ->
    {noreply, State};

handle_cast(Event, State) ->
    io:format("Unknown cast: ~p~n", [Event]),
    {noreply, State}.

%% When not connected, attempt a connection by doing a handshake
%% with the originator
handle_info({tcp, Socket, Data}, State) when (not State#state.connected) ->
    case try_handshake(Socket, Data) of
	true -> 
	    {noreply, State#state{ connected=true }};
	false -> 
	    io:format("Failed on receiving ~p", [{tcp, Socket, Data}]),
	    {noreply, State}
    end;

%% Handle the data, keeping the part of the buffer
%% that hasn't been processed for the next receied tcp
%% data
handle_info({tcp, Socket, Data}, State) when State#state.connected ->
    Buffer1 = handle_data(State#state.recv_buffer, 
			  Data, 
			  Socket,
			  State#state.listener_pid),
    {noreply, State#state{ recv_buffer = Buffer1 }};

handle_info({tcp_closed, Socket}, State) ->
    io:format("TCP connection closed by client. ~n"),
    ok = gen_tcp:close(Socket),
    timer:sleep(500),

    {Listen, _} = State#state.sockets,
    {ok, NewSocket} = gen_tcp:accept(Listen),

    State1 = State#state {sockets = {Listen, NewSocket},
			  recv_buffer = zero,
			  connected = false},
    {noreply, State1 };

handle_info(Event, State) ->
    io:format("Unknown info: ~p~n", [Event]),
    {noreply, State}.

code_change(oldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_socket() ->
    {ok, Listen} = gen_tcp:listen(1234, [{packet,0},
					 {reuseaddr,true},
					 {active, true}]),
    {ok, Socket} = gen_tcp:accept(Listen),
    {Listen, Socket}.
    

try_handshake(Socket, Data) ->
    case parse_handshake(Data) of
	{ok, Fields} ->
	    %% Valid handshake received, send handshake reply
	    io:format("Handshake received: ~p~n", [Fields]),
	    Handshake = [
			 "HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
			 "Upgrade: WebSocket\r\n",
			 "Connection: Upgrade\r\n",
			 "WebSocket-Origin: " ++ proplists:get_value("Origin", Fields) ++ "\r\n",
			 "WebSocket-Location: ws://localhost:1234/websession\r\n\r\n"
			],
	    gen_tcp:send(Socket, Handshake),
	    true;
	{error, _} ->
	    false
    end.
    
%%
%% Create messages from the received data, where each message
%% is delimited by 0 and 255
%%
handle_data(zero, [0|T], Socket, Listener) ->
    handle_data([], T, Socket, Listener);
handle_data(zero, [], _Socket, _Listener) ->
    zero;
handle_data(Message, [255|T], Socket, Listener) ->
    handle_message(lists:reverse(Message), Listener),
    handle_data(zero,T, Socket, Listener);
handle_data(L, [H|T], Socket, Listener) ->
    handle_data([H|L], T, Socket, Listener);
handle_data([], L, _Socket, _Listener) ->
    L.


handle_message(Message, Listener) when is_pid(Listener) ->
    io:format("Message received. Forwarding ~p~n", [Message]),
    Listener ! {ws_msg, Message};
handle_message(Message, _) ->
    io:format("Message received, but no listener registered: ~p~n", [Message]).
    

%%
%% Safari example: 
%% "GET /websession HTTP/1.1\r\nUpgrade: WebSocket\r\nConnection: Upgrade\r\nHost: localhost:1234\r\nOrigin: file://\r\nCookie: __utma=111872281.3829786060766035000.1243542332.1276267522.1276538034.43; __utmz=111872281.1276096495.39.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); __qca=P0-1885873285-1264527945225\r\n\r\n"
%% Chrome example: 
%% "GET /websession HTTP/1.1\r\nUpgrade: WebSocket\r\nConnection: Upgrade\r\nHost: localhost:1234\r\nOrigin: null\r\n\r\n"
parse_handshake(Data) ->
    case Data of
	"GET /websession HTTP/1.1\r\n" ++ Fields ->
	    {ok, parse_fields(Fields)};
	_ ->
	    {error, handshake_preamble}
    end.

-define(IS_WHITESPACE(C),
        (C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).

parse_fields(Fields) ->    
    parse_fields(string:tokens(Fields, "\r\n"), []).
    
parse_fields([NameValueString|Rest], Props) ->
    ColonPosition = string:chr(NameValueString, $:),
    Name = string:substr(NameValueString, 1, ColonPosition - 1),
    Value = string:substr(NameValueString, ColonPosition + 1, length(NameValueString) - ColonPosition),
    parse_fields(Rest, [{Name, string:strip(Value)} | Props]);
parse_fields([], Props) ->
    Props.

-ifdef(TEST).
     
parse_handshake_test_() ->
    [
     ?_assertMatch({ok, [{"Origin","file://"},
			 {"Host","localhost:1234"},
			 {"Connection","Upgrade"},
			 {"Upgrade","WebSocket"}]}, 
		   parse_handshake("GET /websession HTTP/1.1\r\nUpgrade: WebSocket\r\nConnection: Upgrade\r\nHost: localhost:1234\r\nOrigin: file://\r\n\r\n")),
     ?_assertMatch({error, handshake_preamble}, 
		   parse_handshake("GET foo"))
    ].

parse_fields_test_() ->
    Parsed = parse_fields("Upgrade: WebSocket\r\nConnection: Upgrade\r\nHost: localhost:1234\r\nOrigin: file://\r\nCookie: __utma=111872281.3829786060766035000.1243542332.1276267522.1276538034.43; __utmz=111872281.1276096495.39.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); __qca=P0-1885873285-1264527945225\r\n\r\n"),
    [
     ?_assertMatch("WebSocket", proplists:get_value("Upgrade", Parsed)),
     ?_assertMatch("Upgrade", proplists:get_value("Connection", Parsed)),
     ?_assertMatch("localhost:1234", proplists:get_value("Host", Parsed)),
     ?_assertMatch("file://", proplists:get_value("Origin", Parsed)),
     ?_assertMatch("__utma=111872281.3829786060766035000.1243542332.1276267522.1276538034.43; __utmz=111872281.1276096495.39.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); __qca=P0-1885873285-1264527945225", 
		   proplists:get_value("Cookie", Parsed))

    ].
    

-endif.
