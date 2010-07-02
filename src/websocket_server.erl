-module(websocket_server).
-behaviour(gen_server).
-export([start_link/1, start/1, stop/0]).

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

stop() ->
    catch(gen_server:call(?MODULE, stop)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {socket, connected, recv_buffer, listener_pid}).

init(Args) ->
    {ok, Listen} = gen_tcp:listen(1234, [{packet,0},
					 {reuseaddr,true},
					 {active, true}]),
    {ok, Socket} = gen_tcp:accept(Listen),
    State = #state{socket = Socket, 
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

handle_cast(Event, State) ->
    io:format("Unknown cast: ~p~n", [Event]),
    {noreply, State}.

%% When not connected, attempt a connection by doing a handshake
%% with the originator
handle_info({tcp, Socket, Data}, State) when (not State#state.connected) ->
    case try_handshake(Socket, Data) of
	true -> 
	    {noreply, State#state{ connected=true }};
	false -> State
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

handle_info(Event, State) ->
    io:format("Unknown info: ~p~n", [Event]),
    {noreply, State}.

code_change(oldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


try_handshake(Socket, Data) ->
    case Data of
	"GET /websession HTTP/1.1\r\nUpgrade: WebSocket\r\nConnection: Upgrade\r\nHost: localhost:1234\r\nOrigin: null\r\n\r\n"
	->
	    %% Valid handshake received, send handshake reply
	    io:format("Handshake received: ~p~n", [Data]),
	    Handshake = [
			 "HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
			 "Upgrade: WebSocket\r\n",
			 "Connection: Upgrade\r\n",
			 "WebSocket-Origin: null\r\n",
			 "WebSocket-Location: ws://localhost:1234/websession\r\n\r\n"
			],
	    gen_tcp:send(Socket, Handshake),
	    true;
	_ ->
	    false
    end.
    
%% wait(Socket, State) ->
%%     receive
%% 	{tcp, Socket, Data} ->
%% 	    io:format("Handshake: ~p~n", [Data]),
%% 	    Handshake =
%% 		[
%% 		 "HTTP/1.1 101 Web Socket Protocol Handshake\r\n",
%% 		 "Upgrade: WebSocket\r\n",
%% 		 "Connection: Upgrade\r\n",
%% 		 "WebSocket-Origin: null\r\n",
%% 		 "WebSocket-Location: ",
%% 		 "  ws://localhost:1234/websession\r\n\r\n"
%% 		],
%% 	    gen_tcp:send(Socket, Handshake),
%% 	    BrowserPid = self(),
%% 	    Pid = spawn_link(fun() -> proxy(BrowserPid, State) end),
%% 	    loop(zero, Socket, Pid);
%% 	Any ->
%% 	    io:format("??? Received:~p~n",[Any]),
%% 	    wait(Socket, State)
%%     end.


%% proxy(Browser, State0) ->

%%     receive
%% 	{browser, Browser, Msg} ->
%% 	    io:format("received: ~p~n", [Msg]);
%% 	{browser_closed, _Browser} ->
%% 	    ok;
%% 	X -> 
%% 	    io:format("Unknown message: ~p~n", [X]),
%% 	    proxy(Browser, State0)
%%     end.

%% loop(Buff, Socket, Pid) ->
%%     receive
%%  	{tcp, Socket, Data} ->
%%  	    handle_data(Buff, Data, Socket, Pid);
%%  	{tcp_closed, Socket} ->
%%  	    io:format("Connection closed from client~n"),
%%  	    Pid ! {browser_closed, self()};
%%  	{append, NewOps} ->
%%  	    Pid ! {append, self(), NewOps},
%%  	    loop(Buff, Socket, Pid);
%%  	{send, Data} ->
%%  	    gen_tcp:send(Socket, [0,Data,255]),
%%  	    loop(Buff, Socket, Pid);
%%  	Any ->
%%  	    io:format("Received:~p~n",[Any]),
%%  	    loop(Buff, Socket, Pid)
%%     end.

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
    

