% The echo module provides a simple TCP echo server. Users can telnet
% into the server and the sever will echo back the lines that are input by the user.
-module(echo).
-export([accept/1]).

% Starts an echo server listening for incoming connections on the given Port.
accept(Port) ->
    {ok, Socket} = gen_tcp:listen(Port, [list, {active, true}, {packet, raw}, {reuseaddr, true}]),
    io:format("Echo server listening on port ~p~n", [Port]),
    server_loop(Socket).

% Accepts incoming socket connections and passes then off to a separate Handler process
server_loop(Socket) ->
    {ok, Connection} = gen_tcp:accept(Socket),
    Handler = spawn(fun () -> echo_loop(Connection) end),
    gen_tcp:controlling_process(Connection, Handler),
    io:format("New connection ~p~n", [Connection]),
    server_loop(Socket).

echo_loop(Connection) ->
    echo_loop(Connection, 0).

% Echoes the incoming lines from the given connected client socket
echo_loop(Connection, SerialNo) ->
    receive
        {tcp, Connection, Data} ->
            Data_regex = re:replace(Data, "\r\n", "", [global, {return, list}]),
            io:format("serialNo:~p, connection:~p, received:~p~n",[SerialNo, Connection, Data_regex]),
            Tokens = string:tokens(Data_regex, " "),
            io:format("~p~n", [Tokens]),
            case lists:nth(1, Tokens) of
                "file" -> 
                    Filename = lists:nth(2, Tokens),
                    case file:read_file(Filename) of
                        {ok, Binary} -> gen_tcp:send(Connection, erlang:binary_to_list(Binary));
                        _ -> gen_tcp:send(Connection, io_lib:format("failed to read file ~p\r\n",[Filename]))
                    end;
                _ -> 
                    gen_tcp:send(Connection, Data)
            end,
	        echo_loop(Connection, SerialNo + 1);
	    {tcp_closed, Connection} ->
	        io:format("Connection closed ~p~n", [Connection])
    end.