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
            io:format("tokens:~p~n",[Tokens]),
            case proc(Tokens) of
                {read_file, FileResultList} ->
                    % case Result of
                    %     {ok, Binary} -> gen_tcp:send(Connection, erlang:binary_to_list(Binary));
                    %     _ -> gen_tcp:send(Connection, io_lib:format("failed. cannot read file ~p\r\n",[Filename]))
                    % end;
                    lists:foreach(fun({Filename, Result}) ->
                        case Result of
                            {ok, Binary} -> gen_tcp:send(Connection, erlang:binary_to_list(Binary));
                            _ -> gen_tcp:send(Connection, io_lib:format("failed. cannot read file ~p\r\n",[Filename]))
                        end
                    end, FileResultList);
                {echo} -> 
                    gen_tcp:send(Connection, Data);
                {error, Reason} ->
                    gen_tcp:send(Connection, io_lib:format("failed. ~p\r\n",[Reason]));
                _ ->
                    gen_tcp:send(Connection, io_lib:format("failed. unknown error with data ~p\r\n",[Data]))
            end,
	        echo_loop(Connection, SerialNo + 1);
	    {tcp_closed, Connection} ->
	        io:format("Connection closed ~p~n", [Connection])
    end.

proc(Input) ->
    proc(Input, {}, 0).

proc([], Status, Depth) ->
    io:format("depth:~p, final status:~p~n",[Depth, Status]),
    case Status of
        {read_file, FilenameList} ->
            {read_file, lists:map(
                fun(Filename) -> { Filename, file:read_file(Filename) } end, FilenameList)};
        {read_file} ->
            {error, need_more_args};
        {echo} ->
            {echo};
        _ ->
            {error, wrong_input, Status}
    end;
proc([H | T], Status, Depth) ->
    io:format("depth:~p, current token:~p, current status:~p~n",[Depth, H, Status]),
    case Status of
        {read_file} ->  
            proc(T, {read_file, [H]}, Depth + 1);
        {read_file, L} ->
            proc(T, {read_file, [H | L]}, Depth + 1);
        _ ->
            case H of
                "file" -> proc(T, {read_file}, Depth + 1);
                _ -> proc(T, {echo}, Depth + 1)
            end
    end.
