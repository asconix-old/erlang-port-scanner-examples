-module(simple_scanner).
-export([
  scan_host/2, scan_host/3,
  scan_hosts/2, scan_hosts/3,
  ping/2
]).

incr(IPv4Addr) ->
	{Val1, Val2, Val3, Val4} = IPv4Addr,
	case Val4 < 255 of
		true -> Out = integer_to_list(Val1) ++ "." ++ integer_to_list(Val2) ++ "." ++ integer_to_list(Val3) ++ "." ++ integer_to_list(Val4 + 1);
		false -> case Val3 < 255 of
			true -> Out = integer_to_list(Val1) ++ "." ++ integer_to_list(Val2) ++ "." ++ integer_to_list(Val3+1) ++ ".0";
			false -> case Val2 < 255 of
				true -> Out = integer_to_list(Val1) ++ "." ++ integer_to_list(Val2+1) ++ ".0.0";
				false -> case Val1 < 255 of
					true -> Out = integer_to_list(Val1+1) ++ ".0.0.0";
					false -> Out = error
				end
			end
		end
	end,
	Out.

tokenize(IPv4Addr) ->
	[Octet1, Octet2, Octet3, Octet4] = string:tokens(IPv4Addr, "."),
    [{Val1,_},{Val2,_},{Val3,_},{Val4,_}] = [string:to_integer(Octet1), string:to_integer(Octet2), string:to_integer(Octet3), string:to_integer(Octet4)],
    Ip = {Val1, Val2, Val3, Val4},
    Ip.

% Base case reached
range(StartIp, StartIp) ->
	ok;

range(StartIp, EndIp) ->
	IPv4Int = tokenize(StartIp),
	range(incr(IPv4Int), EndIp).

%%% -----------------------------------
%%% One host, multiple ports
%%% -----------------------------------

% Base case, no more hosts to scan
scan_host(_Host, []) ->
 ok;

% Scan n ports of one host
scan_host(Host, [Port|Tail]) ->
  _ = spawn(fun() -> ping(Host, Port) end),
    scan_host(Host, Tail);

% Scan one port of one host
scan_host(Host, Port) ->
  scan_host(Host, [Port]).

% Scan port range of one host
scan_host(Host, StartPort, EndPort) ->
  scan_host(Host, lists:seq(StartPort, EndPort)).

%%% ------------------------------------
%%% Multiple hosts, one port
%%% ------------------------------------

% Base case, no more hosts to scan
scan_hosts([], _Host) ->
  ok;

% Scan n hosts for one port
scan_hosts([Host|Tail], Port) ->
  _ = spawn(fun() -> ping(Host, Port) end),
    scan_hosts(Tail, Port);

% Scan one host for one port
scan_hosts(Host, Port) ->
  scan_hosts([Host], Port).

% Scan a host range for one port
scan_hosts(StartHost, EndHost, Port) ->
  scan_hosts(lists:seq(StartHost, EndHost), Port).

% The scanner itself
ping(Host, Port) ->
 case (gen_tcp:connect(Host, Port, [binary, {packet, 0}], 1000)) of
   {ok, Socket} ->
     io:format("~p - Open~n", [Port]),
     {gen_tcp:close(Socket), {Port, open}};
   {error, _Reason} ->
     io:format("~p - Closed~n", [Port]),
     {ok, {Port, closed}};
   Error ->
     io:format("ERROR: ~p~n", [Error]),
     {error, Error}
 end.

