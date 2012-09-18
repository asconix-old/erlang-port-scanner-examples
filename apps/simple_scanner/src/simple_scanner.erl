-module(simple_scanner).
-export([
  scan_host/2, scan_host/3,
  scan_hosts/2, scan_hosts/3,
  ping/2
]).

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

