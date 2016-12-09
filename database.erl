%%% @author Benjamin E. Holen <bholen01@rm121-01.eecs.tufts.edu>
%%% @copyright (C) 2016, Benjamin E. Holen
%%% @doc
%%% Module for handling DETS tables
%%% @end
%%% Created :  2 Dec 2016 by Benjamin E. Holen

-module(database).

% monitor funcs
-export([init_monitor_dets/0, add_client/1, lookup_monitor_ref/1, lookup_client/1, 
	remove_client_from_database/1, print_all/0, clear_table/0]).

% monitor_tcp_server funcs
-export([lookup_peers/2]).

% client funcs
-export([init_client_dets/0, add_file_to_table/1, lookup_file/1]).

open_table(monitor) ->
    dets:open_file('Database', [{file, 'Database.table'}]);

open_table(client) ->
    dets:open_file('Files', [{file, 'Files.table'}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MONITOR DATABASE FUNCS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_monitor_dets() ->
    open_table(monitor).

%%
%% An entry in the monitor's dets contains a client's account information.
%% Each entry consists of the client's node, a monitor reference (a unique
%% identifier returned by monitor(Pid) which the the monitor can use later 
%% on to stop monitoring the client), the pid of the server on the client's 
%% node, and the client's ip address and port.
%%
add_client(Entry = {_ClientNode, _MonitorRef, _NewClientServPid, _ClientIP, _ClientPort}) ->
    open_table(monitor),
    dets:insert('Database', Entry).
%%
%% looks up the monitor reference from a clientID
%%
lookup_monitor_ref(ClientNode) ->
    open_table(monitor),
    case  dets:lookup('Database', ClientNode) of
        [{ClientNode, MonitorRef,_,_,_}] -> MonitorRef;
	_ -> {error, notfound}
    end.
%%
%% Looks up the clientID from a monitor reference.
%% The monitor ref is pattern matched against all clients.
%% There should be only one client per monitor-ref.
%%
lookup_client(MonitorRef) ->
    open_table(monitor),
    case dets:match_object('Database', {'_', MonitorRef, '_', '_', '_'}) of
        [{ClientNode, MonitorRef, _, _, _}] -> ClientNode;
	_ -> {error, notfound}
    end.

remove_client_from_database(ClientNode) ->
    open_table(monitor),
    dets:delete('Database', ClientNode).

%%
%% Prints all entries in the database
%%
print_all() ->
    open_table(monitor),
    All = dets:foldr(fun (E = {_,_,_, _IP, _Port}, Acc) ->  [E|Acc] end, [], 'Database'),
    erlang:display(All).

clear_table() ->
    open_table(monitor),
    dets:match_delete('Database', {'_','_','_','_','_'}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MONITOR_TCP_SERVER DATABASE FUNCS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Returns all peers, self exclusive so that files can be transfered.
%% The dets is folded with a function that accumulates {IP, Port} tuples
%%
lookup_peers(_Self, ClientIP) ->
    open_table(monitor),
    Peers = dets:foldr(fun (_E = {_,_,_, IP, Port}, Acc) ->  [{IP, Port}|Acc] end, [], 'Database'),
    proplists:delete(ClientIP, Peers).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CLIENT TABLE FUNCS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_client_dets() ->
    open_table(client).

%%
%% Each client keeps a dets of their stored files
%% Ff a file is changed or deleted, the dets keeps track
%% of the previous hash so that the original file can still be
%% retreived.
%%
add_file_to_table(Entry = {_Filename, _Hash}) ->
    open_table(client),
    dets:insert('Files', Entry).

%
% Returns the hash for Filename, as it was when the file was uploaded
%
lookup_file(Filename) ->
    open_table(client),
    case  dets:lookup('Files', Filename) of
        [{Filename, Hash}] -> Hash;
	_ -> {error, notfound}
    end.

