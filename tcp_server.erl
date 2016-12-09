-module(tcp_server).
-behavior(gen_server).

-import(file_proc, [parse_packet/1, write_peer_file/2, read/1]).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {socket}).

%%
%% Start a TCP server
%%
start_link(Socket) ->

    % server name is ?MODULE
    gen_server:start_link(?MODULE, Socket, []).


%%
%% Initialize a TCP server
%%
init(Socket) ->

    %% Start accepting requests
    %% We must cast this to the worker's process, as it blocks it.
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket}}.


%%
%% Stop a TCP server
%%
stop() ->
    gen_server:cast(?MODULE, stop).

%%
%% Handle a connection
%%
handle_cast(accept, State = #state{socket=ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    tcp_sup:start_socket(),
    {noreply, State#state{socket=AcceptSocket}};
handle_cast(_, State) ->
    {noreply, State}.

%%
%% Handle peer packets
%%
handle_info({tcp, Socket, <<"quit", _/binary>>}, State) ->
    gen_tcp:close(Socket),
    {stop, normal, State};
handle_info({tcp, Socket, Packet}, State) ->
    open_packet(Socket, binary_to_term(Packet)),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) -> {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) -> {stop, normal, State};
handle_info(E, State) ->
    io:fwrite("unexpected: ~p~n", [E]),
    {noreply, State}.

%%
%% Not implemented
%%
handle_call(_E, _From, State) -> {noreply, State}.
terminate(_Reason, _Tab) -> ok.
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.


%%
%% Handle upload request from a peer
%%
open_packet(_Socket, {upload, Filename, _Hash, Content}) ->
    write_peer_file(filename:basename(Filename), Content);

%%
%% Handle download request from a peer
%%
open_packet(Socket, {download, _Filename, Hash}) ->

    case file:list_dir("peer_files") of
        {ok, Files} ->
            Hashes = lists:map(fun(File) -> crypto:hash(md5, read(filename:join(["./peer_files", File]))) end, Files),
            FileIndex = lists:zip(Hashes, Files),
            case lists:keyfind(Hash, 1, FileIndex) of
                {Hash, File} -> 
                    Packet = read(filename:join(["./peer_files", File])),
                    gen_tcp:send(Socket, Packet);
                false -> 
                    %Packet = term_to_binary({Hash, "Error: file not found"}),
                    Packet = "Error: file not found",
                    gen_tcp:send(Socket, Packet)
            end;
        {error, _} ->
            Packet = "Error: no files uploaded yet",
            gen_tcp:send(Socket, Packet)
    end.