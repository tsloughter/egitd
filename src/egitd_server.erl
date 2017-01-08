-module(egitd_server).

-behaviour(acceptor).
-behaviour(gen_server).

%% acceptor api
-export([acceptor_init/3,
         acceptor_continue/3,
         acceptor_terminate/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {socket, port, ref, repo_dir}).

acceptor_init(_SockName, LSocket, []) ->
    % monitor listen socket to gracefully close when it closes
    MRef = monitor(port, LSocket),
    {ok, MRef}.

acceptor_continue(_PeerName, Socket, MRef) ->
    {ok, RepoDir} = application:get_env(egitd, repo_dir),
    io:format("REPODIR ~p~n", [RepoDir]),
    gen_server:enter_loop(?MODULE, [], #state{socket=Socket, ref=MRef, repo_dir=RepoDir}).

acceptor_terminate(Reason, _) ->
    % Something went wrong. Either the acceptor_pool is terminating or the
    % accept failed.
    exit(Reason).

%% gen_server api

init(_) ->
    {stop, acceptor}.

handle_call(Req, _, State) ->
    {stop, {bad_call, Req}, State}.

handle_cast(Req, State) ->
    {stop, {bad_cast, Req}, State}.

handle_info({Port, {data, Data}}, State=#state{socket=Socket
                                              ,port=Port}) ->
    gen_tcp:send(Socket, Data),
    {noreply, State};
handle_info({_SocketType, Socket, Packet}, State=#state{socket=Socket
                                                       ,port=Port}) when is_port(Port) ->
    port_command(Port, Packet),
    gen_tcp:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({_SocketType, Socket, <<_Length:4/binary, "git", _:1/binary, "upload-pack", Other/binary>>}
           ,State=#state{socket=Socket
                        ,repo_dir=RepoDir}) ->
    [Path, <<"host=", Host/binary>>, <<>>] = binary:split(Other, <<0>>, [global]),
    case upload_pack(Host, Path, Socket, RepoDir) of
        {error, Reason} ->
            {stop, {error, Reason}, State};
        {ok, Port} ->
            {noreply, State#state{port=Port}}
    end;
handle_info({_SocketType, Socket, <<_Length:4/binary, "git", _:1/binary, "receive-pack", Other/binary>>}
           ,State=#state{socket=Socket}) ->
    [Path, <<"host=", Host/binary>>, <<>>] = binary:split(Other, <<0>>, [global]),
    send_error(Socket, receive_pack_error(Host, Path)),
    {stop, normal, State};
handle_info({_SocketType, Socket, _Packet}, State=#state{socket=Socket}) ->
    send_error(Socket, invalid_method_error()),
    gen_tcp:close(Socket),
    {stop, normal, State};
handle_info({tcp_error, Socket, Reason}, State=#state{socket=Socket}) ->
    {stop, Reason, State};
handle_info({tcp_closed, Socket}, State=#state{socket=Socket}) ->
    {stop, normal, State};
handle_info({'DOWN', MRef, port, _, _}, State=#state{socket=Socket,
                                                     ref=MRef}) ->
    %% Listen socket closed, receive all pending data then stop. In more
    %% advanced protocols will likely be able to do better.
    lager:info("Gracefully closing ~p~n", [Socket]),
    {stop, flush_socket(Socket), State};
handle_info(_, State) ->
    {noreply, State}.

terminate({error, Reason}, #state{socket=Socket}) ->
    send_error(Socket, Reason),
    gen_tcp:close(Socket),
    ok;
terminate(_Reason, #state{socket=Socket}) ->
    gen_tcp:close(Socket),
    ok.

%% @private
-spec code_change(term() | {down, term()}, #state{}, term()) ->
    {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

upload_pack(Host, Path, Socket, RepoDir) ->
    FullPath = filename:join([RepoDir | tl(filename:split(Path))]),
    case repo_existance(FullPath) of
        false ->
            lager:info("no repo match: ~p~n", [Path]),
            {error, no_matching_repo_error(Host, Path)};
        RealPath ->
            GitDaemonExportOkFilePath = filename:join([RealPath, "git-daemon-export-ok"]),
            case filelib:is_regular(GitDaemonExportOkFilePath) of
                true ->
                    %% all validated, yay
                    Port = make_port(Socket, <<"upload-pack">>, Host, Path, RealPath),
                    gen_tcp:setopts(Socket, [{active, once}]),
                    {ok, Port};
                false ->
                    lager:info("permission denied to repo: ~p~n", [RealPath]),
                    {error, permission_denied_error(Host, Path)}
            end
    end.

repo_existance(Path) ->
    case filelib:is_dir(Path) of
        true ->
            Path;
        _ ->
            case filelib:is_dir(<<Path/binary, ".git">>) of
                true ->
                    <<Path/binary, ".git">>;
                _ ->
                    false
            end
    end.

make_port(_Socket, Method, _Host, _Path, FullPath) ->
    Command = binary_to_list(<<"git ", Method/binary, " ", FullPath/binary>>),
    open_port({spawn, Command}, [binary, exit_status]).

send_error(Socket, Error) ->
    FlatError = list_to_binary(Error),
    ErrorMsg = io_lib:format("~4.16.0B~s", [byte_size(FlatError)+4, FlatError]),
    gen_tcp:send(Socket, ErrorMsg).

receive_pack_error(Host, Path) ->
    SSHPath = [":", binary:part(Path, {1, byte_size(Path) -1})],
    ["\n*********\n\nYou can't push to git://", Host, Path, "\nUse git@", Host, SSHPath, "\n\n*********"].

invalid_method_error() ->
    "\n*********\n\nInvalid method declaration. Upgrade to the latest git.\n\n*********'".

no_matching_repo_error(Host, Path) ->
    io_lib:format("\n*********\n\nNo matching repositories found for git://~s~s.\n\n*********", [Host, Path]).

permission_denied_error(Host, Path) ->
    io_lib:format("\n*********\n\nPermission denied. Repository git://~s~s is not public.\n\n*********", [Host, Path]).

flush_socket(Socket) ->
    receive
        {tcp, Socket, Data}         -> flush_send(Socket, Data);
        {tcp_error, Socket, Reason} -> Reason;
        {tcp_closed, Socket}        -> normal
    after
        0                           -> normal
    end.

flush_send(Socket, Data) ->
    case gen_tcp:send(Socket, Data) of
        ok              -> flush_recv(Socket);
        {error, closed} -> normal;
        {error, Reason} -> Reason
    end.

flush_recv(Socket) ->
    case gen_tcp:recv(Socket, 0, 0) of
        {ok, Data}       -> flush_send(Socket, Data);
        {error, timeout} -> normal;
        {error, closed}  -> normal;
        {error, Reason}  -> Reason
end.
