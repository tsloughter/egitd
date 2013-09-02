-module(egitd_protocol).

-behaviour(gen_server).

-export([start_link/4]).

-export([init/4
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

-record(state, {socket
               ,port
               ,transport
               ,repo_dir
               }).


start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%% ------------------------------------------------------------------

init(Ref, Socket, Transport, Opts) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [], #state{socket=Socket, transport=Transport, repo_dir=Opts}).

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {data, Data}}, State=#state{socket=Socket
                                              ,port=Port
                                              ,transport=Transport}) ->
    Transport:send(Socket, Data),
    {noreply, State};
handle_info({_SocketType, Socket, Packet}, State=#state{socket=Socket
                                                       ,port=Port
                                                       ,transport=Transport}) when is_port(Port) ->
    port_command(Port, Packet),
    Transport:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({_SocketType, Socket, <<_Length:4/binary, "git", _:1/binary, "upload-pack", Other/binary>>}
           ,State=#state{socket=Socket
                        ,transport=Transport
                        ,repo_dir=RepoDir}) ->
    [Path, <<"host=", Host/binary>>, <<>>] = binary:split(Other, <<0>>, [global]),
    case upload_pack(Host, Path, Socket, Transport, RepoDir) of
        {error, Reason} ->
            {stop, {error, Reason}, State};
        {ok, Port} ->
            {noreply, State#state{port=Port}}
    end;
handle_info({_SocketType, Socket, <<_Length:4/binary, "git", _:1/binary, "receive-pack", Other/binary>>}
           ,State=#state{socket=Socket
                        ,transport=Transport}) ->
    [Path, <<"host=", Host/binary>>, <<>>] = binary:split(Other, <<0>>, [global]),
    send_error(Socket, Transport, receive_pack_error(Host, Path)),
    {stop, normal, State};
handle_info({_SocketType, Socket, _Packet}, State=#state{socket=Socket
                                                        ,transport=Transport}) ->
    send_error(Socket, Transport, invalid_method_error()),
    Transport:close(Socket),
    {stop, normal, State};
handle_info({tcp_closed, Socket}, State=#state{socket=Socket}) ->
    {stop, normal, State#state{socket=undefined}};
handle_info({Port, {exit_status, 0}}, State=#state{port=Port}) ->
    {stop, normal, State}.

terminate({error, Reason}, #state{socket=Socket, transport=Transport}) ->
    send_error(Socket, Transport, Reason),
    Transport:close(Socket),
    ok;
terminate(_Reason, #state{socket=undefined}) ->
    ok;
terminate(_Reason, #state{socket=Socket, transport=Transport}) ->
    Transport:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

upload_pack(Host, Path, Socket, Transport, RepoDir) ->
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
                    Transport:setopts(Socket, [{active, once}]),
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

send_error(Socket, Transport, Error) ->
    FlatError = list_to_binary(Error),
    ErrorMsg = io_lib:format("~4.16.0B~s", [byte_size(FlatError)+4, FlatError]),
    Transport:send(Socket, ErrorMsg).

receive_pack_error(Host, Path) ->
    SSHPath = [":", binary:part(Path, {1, byte_size(Path) -1})],
    ["\n*********\n\nYou can't push to git://", Host, Path, "\nUse git@", Host, SSHPath, "\n\n*********"].

invalid_method_error() ->
    "\n*********\n\nInvalid method declaration. Upgrade to the latest git.\n\n*********'".

no_matching_repo_error(Host, Path) ->
    io_lib:format("\n*********\n\nNo matching repositories found for git://~s~s.\n\n*********", [Host, Path]).

permission_denied_error(Host, Path) ->
    io_lib:format("\n*********\n\nPermission denied. Repository git://~s~s is not public.\n\n*********", [Host, Path]).
