-module(egitd_acceptor_pool).

-behaviour(acceptor_pool).

-export([start_link/0,
         accept_socket/2]).

-export([init/1]).

%% public api

start_link() ->
    acceptor_pool:start_link({local, ?MODULE}, ?MODULE, []).

accept_socket(Socket, Acceptors) ->
    acceptor_pool:accept_socket(?MODULE, Socket, Acceptors).

%% acceptor_pool api

init([]) ->
    Conn = #{id => egitd_server,
             start => {egitd_server, [], []},
             grace => 5000}, % Give connections 5000ms to close before shutdown
    {ok, {#{}, [Conn]}}.
