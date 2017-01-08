-module(egitd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    Flags = #{strategy => rest_for_one},
    Pool = #{id => egitd_acceptor_pool,
             start => {egitd_acceptor_pool, start_link, []}},
    Socket = #{id => egitd_socket,
               start => {egitd_socket, start_link, []}},
    {ok, {Flags, [Pool, Socket]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
