-module(egitd_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, RepoDir} = application:get_env(egitd, repo_dir),
   	{ok, _} = ranch:start_listener(egitd, 100,
                                   ranch_tcp, [{port, 9418}],
                                   egitd_protocol, [RepoDir]),
    egitd_sup:start_link().

stop(_State) ->
    ok.
