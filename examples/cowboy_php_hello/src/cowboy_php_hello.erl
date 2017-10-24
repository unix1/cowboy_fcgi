-module(cowboy_php_hello).

-export([start/2]).
-export([start_with_phpfpm/2]).
-export([stop/0]).

-spec start(pos_integer(), pos_integer()) -> {ok, pid()}.
start(CowboyPort, PhpFpmPort) ->
    PrefixDir = get_prefix_dir(),
    % start fcgi client
    {ok, _} = ex_fcgi:start('php-fpm', localhost, PhpFpmPort),
    % start cowboy_fcgi
    {ok, _} = start_cowboy_fcgi(CowboyPort, PrefixDir).

-spec start_with_phpfpm(pos_integer(), pos_integer()) -> {ok, pid()}.
start_with_phpfpm(CowboyPort, PhpFpmPort) ->
    PrefixDir = get_prefix_dir(),
    % start php-fpm
    ok = start_phpfpm(PrefixDir),
    start(CowboyPort, PhpFpmPort).

-spec stop() -> ok.
stop() ->
    % stop cowboy listener
    ok = cowboy:stop_listener(fcgi),
    % stop fcgi client
    ok = ex_fcgi:stop('php-fpm'),
    PidFile = get_pid_file(),
    {ok, KillPath, _PhpFpmPath} = get_exe_paths(),
    {ok, Pid} = file:read_file(PidFile),
    KillCmd = "\"" ++ KillPath ++ "\" \"" ++ binary_to_list(Pid) ++ "\"",
    io:format("kill command: ~p~n", [KillCmd]),
    % stop php-fpm processes
    KillOut = os:cmd(KillCmd),
    io:format("kill output: ~p~n", [KillOut]),
    ok.

%% Private functions

-spec start_phpfpm(string()) -> ok.
start_phpfpm(PrefixDir) ->
    {ok, _KillPath, PhpFpmPath} = get_exe_paths(),
    % priv/conf/php-fpm.conf
    ConfigFile = get_config_file(),
    PidFile = get_pid_file(),
    PhpFpmCmd = get_phpfpm_cmd(PhpFpmPath, ConfigFile, PrefixDir, PidFile),
    io:format("php-fpm command: ~p~n", [PhpFpmCmd]),
    PhpFpmOut = os:cmd(PhpFpmCmd),
    io:format("php-fpm output: ~p~n", [PhpFpmOut]),
    ok.

-spec start_cowboy_fcgi(pos_integer(), string()) -> {ok, pid()}.
start_cowboy_fcgi(TcpPort, ScriptDir) ->
    Opts = [{name, 'php-fpm'}, {script_dir, ScriptDir}],
    Dispatch = cowboy_router:compile([
        {'_', [
            {'_', cowboy_http_fcgi, Opts}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(fcgi,
        [{port, TcpPort}],
        #{env => #{dispatch => Dispatch}}
    ).

-spec get_exe_paths() -> {ok, string(), string()} | {error, string()}.
get_exe_paths() ->
    case {os:find_executable("kill"), os:find_executable("php-fpm")} of
        {false, _} ->
            {error, "kill not found in path"};
        {_, false} ->
            {error, "php-fpm not found in path"};
        {KillPath, PhpFpmPath} ->
            {ok, KillPath, PhpFpmPath}
    end.

-spec get_phpfpm_cmd(string(), string(), string(), string()) -> string().
get_phpfpm_cmd(PhpFpmPath, ConfigFile, PrefixDir, PidFile) ->
    "\"" ++ PhpFpmPath ++ "\" -y \"" ++ ConfigFile ++ "\" -g \"" ++ PidFile ++ "\" -p \"" ++ PrefixDir ++ "\"".

-spec get_pid_file() -> string().
get_pid_file() ->
    get_priv_dir() ++ "/php-fpm.pid".

-spec get_config_file() -> string().
get_config_file() ->
    get_config_dir() ++ "/php-fpm.conf".

-spec get_config_dir() -> string().
get_config_dir() ->
    get_priv_dir() ++ "/conf".

-spec get_prefix_dir() -> string().
get_prefix_dir() ->
    get_priv_dir() ++ "/www".

-spec get_priv_dir() -> string().
get_priv_dir() ->
    {ok, Application} = application:get_application(),
    code:priv_dir(Application).
