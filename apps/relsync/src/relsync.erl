%%   Copyright 2014 Frank Hunleth
%%   Copyright 2020 João Henrique Ferreira de Freitas
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.

%%% @doc
%%%  This is the script that runs the relsync logic on the host.
%%% @end
-module(relsync).

-export([
    main/1,
    init/1,
    update_nodes/2,
    setup_local_node/1
]).

-include("relsync.hrl").

-spec main([string()]) -> no_return().
main(CmdLine) ->
    Opts = relsync_cli:opts(),
    case getopt:parse(Opts, CmdLine) of
        {ok, {Options, []}} ->
            relsync_cli:do("relsync", Options, []);
        {error, Error} ->
            io:put_chars(standard_error, [getopt:format_error(Opts, Error), "\n\n"]),
            getopt:usage(relsync_cli:opts(), "relsync")
    end.

%% rebar3 plugin entry point
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    rebar3_relsync_prv:init(State).

%% Use the command line parameters to setup the local
%% Erlang instance to be able to talk to remote nodes.
-spec setup_local_node(config()) -> ok | {error, set_cookie} | {error, net_kernel, term()} .
setup_local_node(Config) ->
    % First, make sure that Epmd is running
    case net_adm:names() of
        %% Epmd is running
        {ok, _} ->
            ok;
        {error, address} ->
            Epmd = os:find_executable("epmd"),
            os:cmd(Epmd ++ " -daemon")
    end,

    % Next, start up net_kernel
    case net_kernel:start(options_to_netkernel(Config)) of
        {ok, _} ->
            case erlang:is_alive() of
                true ->
                    Cookie = Config#config.cookie,
                    erlang:set_cookie(node(), list_to_atom(Cookie)),
                    ok;
                false ->
                    {error, set_cookie}
            end;
        {error, {already_started, _Pid}} ->
            ok;
        {error, Reason} ->
            {error, net_kernel, Reason}
    end.

-spec options_to_netkernel(config()) -> [atom()] | no_return().
options_to_netkernel(Config) ->
    case Config#config.sname of
        undefined ->
            case Config#config.name of
                undefined ->
                    exit({badargs, "Specify --sname or --name"});
                Name ->
                    [list_to_atom(Name), longnames]
            end;
        SName ->
            [list_to_atom(SName), shortnames]
    end.

update_nodes(ssh, Config) ->
    ok = ssh:start(),
    DestNode = Config#config.destnode,
    update_node(ssh, [DestNode], Config);
update_nodes(erl, Config) ->
    % Only support one node for now. In theory, we could update
    % a swarm of devices. That would be wild.
    DestNode = Config#config.destnode,
    update_node(erl, [list_to_atom(DestNode)], Config).

update_node(ssh, [], _Config) ->
    ok;
update_node(ssh, [Node | T], Config) ->
    io:format("Updating ~p...~n", [Node]),

    % Gather our options
    {ok, UserDir} = file:get_cwd(),
    DestPath = normalize_path(Config#config.destpath),
    DestRwPath = normalize_path(Config#config.destrwpath),
    LocalPath = normalize_path(Config#config.localpath),
    Hooks = Config#config.hooks,

    Port = Config#config.port,

    {ok, ConRef} = relsync_ssh_client:connect(Node, Port, UserDir),

    {ok, Client} = relsync_ssh_client:start_link(ConRef),

    % Start syncing
    ok = relsync_ssh_client:set_hooks(Client, Hooks),
    ok = relsync_ssh_client:notify_presync(Client),

    DestPathToUse =
        case DestRwPath of
            "" ->
                DestPath;
            _ ->
                %ok = target_syncer:create_symlink_mirror(Node, DestPath, DestRwPath),
                DestRwPath
        end,
    {ok, DestFileInfos0} = relsync_ssh_client:get_file_listing(Client, DestPathToUse),
    LocalFileInfos0 = relsync_lib:get_file_listing(LocalPath),

    DestFileInfos = maybe_exclude_system_libs(DestFileInfos0, Config),
    LocalFileInfos = maybe_exclude_system_libs(LocalFileInfos0, Config),

    {ok, FileList} = synchronize_node(
        {relsync_ssh_client, Client},
        LocalPath,
        LocalFileInfos,
        DestPathToUse,
        DestFileInfos
    ),

    {ok, FilesReloaded} = relsync_ssh_client:reload(Client, FileList),

    print_files_reloaded(FilesReloaded),

    ok = relsync_ssh_client:notify_postsync(Client),

    ok = relsync_ssh_client:stop(Client),
    ok = relsync_ssh_client:close(ConRef),

    % Do the next node.
    update_node(erl, T, Config);
update_node(erl, [], _Config) ->
    ok;
update_node(erl, [Node | T], Config) ->
    % Ping the node to make sure that it is connected
    io:format("Updating ~p...~n", [Node]),
    pong = net_adm:ping(Node),

    % Start up the remote syncer
    case target_syncer_sup:start_child(Node) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,

    % Gather our options
    DestPath = normalize_path(Config#config.destpath),
    DestRwPath = normalize_path(Config#config.destrwpath),
    LocalPath = normalize_path(Config#config.localpath),
    Hooks = Config#config.hooks,

    % Start syncing
    ok = target_syncer:set_hooks(Node, Hooks),
    ok = target_syncer:notify_presync(Node),

    DestPathToUse =
        case DestRwPath of
            "" ->
                DestPath;
            _ ->
                ok = target_syncer:create_symlink_mirror(Node, DestPath, DestRwPath),
                DestRwPath
        end,
    DestFileInfos0 = target_syncer:get_file_listing(Node, DestPathToUse),
    LocalFileInfos0 = relsync_lib:get_file_listing(LocalPath),

    DestFileInfos = maybe_exclude_system_libs(DestFileInfos0, Config),
    LocalFileInfos = maybe_exclude_system_libs(LocalFileInfos0, Config),

    {ok, FileList} = synchronize_node(
        {target_syncer, Node},
        LocalPath,
        LocalFileInfos,
        DestPathToUse,
        DestFileInfos
    ),

    {ok, FilesReloaded} = target_syncer:reload(Node, FileList),

    print_files_reloaded(FilesReloaded),

    ok = target_syncer:notify_postsync(Node),

    % Do the next node.
    update_node(erl, T, Config).

-spec normalize_path(string()) -> string().
normalize_path("") ->
    "";
normalize_path(Path) ->
    case lists:last(Path) of
        $/ -> Path;
        _ -> Path ++ "/"
    end.

% Return true if this is a safe file to synchronize
-spec safe_file({string(), {integer(), binary()}}) -> true | false.
safe_file({Filename, _Info}) ->
    filename:extension(Filename) =/= ".so".

% Synchronize the nodes by taking the local and remote file
% lists, filtering and sorting them, and then comparing them
% one by one to make sure that both sides are in sync.
-spec synchronize_node(Node, LocalPath, LocalFileInfos, DestPath, DestFileInfos) -> {ok, list()} when
    Node :: {module(), pid() | atom()},
    LocalPath :: string(),
    LocalFileInfos :: [{string(), {integer(), binary()}}],
    DestPath :: string(),
    DestFileInfos :: [{string(), {integer(), binary()}}].
synchronize_node(Node, LocalPath, LocalFileInfos, DestPath, DestFileInfos) ->
    FilteredLocalInfos = lists:filter(fun safe_file/1, LocalFileInfos),
    SortedLocalInfos = lists:sort(FilteredLocalInfos),
    FilteredDestInfos = lists:filter(fun safe_file/1, DestFileInfos),
    SortedDestInfos = lists:sort(FilteredDestInfos),
    sync_files(
        Node,
        normalize_path(LocalPath),
        SortedLocalInfos,
        normalize_path(DestPath),
        SortedDestInfos,
        []
    ).

make_system_libs_regexp() ->
    Version = "-[[:digit:]{0,*}]?\.",
    SystemLibs = [
        asn1,
        common_test,
        compiler,
        crypto,
        debugger,
        dialyzer,
        diameter,
        edoc,
        eldap,
        erl_docgen,
        erl_interface,
        et,
        eunit,
        ftp,
        hipe,
        inets,
        jinterface,
        kernel,
        megaco,
        mnesia,
        observer,
        odbc,
        os_mon,
        parsetools,
        public_key,
        reltool,
        runtime_tools,
        sasl,
        snmp,
        ssh,
        ssl,
        stdlib,
        syntax_tools,
        tftp,
        tools,
        wx,
        xmerl
    ],

    [
        begin
            Regexp = [atom_to_list(Lib), Version],
            {ok, MP} = re:compile(Regexp),
            MP
        end
        || Lib <- SystemLibs
    ].

maybe_exclude_system_libs(FileInfos, #config{exclude_system_libs = true}) ->
    SystemLibsRegexp = make_system_libs_regexp(),
    Fun = fun({Path, _}) ->
        Match = lists:any(
            fun(Regexp) ->
                case re:run(Path, Regexp, [{capture, none}]) of
                    match ->
                        true;
                    nomatch ->
                        false
                end
            end,
            SystemLibsRegexp
        ),
        case Match of
            true ->
                % Path is a system_lib, so remove it from FileInfos
                false;
            false ->
                true
        end
    end,
    lists:filter(Fun, FileInfos);
maybe_exclude_system_libs(LocalFileInfos, #config{exclude_system_libs = false}) ->
    LocalFileInfos.

-spec sync_files(Node, LocalPath, LocalFiles, DestPath, DestFiles, AccFiles) -> {ok, list()} when
    Node :: {module(), pid() | atom()},
    LocalPath :: string(),
    LocalFiles :: [{string(), {integer(), binary()}}],
    DestPath :: string(),
    DestFiles :: [{string(), {integer(), binary()}}],
    AccFiles :: [string()].
sync_files(_Node, _LocalPath, [], _DestPath, [], AccFiles) ->
    {ok, AccFiles};
sync_files(Node, LocalPath, [{LocalFile, LocalInfo} | LTail], DestPath, [], AccFiles) ->
    io:format("Creating ~p on ~p...~n", [LocalFile, Node]),
    {ok, Contents} = file:read_file(LocalPath ++ LocalFile),
    {Mode, _} = LocalInfo,
    Path = DestPath ++ LocalFile,
    ok = copy_file(Node, Path, Mode, Contents),
    sync_files(Node, LocalPath, LTail, DestPath, [], [Path | AccFiles]);
sync_files(Node, LocalPath, [], DestPath, [{DestFile, _DestInfo} | DTail], AccFiles) ->
    io:format("Deleting ~p from ~p...~n", [DestFile, Node]),
    Path = DestPath ++ DestFile,
    rm_file(Node, Path),
    sync_files(Node, LocalPath, [], DestPath, DTail, [Path | AccFiles]);
sync_files(
    Node,
    LocalPath,
    [{LocalFile, LocalInfo} | LTail],
    DestPath,
    [{DestFile, DestInfo} | DTail],
    AccFiles
) when LocalFile =:= DestFile, LocalInfo =:= DestInfo ->
    sync_files(Node, LocalPath, LTail, DestPath, DTail, AccFiles);
sync_files(
    Node,
    LocalPath,
    [{LocalFile, LocalInfo} | LTail],
    DestPath,
    [{DestFile, DestInfo} | DTail],
    AccFiles
) when LocalFile =:= DestFile, LocalInfo =/= DestInfo ->
    io:format("Updating ~p on ~p...~n", [LocalFile, Node]),
    Path = LocalPath ++ LocalFile,
    {ok, Contents} = file:read_file(Path),
    {Mode, _} = LocalInfo,
    ok = copy_file(Node, DestPath ++ LocalFile, Mode, Contents),
    sync_files(Node, LocalPath, LTail, DestPath, DTail, [Path | AccFiles]);
sync_files(
    Node,
    LocalPath,
    [{LocalFile, LocalInfo} | LTail],
    DestPath,
    [{DestFile, _DestInfo} | DTail],
    AccFiles
) when LocalFile > DestFile ->
    io:format("Deleting ~p from ~p...~n", [DestFile, Node]),
    Path = DestPath ++ DestFile,
    rm_file(Node, Path),
    sync_files(Node, LocalPath, [{LocalFile, LocalInfo} | LTail], DestPath, DTail, [Path | AccFiles]);
sync_files(
    Node,
    LocalPath,
    [{LocalFile, LocalInfo} | LTail],
    DestPath,
    [{DestFile, DestInfo} | DTail],
    AccFiles
) when LocalFile < DestFile ->
    io:format("Creating ~p on ~p...~n", [LocalFile, Node]),
    Path = LocalPath ++ LocalFile,
    {ok, Contents} = file:read_file(Path),
    {Mode, _} = LocalInfo,
    ok = copy_file(Node, DestPath ++ LocalFile, Mode, Contents),
    sync_files(Node, LocalPath, LTail, DestPath, [{DestFile, DestInfo} | DTail], [Path | AccFiles]).

copy_file({Module, Node}, FilePath, Mode, Contents) ->
    Module:copy_file(Node, FilePath, Mode, Contents).

rm_file({Module, Node}, FilePath) ->
    Module:rm_file(Node, FilePath).

print_files_reloaded(FilesReloaded) ->
    [io:format("~-10s ~s~n", [Status, Path]) || {Path, Status} <- FilesReloaded].
