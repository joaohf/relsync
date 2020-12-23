%%%-------------------------------------------------------------------
%%% File    : relsync_SUITE.erl
%%% Author  : João Henrique Ferreira de Freitas
%%% Description : Main relsync test cases
%%%
%%% Created : 2020-11-26T07:44:45+00:00
%%%-------------------------------------------------------------------
-module(relsync_SUITE).

%% Test server callbacks
-export([
    suite/0,
    all/0,
    groups/0,
    group/1,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    sync_release/0,
    sync_release/1,
    sync_release_calling_hooks/0,
    sync_release_calling_hooks/1,
    sync_release_new_applications/0,
    sync_release_new_applications/1,
    show_usage/0,
    show_usage/1,
    start_and_stop/0,
    start_and_stop/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
    [{timetrap, {seconds, 300}}].

init_per_suite(Config) ->
    % prepare release a
    {ok, RelAPath} = prepare_release(Config, "a"),

    % prepare release b
    {ok, RelBPath} = prepare_release(Config, "b"),

    % prepare release c
    {ok, RelCPath} = prepare_release(Config, "c"),

    % prepare release d
    {ok, RelDPath} = prepare_release(Config, "d"),

    {ok, RelsyncHookErl} = prepare_hook(Config, "relsync_hook"),

    Releases = [{a, RelAPath}, {b, RelBPath}, {c, RelCPath}, {d, RelDPath}],
    Hooks = [{default, RelsyncHookErl}],

    [{hooks, Hooks}, {releases, Releases} | Config].

end_per_suite(_Config) ->
    ok.

init_per_group(commands, Config) ->
    UserDir = proplists:get_value(priv_dir, Config),
    NoPubKeyDir = filename:join(UserDir, "nopubkey"),
    ok = file:make_dir(NoPubKeyDir),
    [{user_dir, NoPubKeyDir} | Config];
init_per_group(ssh, Config) ->
    Config0 = proplists:delete(args, Config),
    Args = "--mode ssh --destination-node localhost --port 2222 --user relsync ",
    [{args, Args} | Config0];
init_per_group(erl, Config) ->
    Config0 = proplists:delete(args, Config),
    NodeName = atom_to_list(make_node_name(foo)),
    Args = "--mode erl --sname relsync --destination-node " ++ NodeName ++ " --cookie magic ",
    [{args, Args} | Config0];
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [
        {start_stop, [], [
            start_and_stop
        ]},
        {integration, [], [
            {group, commands}
        ]},
        {commands, [], [
            show_usage,
            {group, ssh},
            {group, erl}
        ]},
        {ssh, [], [
            sync_release,
            sync_release_calling_hooks,
            sync_release_new_applications
        ]},
        {erl, [], [
            sync_release,
            sync_release_calling_hooks,
            sync_release_new_applications
        ]}
    ].

group(_) -> [].

all() ->
    [
        {group, start_stop},
        {group, integration}
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

start_and_stop() ->
    [{doc, "start and stop relsyncd application locally works"}].

start_and_stop(_Config) ->
    {ok, _} = application:ensure_all_started(relsyncd),

    ok = application:stop(relsyncd).

show_usage() ->
    [{doc, "Print the usage helper"}].

show_usage(_Config) ->
    ?assertMatch("Usage: relsync " ++ _, os:cmd(escript() ++ " -h")).

sync_release() ->
    [
        {doc,
            "Start an erlang node using release A, then use relsync"
            " to update the node release between B and C back to A. All"
            " releases have the same file struct and applications"}
    ].

sync_release(Config) ->
    ok = file:set_cwd(?config(user_dir, Config)),

    RelAPath = proplists:get_value(a, ?config(releases, Config)),
    RelBPath = proplists:get_value(b, ?config(releases, Config)),
    RelCPath = proplists:get_value(c, ?config(releases, Config)),

    % use release a
    {ok, DestDir} = enable_release(Config, RelAPath),

    Args = ?config(args, Config),
    ArgsA = Args ++ "--local-path " ++ RelAPath ++ " --destination-path " ++ DestDir,
    ArgsB = Args ++ "--local-path " ++ RelBPath ++ " --destination-path " ++ DestDir,
    ArgsC = Args ++ "--local-path " ++ RelCPath ++ " --destination-path " ++ DestDir,

    % start an erlang node using release A
    {ok, Node} = node_start(foo, DestDir),

    % check if secondary node is running release A
    {ok, a} = erpc:call(Node, fake, run, []),

    % call relsync to update files using release B
    ok = relsync:main(ArgsB),

    % check if secondary node is running release B
    {ok, b} = erpc:call(Node, fake, run, []),

    % call relsync to update files using release A
    ok = relsync:main(ArgsA),

    % check if secondary node is running release A
    {ok, a} = erpc:call(Node, fake, run, []),

    % call relsync to update files using release C, does not have fake module
    ok = relsync:main(ArgsC),

    % check if the old fake module is not in the remote node anymore
    case catch erpc:call(Node, fake, run, []) of
        {'EXIT', {{exception, undef, [{fake, run, [], []}]}, _}} ->
            ok
    end,

    % check if the new module is there
    {ok, c} = erpc:call(Node, fake2, run, []),

    % call relsync to update files using release A
    ok = relsync:main(ArgsA),

    % check if the fake module is back
    {ok, a} = erpc:call(Node, fake, run, []),

    % check if the old fake2 has gone
    case catch erpc:call(Node, fake2, run, []) of
        {'EXIT', {{exception, undef, [{fake2, run, [], []}]}, _}} ->
            ok
    end,

    ok = slave:stop(Node),

    % clenup destdir
    ec_file:remove(DestDir, [recursive]),

    ok.

sync_release_calling_hooks() ->
    [{doc, "Update a release using pre and post hooks."}].

sync_release_calling_hooks(Config) ->
    ok = file:set_cwd(?config(user_dir, Config)),

    RelsyncHookErl = proplists:get_value(default, ?config(hooks, Config)),

    RelAPath = proplists:get_value(a, ?config(releases, Config)),
    RelBPath = proplists:get_value(b, ?config(releases, Config)),

    % use release a
    {ok, DestDir} = enable_release(Config, RelAPath),

    Args = ?config(args, Config),
    ArgsWithHook = " --hooks " ++ RelsyncHookErl,
    ArgsA = Args ++ "--local-path " ++ RelAPath ++ " --destination-path " ++ DestDir,
    ArgsB = Args ++ "--local-path " ++ RelBPath ++ " --destination-path " ++ DestDir,

    % start an erlang node using release A
    {ok, Node} = node_start(foo, DestDir),

    % call relsync to update files using release B, with hooks
    ok = relsync:main(ArgsB ++ ArgsWithHook),

    % % check if secondary node is running release B
    {ok, b} = erpc:call(Node, fake, run, []),

    % % call relsync to update files using release A, no hooks
    ok = relsync:main(ArgsA),

    % % check if secondary node is running release A
    {ok, a} = erpc:call(Node, fake, run, []),
    case catch erpc:call(Node, foo, run, []) of
        {'EXIT', {{exception, undef, [{foo, run, [], []}]}, _}} ->
            ok
    end,

    ok = slave:stop(Node),

    % clenup destdir
    ec_file:remove(DestDir, [recursive]),

    ok.

sync_release_new_applications() ->
    [{doc, "Update a release adding a new application."}].

sync_release_new_applications(Config) ->
    ok = file:set_cwd(?config(user_dir, Config)),

    RelAPath = proplists:get_value(a, ?config(releases, Config)),
    RelDPath = proplists:get_value(d, ?config(releases, Config)),

    % use release a
    {ok, DestDir} = enable_release(Config, RelAPath),

    Args = ?config(args, Config),
    ArgsA = Args ++ "--local-path " ++ RelAPath ++ " --destination-path " ++ DestDir,
    ArgsD = Args ++ "--local-path " ++ RelDPath ++ " --destination-path " ++ DestDir,

    % start an erlang node using release A
    {ok, Node} = node_start(foo, DestDir),

    % call relsync to update files using release D which has a new application that
    % does not exist in release A
    ok = relsync:main(ArgsD),

    % check if secondary node is running release D and still have fake application
    % from release A
    {ok, a} = erpc:call(Node, fake, run, []),
    % the VM will load foo ondemand
    {ok, d} = erpc:call(Node, foo, run, []),

    % call relsync to update files using release A, no hooks
    ok = relsync:main(ArgsA),

    % check if secondary node is running release A
    {ok, a} = erpc:call(Node, fake, run, []),

    ok = slave:stop(Node),

    % clenup destdir
    ec_file:remove(DestDir, [recursive]),

    ok.

%%--------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------

escript() ->
    %% this relies on the _build structure rebar3 uses
    filename:join(code:lib_dir(relsync), "../../bin/relsync").

% compile fake application
prepare_fake_applications(Config, Name, BasePath) when Name == "a"; Name == "b" ->
    FakeModuleName = fake,
    FakeModuleBeam = "fake.beam",
    FileFake = filename:join([?config(data_dir, Config), Name, "fake_erl"]),
    FileFakeErl = filename:join([?config(data_dir, Config), Name, "fake.erl"]),
    FakeAppSrc = filename:join([?config(data_dir, Config), "fake.app"]),
    FakePath = filename:join([BasePath, "fake"]),
    FakeEbin = filename:join([FakePath, "ebin"]),
    FakeModule = filename:join([FakeEbin, FakeModuleBeam]),
    FakeAppDst = filename:join([FakeEbin, "fake.app"]),
    [ok = file:make_dir(Path) || Path <- [FakePath, FakeEbin]],
    {ok, _} = file:copy(FileFake, FileFakeErl),
    {ok, FakeModuleName, FakeBin} = compile:file(FileFakeErl, [binary]),
    ok = file:delete(FileFakeErl),
    ok = file:write_file(FakeModule, FakeBin),
    {ok, _} = file:copy(FakeAppSrc, FakeAppDst),
    ok;
prepare_fake_applications(Config, Name, BasePath) when Name == "c" ->
    FakeModuleName = fake2,
    FakeModuleBeam = "fake2.beam",
    FileFake = filename:join([?config(data_dir, Config), Name, "fake2_erl"]),
    FileFakeErl = filename:join([?config(data_dir, Config), Name, "fake2.erl"]),
    FakeAppSrc = filename:join([?config(data_dir, Config), "fake.app"]),
    FakePath = filename:join([BasePath, "fake"]),
    FakeEbin = filename:join([FakePath, "ebin"]),
    FakeModule = filename:join([FakeEbin, FakeModuleBeam]),
    FakeAppDst = filename:join([FakeEbin, "fake.app"]),
    [ok = file:make_dir(Path) || Path <- [FakePath, FakeEbin]],
    {ok, _} = file:copy(FileFake, FileFakeErl),
    {ok, FakeModuleName, FakeBin} = compile:file(FileFakeErl, [binary]),
    ok = file:delete(FileFakeErl),
    ok = file:write_file(FakeModule, FakeBin),
    {ok, _} = file:copy(FakeAppSrc, FakeAppDst),
    ok;
prepare_fake_applications(Config, Name, BasePath) when Name == "d" ->
    % make a/fake application
    NameA = "a",
    FakeModuleNameA = fake,
    FakeModuleBeamA = "fake.beam",
    FileFakeA = filename:join([?config(data_dir, Config), NameA, "fake_erl"]),
    FileFakeErlA = filename:join([?config(data_dir, Config), NameA, "fake.erl"]),
    FakeAppSrcA = filename:join([?config(data_dir, Config), "fake.app"]),
    FakePathA = filename:join([BasePath, "fake"]),
    FakeEbinA = filename:join([FakePathA, "ebin"]),
    FakeModuleA = filename:join([FakeEbinA, FakeModuleBeamA]),
    FakeAppDstA = filename:join([FakeEbinA, "fake.app"]),
    [ok = file:make_dir(Path) || Path <- [FakePathA, FakeEbinA]],
    {ok, _} = file:copy(FileFakeA, FileFakeErlA),
    {ok, FakeModuleNameA, FakeBinA} = compile:file(FileFakeErlA, [binary]),
    ok = file:delete(FileFakeErlA),
    ok = file:write_file(FakeModuleA, FakeBinA),
    {ok, _} = file:copy(FakeAppSrcA, FakeAppDstA),
    % make d/foo application
    FakeModuleNameD = foo,
    FakeModuleBeamD = "foo.beam",
    FileFakeD = filename:join([?config(data_dir, Config), Name, "foo_erl"]),
    FileFakeErlD = filename:join([?config(data_dir, Config), Name, "foo.erl"]),
    FakeAppSrcD = filename:join([?config(data_dir, Config), "foo.app"]),
    FakePathD = filename:join([BasePath, "foo"]),
    FakeEbinD = filename:join([FakePathD, "ebin"]),
    FakeModuleD = filename:join([FakeEbinD, FakeModuleBeamD]),
    FakeAppDstD = filename:join([FakeEbinD, "foo.app"]),
    [ok = file:make_dir(Path) || Path <- [FakePathD, FakeEbinD]],
    {ok, _} = file:copy(FileFakeD, FileFakeErlD),
    {ok, FakeModuleNameD, FakeBinD} = compile:file(FileFakeErlD, [binary]),
    ok = file:delete(FileFakeErlD),
    ok = file:write_file(FakeModuleD, FakeBinD),
    {ok, _} = file:copy(FakeAppSrcD, FakeAppDstD),
    ok.

% copy relsyncd application
prepare_relsyncd_application(_Config, _Name, BasePath) ->
    EbinSrc = code:lib_dir(relsyncd, ebin),
    RelsyncdPath = filename:join([BasePath, "relsyncd"]),
    RelsyncdEbin = filename:join([RelsyncdPath, "ebin"]),
    ok = ec_file:copy(EbinSrc, RelsyncdEbin, [recursive]).

prepare_relsync_lib_application(_Config, _Name, BasePath) ->
    EbinSrc = code:lib_dir(relsync_lib, ebin),
    RelsynAsnPath = filename:join([BasePath, "relsync_lib"]),
    RelsynAsnEbin = filename:join([RelsynAsnPath, "ebin"]),
    ok = ec_file:copy(EbinSrc, RelsynAsnEbin, [recursive]).

% add all application needed
prepare_release(Config, Name) ->
    BasePath = filename:join([?config(priv_dir, Config), Name]),
    ok = file:make_dir(BasePath),
    ok = prepare_fake_applications(Config, Name, BasePath),
    ok = prepare_relsyncd_application(Config, Name, BasePath),
    ok = prepare_relsync_lib_application(Config, Name, BasePath),
    {ok, BasePath}.

% copy ReleaseSource files to `rel' directory
enable_release(Config, ReleaseSource) ->
    DestDir = filename:join([?config(priv_dir, Config), "rel"]),
    ec_file:remove(DestDir, [recursive]),
    ok = ec_file:copy(ReleaseSource, DestDir, [recursive]),
    {ok, DestDir}.

prepare_hook(Config, Hook) ->
    HookFake = filename:join([?config(data_dir, Config), Hook ++ "_erl"]),
    FileErl = filename:join([?config(priv_dir, Config), Hook ++ ".erl"]),
    {ok, _} = file:copy(HookFake, FileErl),
    {ok, FileErl}.

make_node_args(DestDir) ->
    FakeEbin = filename:join([DestDir, "fake", "ebin"]),
    RelsyncdEbin = filename:join([DestDir, "relsyncd", "ebin"]),
    RelsyncAsnEbin = filename:join([DestDir, "relsync_lib", "ebin"]),
    lists:join(" ", ["-pa", FakeEbin, RelsyncdEbin, RelsyncAsnEbin]).

make_node_name(Name) ->
    {ok, Host} = inet:gethostname(),
    list_to_atom(lists:concat([Name, "@", Host])).

node_start(Name, DestDir) ->
    {ok, Host} = inet:gethostname(),
    {ok, Node} = slave:start(Host, Name, make_node_args(DestDir)),

    % start relsyncd application
    {ok, [crypto, asn1, public_key, ssh, relsync_lib, relsyncd]} = erpc:call(
        Node,
        application,
        ensure_all_started,
        [relsyncd]
    ),

    {ok, Node}.
