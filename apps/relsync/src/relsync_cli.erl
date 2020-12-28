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

-module(relsync_cli).

-export([
    do/3,
    opts/0
]).

-include("relsync.hrl").

-type parsed() ::
    {undefined, config()} | {ssh, config()} | {erl, config()} | help | version | {error, string()}.

-spec opts() -> [getopt:option_spec()].
opts() ->
    [
        %% {Name,     ShortOpt, LongOpt,                ArgSpec,                HelpMsg}
        {destnode, $d, "destination-node", {string, "node@other"}, "Destination node"},
        {destpath, $p, "destination-path", {string, "/srv/erlang"},
            "Path to release on the destination (Can be on a read-only filesystem)"},
        {destrwpath, $q, "destination-rw-path", {string, ""},
            "Path to writable location on the destination (empty if not needed)"},
        {localpath, $l, "local-path", {string, "./_rel"}, "Path to local release"},
        {hooks, $H, "hooks", string, "Erlang module containing hooks to run on the destination"},
        {exclude_system_libs, $E, "exclude-system-libs", {boolean, true},
            "Do not synchronize system libs"},

        {cookie, $c, "cookie", {string, "cookie"}, "Erlang magic cookie to use"},
        {sname, $s, "sname", string, "Short name for the local node"},
        {name, $n, "name", string, "Long name for the local node"},

        {mode, $M, "mode", {string, "erl"},
            "Method to synchronize release using ssh or erlang distributed protocol"},
        {port, $P, "port", integer, "ssh port to use"},
        {user, $u, "user", string, "ssh username"},

        {help, $h, "help", undefined, "Show help usage"}
    ].

do(Name, ArgOptions, ConfigOptions) ->
    ArgParsed = parse_opts(ArgOptions),
    ConfigParsed = parse_opts(ConfigOptions),
    Options = resolve_parsed(ArgParsed, ConfigParsed),

    case Options of
        help ->
            getopt:usage(relsync_cli:opts(), Name),
            erlang:halt(0);
        {ssh, Config} ->
            ok = relsync:update_nodes(ssh, Config);
        {erl, Config} ->
            target_syncer_sup:start_link(),
            ok = relsync:setup_local_node(Config),
            ok = relsync:update_nodes(erl, Config);
        {error, Message} ->
            io:put_chars(standard_error, [Message, "\n\n"]),
            getopt:usage(relsync_cli:opts(), Name),
            erlang:halt(2)
    end.

-spec parse_opts(list()) -> parsed().
parse_opts(Args) ->
    parse_opts(Args, undefined, #config{}).

parse_opts([help | _Rest], _, _) ->
    help;
parse_opts([version | _Rest], _, _) ->
    version;
parse_opts([{mode, "ssh"} | Rest], undefined, Config) ->
    parse_opts(Rest, ssh, Config#config{mode = ssh});
parse_opts([{mode, "erl"} | Rest], undefined, Config) ->
    parse_opts(Rest, erl, Config#config{mode = erl});
parse_opts([{exclude_system_libs, Flag} | Rest], Opt, Config) ->
    parse_opts(Rest, Opt, Config#config{exclude_system_libs = Flag});
parse_opts([{destnode, Node} | Rest], Opt, Config) ->
    parse_opts(Rest, Opt, Config#config{destnode = Node});
parse_opts([{destpath, Path} | Rest], Opt, Config) ->
    parse_opts(Rest, Opt, Config#config{destpath = Path});
parse_opts([{destrwpath, Path} | Rest], Opt, Config) ->
    parse_opts(Rest, Opt, Config#config{destrwpath = Path});
parse_opts([{localpath, Path} | Rest], Opt, Config) ->
    parse_opts(Rest, Opt, Config#config{localpath = Path});
parse_opts([{hooks, Hooks} | Rest], Opt, Config) ->
    parse_opts(Rest, Opt, Config#config{hooks = Hooks});
parse_opts([{port, Port} | Rest], Opt, Config) ->
    parse_opts(Rest, Opt, Config#config{port = Port});
parse_opts([{user, User} | Rest], Opt, Config) ->
    parse_opts(Rest, Opt, Config#config{user = User});
parse_opts([{cookie, Cookie} | Rest], Opt, Config) ->
    parse_opts(Rest, Opt, Config#config{cookie = Cookie});
parse_opts([{sname, Name} | Rest], Opt, Config) ->
    parse_opts(Rest, Opt, Config#config{sname = Name});
parse_opts([{name, Name} | Rest], Opt, Config) ->
    parse_opts(Rest, Opt, Config#config{name = Name});
parse_opts([], Mode, Config) ->
    {Mode, Config};
parse_opts([Unknown | _], _, _) ->
    {error, io_lib:format("unknown option: ~p", [Unknown])}.

-spec resolve_parsed(parsed(), parsed()) -> parsed().
resolve_parsed(ArgParsed, ConfigParsed) ->
    case {ArgParsed, ConfigParsed} of
        {{error, _} = Error, _} -> Error;
        {_, {error, _} = Error} -> Error;
        {help, _} -> help;
        {_, help} -> help;
        {version, _} -> version;
        {_, version} -> version;
        {{ssh, _}, {erl, _}} -> {error, "either ssh or erl should be provide"};
        {{erl, _}, {ssh, _}} -> {error, "either ssh or erl should be provide"};
        {{undefined, _}, {undefined, _}} -> {error, "either ssh or erl should be provide"};
        {{ssh, C0}, {ssh, C1}} -> {ssh, resolve_config(C0, C1)};
        {{erl, C0}, {erl, C1}} -> {erl, resolve_config(C0, C1)};
        {{ssh, C0}, {undefined, C1}} -> {ssh, resolve_config(C0, C1)};
        {{undefined, C0}, {ssh, C1}} -> {ssh, resolve_config(C0, C1)};
        {{erl, C0}, {undefined, C1}} -> {erl, resolve_config(C0, C1)};
        {{undefined, C0}, {erl, C1}} -> {erl, resolve_config(C0, C1)}
    end.

resolve_config(
    #config{
        cookie = ArgsCookie,
        sname = ArgsSName,
        name = ArgsName,
        port = ArgsPort,
        user = ArgsUser,
        destnode = ArgsDestNode,
        destpath = ArgsDestPath,
        destrwpath = ArgsDestRWPath,
        localpath = ArgsLocalPath,
        hooks = ArgsHooks,
        exclude_system_libs = ArgsExcludeSystemLibs
    },
    #config{
        cookie = OptsCookie,
        sname = OptsSName,
        name = OptsName,
        port = OptsPort,
        user = OptsUser,
        destnode = OptsDestNode,
        destpath = OptsDestPath,
        destrwpath = OptsDestRWPath,
        localpath = OptsLocalPath,
        hooks = OptsHooks,
        exclude_system_libs = OptsExcludeSystemLibs
    }
) ->
    #config{
        cookie = resolve_cookie(ArgsCookie, OptsCookie),
        sname = resolve_sname(ArgsSName, OptsSName),
        name = resolve_name(ArgsName, OptsName),
        port = resolve_port(ArgsPort, OptsPort),
        user = resolve_user(ArgsUser, OptsUser),
        destnode = resolve_destnode(ArgsDestNode, OptsDestNode),
        destpath = resolve_destpath(ArgsDestPath, OptsDestPath),
        destrwpath = resolve_destrwpath(ArgsDestRWPath, OptsDestRWPath),
        localpath = resolve_localpath(ArgsLocalPath, OptsLocalPath),
        hooks = resolve_hooks(ArgsHooks, OptsHooks),
        exclude_system_libs = resolve_exclude_system_libs(
            ArgsExcludeSystemLibs,
            OptsExcludeSystemLibs
        )
    }.

resolve_cookie(undefined, C) -> C;
resolve_cookie(C, _) -> C.

resolve_sname(undefined, S) -> S;
resolve_sname(S, _) -> S.

resolve_name(undefined, N) -> N;
resolve_name(N, _) -> N.

resolve_port(0, P) -> P;
resolve_port(P, _) -> P.

resolve_user(undefined, U) -> U;
resolve_user(U, _) -> U.

resolve_destnode(undefined, D) -> D;
resolve_destnode(D, _) -> D.

resolve_destpath(undefined, D) -> D;
resolve_destpath(D, _) -> D.

resolve_destrwpath(undefined, D) -> D;
resolve_destrwpath(D, _) -> D.

resolve_localpath(undefined, L) -> L;
resolve_localpath(L, _) -> L.

resolve_hooks(undefined, H) -> H;
resolve_hooks(H, _) -> H.

resolve_exclude_system_libs(true, E) -> E;
resolve_exclude_system_libs(E, _) -> E.
