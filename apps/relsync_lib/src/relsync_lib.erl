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

-module(relsync_lib).

-export([
    maybe_update_beam/1,
    maybe_add_path/1,
    get_file_listing/1,
    maybe_compile/1
]).

-include_lib("kernel/include/file.hrl").

% Traverse the directory specified by path and return the list of files
% with their info.
-spec get_file_listing(string()) -> [{string(), {integer(), binary()}}].
get_file_listing(Path) when is_binary(Path) ->
    get_file_listing(binary_to_list(Path));
get_file_listing(Path) ->
    PrefixLength = length(Path),
    filelib:fold_files(
        Path,
        ".*",
        true,
        fun(Y, Acc) -> [{lists:nthtail(PrefixLength, Y), file_info(Y)} | Acc] end,
        []
    ).

-spec file_info(string()) -> {integer(), binary()}.
file_info(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Hash = crypto:hash(sha, Data),

    {ok, #file_info{mode = Mode}} = file:read_file_info(Filename),
    {Mode, Hash}.

maybe_update_beam(Path) when is_binary(Path) ->
    maybe_update_beam(binary:bin_to_list(Path));
maybe_update_beam(Path) when is_list(Path) ->
    case filename:extension(Path) of
        ".beam" ->
            update_beam(Path);
        _ ->
            skipped
    end.

update_beam(Path) ->
    Module = list_to_atom(filename:rootname(filename:basename(Path))),
    case code:which(Module) of
        non_existing ->
            % Code not loaded yet. Let the VM load it on demand.
            notloaded;
        _ ->
            % NOTE: we don't check whether the old path (from code:which/1)
            % is the same as the new Path. Symlink mirroring would fail this even
            % though it is ok, but in general, we don't police module naming collisions.
            case code:is_sticky(Module) of
                true ->
                    sticky;
                false ->
                    % Updating code that has been loaded
                    code:purge(Module),
                    case code:load_file(Module) of
                        {module, Module} ->
                            ok;
                        {error, nofile} ->
                            true = code:delete(Module),
                            ok
                    end
            end
    end.

maybe_compile(ModuleName) ->
    Module = list_to_atom(ModuleName),
    case code:get_object_code(Module) of
        {Module, Bin, File} ->
            {Module, Bin, File};
        _ ->
            {ok, CompiledModule, Bin} = compile:file(ModuleName, [binary]),
            {CompiledModule, Bin, ModuleName}
    end.

maybe_add_path(PathsAndStatus) when is_list(PathsAndStatus) ->
    NotLoaded = lists:filter(
        fun
            ({_Path, notloaded}) -> true;
            (_) -> false
        end,
        PathsAndStatus
    ),
    maybe_add_path(NotLoaded, []).

maybe_add_path([], []) ->
    % there is nothing to load
    ok;
maybe_add_path([], Acc) ->
    % call code:add_path/1
    [true = code:add_path(Dir) || Dir <- Acc],
    ok;
maybe_add_path([{Path, _Status} | Rest], Acc) when is_binary(Path) ->
    maybe_add_path([{binary_to_list(Path), _Status} | Rest], Acc);
maybe_add_path([{Path0, _Status} | Rest], Acc) ->
    Path = filename:dirname(Path0),
    % From Path, find ebin
    case lists:reverse(filename:split(Path)) of
        ["ebin" | _] ->
            maybe_add_path(Rest, [Path | Acc]);
        _ ->
            maybe_add_path(Rest, Acc)
    end.
