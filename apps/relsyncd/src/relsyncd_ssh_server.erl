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

-module(relsyncd_ssh_server).

-behaviour(ssh_server_channel).

-record(state, {
    n,
    id,
    cm,
    % hooks holds the module name that provides an alternative
    % implementation to the default synchronization
    hooks
}).

-export([
    init/1,
    handle_msg/2,
    handle_ssh_msg/2,
    terminate/2
]).

-export([subsystem_spec/0]).

-include_lib("kernel/include/logger.hrl").

-include_lib("relsync_lib/include/Relsyn.hrl").

subsystem_spec() ->
    {"relsync", {?MODULE, []}}.

init([]) ->
    {ok, #state{}}.

handle_msg({ssh_channel_up, ChannelId, ConnectionManager}, State) ->
    {ok, State#state{
        id = ChannelId,
        cm = ConnectionManager
    }}.

handle_ssh_msg({ssh_cm, CM, {data, ChannelId, 0, Data}}, #state{} = State) ->
    handle_req('Relsyn':decode('RelsynMessage', Data), CM, ChannelId, State);
handle_ssh_msg({ssh_cm, _ConnectionManager, {data, _ChannelId, 1, Data}}, State) ->
    ?LOG_INFO(" ~p~n", [binary_to_list(Data)]),
    {ok, State};
handle_ssh_msg({ssh_cm, _ConnectionManager, {eof, _ChannelId}}, State) ->
    {ok, State};
handle_ssh_msg({ssh_cm, _, {signal, _, _}}, State) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    {ok, State};
handle_ssh_msg(
    {ssh_cm, _, {exit_signal, ChannelId, _, _Error, _}},
    State
) ->
    {stop, ChannelId, State};
handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, _Status}}, State) ->
    {stop, ChannelId, State}.

terminate(_Reason, _State) ->
    ok.

handle_req({ok, {request, {clearHookRequest, #'ClearHookRequest'{}}}}, CM, ChannelId, State) ->
    {ok, Data} = 'Relsyn':encode(
        'RelsynMessage',
        {response, {syncResponse, #'RelsyncResult'{resultCode = ok}}}
    ),

    ok = ssh_connection:send(CM, ChannelId, Data),

    {ok, State#state{hooks = undefined}};
handle_req(
    {ok, {request, {setHookRequest, #'SetHookRequest'{module = Module0, bin = Bin, file = File0}}}},
    CM,
    ChannelId,
    State
) ->
    Module = binary_to_atom(Module0),
    File = binary_to_list(File0),
    {ok, Data} = 'Relsyn':encode(
        'RelsynMessage',
        {response, {syncResponse, #'RelsyncResult'{resultCode = ok}}}
    ),

    {module, Module} = code:load_binary(Module, File, Bin),

    ok = ssh_connection:send(CM, ChannelId, Data),

    {ok, State#state{hooks = Module}};
handle_req({ok, {request, {syncRequest, #'SyncRequest'{operation = pre}}}}, CM, ChannelId, State) ->
    #state{hooks = Hooks} = State,
    Res = maybe_call_hook(Hooks, presync),

    {ok, Data} = 'Relsyn':encode('RelsynMessage', {response, {syncResponse, {resultCode, Res}}}),

    ok = ssh_connection:send(CM, ChannelId, Data),
    {ok, State};
handle_req({ok, {request, {syncRequest, #'SyncRequest'{operation = post}}}}, CM, ChannelId, State) ->
    #state{hooks = Hooks} = State,
    Res = maybe_call_hook(Hooks, postsync),

    {ok, Data} = 'Relsyn':encode('RelsynMessage', {response, {syncResponse, {resultCode, Res}}}),

    ok = ssh_connection:send(CM, ChannelId, Data),
    {ok, State};
handle_req({ok, {request, {reloadRequest, #'ReloadRequest'{paths = Paths}}}}, CM, ChannelId, State) ->
    Entries0 = [{Path, relsync_lib:maybe_update_beam(Path)} || Path <- Paths],

    ok = relsync_lib:maybe_add_path(Entries0),

    Entries = [#'ReloadEntry'{path = Path, status = Status} || {Path, Status} <- Entries0],

    {ok, Data} = 'Relsyn':encode(
        'RelsynMessage',
        {response, {reloadResponse, #'ReloadResponse'{paths = Entries}}}
    ),
    ok = ssh_connection:send(CM, ChannelId, Data),
    {ok, State};
handle_req(
    {ok, {request, {fileListRequest, #'FileListRequest'{absoluteDirectoryPath = Path}}}},
    CM,
    ChannelId,
    State
) ->
    Files = do_file_listing(Path),
    Resp0 = {response, {fileListResponseEntry, #'FileListEntry'{files = Files}}},
    Resp1 = {response, {fileListResponseDone, {resultCode, ok}}},
    {ok, Data0} = 'Relsyn':encode('RelsynMessage', Resp0),
    {ok, Data1} = 'Relsyn':encode('RelsynMessage', Resp1),
    ok = ssh_connection:send(CM, ChannelId, Data0),
    ok = ssh_connection:send(CM, ChannelId, Data1),
    {ok, State};
handle_req({error, _Reason}, CM, ChannelId, State) ->
    ?LOG_NOTICE("error req ~p", [_Reason]),
    ok = ssh_connection:send_eof(CM, ChannelId),
    {stop, ChannelId, State}.

do_file_listing(Path) ->
    [
        begin
            FileInfo = #'FileInfo'{mode = Mode, hash = Hash},
            #'FileAttribute'{file = FilePath, fileInfo = FileInfo}
        end
        || {FilePath, {Mode, Hash}} <- relsync_lib:get_file_listing(Path)
    ].

-spec maybe_call_hook(module(), presync | postsync) -> ok.
maybe_call_hook(undefined, _) ->
    ok;
maybe_call_hook(M, F) ->
    try erlang:apply(M, F, []) of
        _ -> ok
    catch
        _ -> error
    end.
