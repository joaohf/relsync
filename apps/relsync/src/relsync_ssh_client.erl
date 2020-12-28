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

%%% Description: Example ssh client
-module(relsync_ssh_client).

-behaviour(ssh_client_channel).

-behaviour(ssh_client_key_api).

-include_lib("relsync_lib/include/Relsyn.hrl").

-record(state, {
    cm,
    ch_relsyncd,
    ch_sftp,
    dbg,
    files = [],
    from
}).

-export([
    connect/3,
    close/1,
    start_link/1,
    start/1,
    stop/1,
    send_eof/1,
    notify_presync/1,
    notify_postsync/1,
    get_file_listing/2,
    reload/2,
    set_hooks/2,
    copy_file/4,
    rm_file/2
]).

%% ssh_client_channel
-export([
    init/1,
    handle_msg/2,
    handle_ssh_msg/2,
    terminate/2,
    code_change/3,
    handle_call/3,
    handle_cast/2
]).

%% ssh_client_key_api
-export([
    add_host_key/3,
    is_host_key/4,
    user_key/2
]).

add_host_key(_, _, _) ->
    ok.

is_host_key(_, _, _, _) ->
    true.

user_key('ssh-rsa', Opts) ->
    UserDir = proplists:get_value(user_dir, Opts),
    KeyFile = filename:join(filename:dirname(UserDir), "id_rsa"),
    {ok, KeyBin} = file:read_file(KeyFile),
    [Entry] = public_key:pem_decode(KeyBin),
    Key = public_key:pem_entry_decode(Entry),
    {ok, Key};
user_key(_Alg, _Opt) ->
    {error, "Not Supported"}.

-define(DBG(State, Fmt, Args),
    case State#state.dbg of
        true ->
            io:format(
                "~p:~p ~p C=~p Ch=~p " ++ Fmt,
                [?MODULE, ?LINE, self(), State#state.cm, State#state.ch_relsyncd | Args]
            );
        false ->
            ok
    end
).

connect(Node, Port, UserDir) ->
    ConnectOpts = [
        {silently_accept_hosts, true},
        {user_interaction, false},
        {user_dir, UserDir},
        {key_cb, ?MODULE},
        {password, "l"}
    ],
    {ok, _ConRef} = ssh:connect(Node, Port, ConnectOpts).

close(ConRef) ->
    ok = ssh:close(ConRef).

start_link(ConRef) ->
    Dbg = false,
    {ok, ChRelsyncd} = ssh_connection:session_channel(ConRef, infinity),
    {ok, ChSftp} = ssh_sftp:start_channel(ConRef),

    ssh_client_channel:start_link(ConRef, ChRelsyncd, ?MODULE, [ConRef, ChRelsyncd, ChSftp, Dbg]).

start(ConRef) ->
    {ok, ChRelsyncd} = ssh_connection:session_channel(ConRef, infinity),
    {ok, ChSftp} = ssh_sftp:start_channel(ConRef),

    ssh_client_channel:start(ConRef, ChRelsyncd, ?MODULE, [ConRef, ChRelsyncd, ChSftp]).

send_eof(ChRef) ->
    ssh_client_channel:call(ChRef, send_eof).

notify_presync(ChRef) ->
    ssh_client_channel:call(ChRef, notify_presync).

notify_postsync(ChRef) ->
    ssh_client_channel:call(ChRef, notify_postsync).

reload(ChRef, FileList) ->
    ssh_client_channel:call(ChRef, {reload, FileList}).

get_file_listing(ChRef, DestPath) ->
    ssh_client_channel:call(ChRef, {get_file_listing, DestPath}).

copy_file(ChRef, DestPath, Mode, Contents) ->
    ssh_client_channel:cast(ChRef, {copy_file, {DestPath, Mode, Contents}}).

rm_file(ChRef, DestPath) ->
    ssh_client_channel:cast(ChRef, {rm_file, DestPath}).

set_hooks(ChRef, undefined) ->
    ssh_client_channel:call(ChRef, clear_hooks);
set_hooks(ChRef, ModuleName) ->
    {Module0, Bin, File} = relsync_lib:maybe_compile(ModuleName),
    Module = atom_to_binary(Module0),
    ssh_client_channel:call(ChRef, {set_hooks, Module, Bin, File}).

stop(ChRef) ->
    ssh_client_channel:call(ChRef, stop).

init([CM, ChRelsyncd, ChSftp, Dbg]) ->
    case ssh_connection:subsystem(CM, ChRelsyncd, "relsync", infinity) of
        success ->
            State = #state{
                cm = CM,
                ch_relsyncd = ChRelsyncd,
                ch_sftp = ChSftp,
                dbg = Dbg
            },
            ?DBG(State, "callback spawned", []),
            {ok, State};
        Other ->
            {stop, Other}
    end.

handle_msg({ssh_channel_up, ChannelId, ConnectionManager}, State0) ->
    State = State0#state{
        cm = ConnectionManager,
        ch_relsyncd = ChannelId
    },
    ?DBG(State, "ssh_channel_up", []),
    {ok, State}.

handle_ssh_msg({ssh_cm, C, {data, Ch, 0, Data}}, #state{ch_relsyncd = Ch, cm = C} = State) ->
    NewState = handle_resp('Relsyn':decode('RelsynMessage', Data), State),
    ?DBG(State, "ssh_cm data size(Data)=~p", [size(Data)]),
    {ok, NewState};
handle_ssh_msg({ssh_cm, C, {data, Ch, Type, Data}}, #state{ch_relsyncd = Ch, cm = C} = State) ->
    ?DBG(State, "ssh_cm data Type=~p : ~p", [Type, Data]),
    {ok, State};
handle_ssh_msg({ssh_cm, C, {eof, Ch}}, #state{ch_relsyncd = Ch, cm = C} = State) ->
    ?DBG(State, "eof", []),
    {ok, State};
handle_ssh_msg(
    {ssh_cm, C, {signal, Ch, _SigNameStr} = Sig},
    #state{ch_relsyncd = Ch, cm = C} = State
) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    ?DBG(State, "~p", [Sig]),
    {ok, State};
handle_ssh_msg(
    {ssh_cm, C, {exit_signal, Ch, _, _Error, _} = Sig},
    #state{ch_relsyncd = Ch, cm = C} = State
) ->
    ?DBG(State, "~p", [Sig]),
    {stop, Ch, State};
handle_ssh_msg(
    {ssh_cm, C, {exit_status, Ch, _Status} = Sig},
    #state{ch_relsyncd = Ch, cm = C} = State
) ->
    ?DBG(State, "~p", [Sig]),
    {stop, Ch, State}.

handle_call(send_eof, _From, #state{ch_relsyncd = Ch, cm = C} = State) ->
    {reply, ssh_connection:send_eof(C, Ch), State};
handle_call(notify_presync, From, #state{ch_relsyncd = Ch, cm = C} = State) ->
    {ok, Req} = 'Relsyn':encode('RelsynMessage', {request, {syncRequest, {operation, pre}}}),
    ?DBG(State, "pre sync", []),
    ok = ssh_connection:send(C, Ch, Req),
    {noreply, State#state{from = From}};
handle_call(notify_postsync, From, #state{ch_relsyncd = Ch, cm = C} = State) ->
    {ok, Req} = 'Relsyn':encode('RelsynMessage', {request, {syncRequest, {operation, post}}}),
    ?DBG(State, "post sync", []),
    ok = ssh_connection:send(C, Ch, Req),
    {noreply, State#state{from = From}};
handle_call({reload, FileList}, From, #state{ch_relsyncd = Ch, cm = C} = State) ->
    {ok, Req} = 'Relsyn':encode('RelsynMessage', {request, {reloadRequest, {paths, FileList}}}),
    ?DBG(State, "reload sync", []),
    ok = ssh_connection:send(C, Ch, Req),
    {noreply, State#state{from = From}};
handle_call(clear_hooks, From, #state{ch_relsyncd = Ch, cm = C} = State) ->
    {ok, Req} = 'Relsyn':encode(
        'RelsynMessage',
        {request, {clearHookRequest, #'ClearHookRequest'{}}}
    ),
    ?DBG(State, "clear hooks", []),
    ok = ssh_connection:send(C, Ch, Req),
    {noreply, State#state{from = From}};
handle_call({set_hooks, Module, Bin, File}, From, #state{ch_relsyncd = Ch, cm = C} = State) ->
    {ok, Req} = 'Relsyn':encode(
        'RelsynMessage',
        {request, {setHookRequest, #'SetHookRequest'{module = Module, bin = Bin, file = File}}}
    ),
    ?DBG(State, "set hooks", []),
    ok = ssh_connection:send(C, Ch, Req),
    {noreply, State#state{from = From}};
handle_call({get_file_listing, DestPath}, From, #state{ch_relsyncd = Ch, cm = C} = State) ->
    {ok, Req} = 'Relsyn':encode(
        'RelsynMessage',
        {request, {fileListRequest, {absoluteDirectoryPath, DestPath}}}
    ),
    ?DBG(State, "get_file_listing ~p", [DestPath]),
    ok = ssh_connection:send(C, Ch, Req),
    {noreply, State#state{from = From}};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(Msg, _From, State) ->
    ?DBG(State, "Unknown call ~p", [Msg]),
    {reply, {unknown_call, Msg}, State}.

terminate(Reason, State) ->
    ?DBG(State, "terminate Reason = ~p", [Reason]).

handle_cast({copy_file, {DestPath, _Mode, Contents}}, #state{ch_sftp = Ch} = State) ->
    ok = copy_file(Ch, DestPath, Contents),
    {noreply, State};
handle_cast({rm_file, DestPath}, #state{ch_sftp = Ch} = State) ->
    ok = ssh_sftp:delete(Ch, DestPath),
    {noreply, State};
handle_cast(Msg, State) ->
    ?DBG(State, "Unknown cast ~p", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_resp(
    {ok, {response, {syncResponse, #'RelsyncResult'{resultCode = Code}}}},
    #state{from = From} = State
) ->
    ssh_client_channel:reply(From, Code),
    State#state{from = undefined};
handle_resp(
    {ok, {response, {setHookResponse, #'RelsyncResult'{resultCode = Code}}}},
    #state{from = From} = State
) ->
    ssh_client_channel:reply(From, Code),
    State#state{from = undefined};
handle_resp(
    {ok, {response, {clearHookResponse, #'RelsyncResult'{resultCode = Code}}}},
    #state{from = From} = State
) ->
    ssh_client_channel:reply(From, Code),
    State#state{from = undefined};
handle_resp(
    {ok, {response, {fileListResponseEntry, #'FileListEntry'{files = Files}}}},
    State
) ->
    State#state{files = Files};
handle_resp(
    {ok, {response, {reloadResponse, #'ReloadResponse'{paths = Paths}}}},
    #state{from = From} = State
) ->
    Result = [{Path, Status} || #'ReloadEntry'{path = Path, status = Status} <- Paths],
    ssh_client_channel:reply(From, {ok, Result}),
    State#state{from = undefined};
handle_resp(
    {ok, {response, {fileListResponseDone, {'RelsyncResult', ok}}}},
    #state{from = From, files = Files} = State
) ->
    Files0 = [
        {binary:bin_to_list(FilePath), {Mode, Hash}}
        || #'FileAttribute'{file = FilePath, fileInfo = #'FileInfo'{mode = Mode, hash = Hash}} <-
               Files
    ],

    ssh_client_channel:reply(From, {ok, Files0}),
    State#state{files = []}.

copy_file(Ch, DestPath, Contents) ->
    case ssh_sftp:write_file(Ch, DestPath, Contents) of
        {error, no_such_file} ->
            ok = make_dir(Ch, filename:dirname(DestPath)),
            copy_file(Ch, DestPath, Contents);
        ok ->
            ok
    end.

make_dir(Ch, DestPath) ->
    case ssh_sftp:make_dir(Ch, DestPath) of
        ok -> ok;
        {error, no_such_file} -> make_dir(Ch, filename:dirname(DestPath))
    end.
