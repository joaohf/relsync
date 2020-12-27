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
%%%  This module is sent to the target device to scan files locally and
%%%  implement the updates as requested by the host device running relsync.
%%% @end
-module(target_syncer).

-include_lib("kernel/include/file.hrl").

-behaviour(gen_server).

%% API
-export([
    start_link/0, start_link/1,
    get_file_listing/2,
    set_hooks/2,
    copy_file/4,
    rm_file/2,
    create_symlink_mirror/3,
    notify_presync/1,
    notify_postsync/1,
    reload/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
    % hooks holds the module name that provides an alternative
    % implementation to the default synchronization
    hooks
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec get_file_listing(atom(), string()) -> [{string(), {integer(), binary()}}].
get_file_listing(Node, Path) ->
    gen_server:call({?SERVER, Node}, {get_file_listing, Path}).

% Use the specified Module to customize the behavior of the
% synchronization process. The object code for the Module
% is sent to the remote Node as well.
-spec set_hooks(atom(), atom()) -> ok.
set_hooks(Node, undefined) ->
    gen_server:call({?SERVER, Node}, clear_hooks);
set_hooks(Node, ModuleName) ->
    {Module, Bin, File} = maybe_compile(ModuleName),
    gen_server:call({?SERVER, Node}, {set_hooks, Module, Bin, File}).

maybe_compile(ModuleName) ->
    Module = list_to_atom(ModuleName),
    case code:get_object_code(Module) of
        {Module, Bin, File} ->
            {Module, Bin, File};
        _ ->
            {ok, CompiledModule, Bin} = compile:file(ModuleName, [binary]),
            {CompiledModule, Bin, ModuleName}
    end.

% Copy the Contents to the file specified by Path on Node, and
% then set the mode to Mode.
-spec copy_file(atom(), string(), integer(), binary()) -> ok | {error, _}.
copy_file(Node, Path, Mode, Contents) ->
    gen_server:call({?SERVER, Node}, {copy_file, Path, Mode, Contents}).

% Remove the specified file from Node
-spec rm_file(atom(), string()) -> ok | {error, _}.
rm_file(Node, Path) ->
    gen_server:call({?SERVER, Node}, {rm_file, Path}).

% Create a symlink mirror of all files in Path in NewPath,
% but only if NewPath doesn't exist.
-spec create_symlink_mirror(atom(), string(), string()) -> ok | {error, _}.
create_symlink_mirror(Node, Path, NewPath) ->
    gen_server:call({?SERVER, Node}, {create_symlink_mirror, Path, NewPath}).

% Called to let the remote node know that a synchronization
% run is coming.
-spec notify_presync(atom()) -> ok.
notify_presync(Node) ->
    gen_server:call({?SERVER, Node}, notify_presync).

% Called to let the remote node know that a synchronization
% run has finished.
-spec notify_postsync(atom()) -> ok.
notify_postsync(Node) ->
    gen_server:call({?SERVER, Node}, notify_postsync).

-spec reload(atom(), list()) -> {ok, list()}.
reload(Node, FileList) ->
    gen_server:call({?SERVER, Node}, {reload, FileList}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server locally (called by the supervisor)
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Starts the server on the specified remote node.
start_link(Node) ->
    Result = rpc:call(Node, gen_server, start, [{local, ?SERVER}, ?MODULE, [], []]),
    case Result of
        {ok, Pid} ->
            link(Pid)
    end,
    Result.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_file_listing, Path}, _From, State) ->
    Reply = relsync_lib:get_file_listing(Path),
    {reply, Reply, State};
handle_call(clear_hooks, _From, State) ->
    NewState = State#state{hooks = undefined},
    {reply, ok, NewState};
handle_call({set_hooks, Module, Bin, File}, _From, State) ->
    code:load_binary(Module, File, Bin),
    NewState = State#state{hooks = Module},
    {reply, ok, NewState};
handle_call({copy_file, Path, Mode, Contents}, _From, State) ->
    ok = filelib:ensure_dir(Path),
    % delete the file first so that we write to a new inode. This is needed
    % for symlink mirrors, but also more gracefully handles the case where
    % someone else has the file opened.
    file:delete(Path),
    ok = file:write_file(Path, Contents),
    ok = file:change_mode(Path, Mode),
    {reply, ok, State};
handle_call({create_symlink_mirror, Path, NewPath}, _From, State) ->
    case filelib:is_dir(NewPath) of
        false ->
            ok = filelib:ensure_dir(NewPath),
            FromFiles = relsync_lib:get_file_listing(Path),
            [ok = symlink_files(Path ++ File, NewPath ++ File) || {File, _} <- FromFiles],
            % Update Erlang's search paths to look in the mirror location now.
            ok = update_code_paths(Path, NewPath);
        true ->
            % Don't do anything, since the mirror already exists.
            ok
    end,
    {reply, ok, State};
handle_call({rm_file, Path}, _From, State) ->
    Reply = file:delete(Path),
    {reply, Reply, State};
handle_call(notify_presync, _From, State) ->
    #state{hooks = Hooks} = State,
    call_hook_or_not(Hooks, presync),
    {reply, ok, State};
handle_call(notify_postsync, _From, State) ->
    #state{hooks = Hooks} = State,
    call_hook_or_not(Hooks, postsync),
    {reply, ok, State};
handle_call({reload, Paths}, _From, State) ->
    Entries0 = [{Path, relsync_lib:maybe_update_beam(Path)} || Path <- Paths],
    ok = relsync_lib:maybe_add_path(Entries0),
    {reply, {ok, Entries0}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec call_hook_or_not(module() | undefined, presync | postsync) -> ok.
call_hook_or_not(undefined, presync) ->
    ok;
call_hook_or_not(undefined, postsync) ->
    ok;
call_hook_or_not(M, F) ->
    M:F().

symlink_files(From, To) ->
    filelib:ensure_dir(To),
    file:make_symlink(From, To).

replace_prefix(Path, From, To) ->
    case lists:prefix(From, Path) of
        false ->
            % Not affected, so don't update.
            Path;
        true ->
            To ++ lists:nthtail(length(From), Path)
    end.

% Update the Erlang VM's code search path to the new directory prefix.
% This is called after mirroring the directory so that we can write to it.
update_code_paths(From, To) ->
    NewPaths = [replace_prefix(Path, From, To) || Path <- code:get_path()],
    true = code:set_path(NewPaths),
    ok.
