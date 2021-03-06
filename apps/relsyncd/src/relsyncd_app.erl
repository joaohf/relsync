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

%%%-------------------------------------------------------------------
%% @doc relsyncd public API
%% @end
%%%-------------------------------------------------------------------

-module(relsyncd_app).

-behaviour(application).

-export([start/2, stop/1, prep_stop/1]).

start(_StartType, _StartArgs) ->
    {ok, SshDaemonRef} = start_sshd(),
    {ok, Pid} = relsyncd_sup:start_link(),
    {ok, Pid, SshDaemonRef}.

prep_stop(SshDaemonRef) ->
    ok = ssh:stop_daemon(SshDaemonRef),
    [].

stop(_State) ->
    ok.

%% internal functions

start_sshd() ->
    ok = ssh:start(),

    SshOptions = [
        {key_cb, relsyncd_ssh_server_key},
        {preferred_algorithms, [{public_key, ['ssh-rsa']}]},
        {system_dir, "/"},
        {password, "relsync"},
        {subsystems, [
            ssh_sftpd:subsystem_spec([{cwd, "/"}, {root, "/"}]),
            relsyncd_ssh_server:subsystem_spec()
        ]}
    ],

    ssh:daemon(2222, SshOptions).
