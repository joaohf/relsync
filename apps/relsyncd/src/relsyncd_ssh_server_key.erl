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

-module(relsyncd_ssh_server_key).

-behaviour(ssh_server_key_api).

-export([host_key/2, is_auth_key/3]).

host_key('ssh-rsa', _DaemonOptions) ->
    Key = public_key:generate_key({rsa, 2048, 17}),
    {ok, Key}.

%% disable authentication using keys
is_auth_key(_PublicUserKey, _User, _DaemonOptions) -> false.
