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

-ifndef(_RELSYNC_HRL_).
-define(_RELSYNC_HRL_, true).

-record(config, {
    mode = undefined,
    cookie = undefined,
    sname = undefined,
    name = undefined,
    port = 0,
    user = undefined,
    destnode = undefined,
    destpath = undefined,
    destrwpath = undefined,
    localpath = undefined,
    hooks = undefined,

    exclude_system_libs = true :: boolean()
}).

-type config() :: #config{}.

-endif.
