%% Generated by the Erlang ASN.1 compiler. Version: 5.0.14
%% Purpose: Erlang record definitions for each named and unnamed
%% SEQUENCE and SET, and macro definitions for each value
%% definition in module Relsyn.

-ifndef(_RELSYN_HRL_).
-define(_RELSYN_HRL_, true).

-record('RelsyncResult', {
    resultCode
}).

-record('SyncRequest', {
    operation
}).

-record('ClearHookRequest', {}).

-record('SetHookRequest', {
    module,
    bin,
    file
}).

-record('ReloadResponse', {
    paths
}).

-record('ReloadEntry', {
    path,
    status
}).

-record('ReloadRequest', {
    paths
}).

-record('FileListRequest', {
    absoluteDirectoryPath
}).

-record('FileListEntry', {
    files
}).

-record('FileAttribute', {
    file,
    fileInfo
}).

-record('FileInfo', {
    mode,
    hash
}).

%% _RELSYN_HRL_
-endif.