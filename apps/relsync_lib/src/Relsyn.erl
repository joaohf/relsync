%% Generated by the Erlang ASN.1 BER compiler. Version: 5.0.14
%% Purpose: Encoding and decoding of the types in Relsyn.

-module('Relsyn').

-compile(nowarn_unused_vars).

-dialyzer(no_improper_lists).
-dialyzer(no_match).

-include("Relsyn.hrl").

-asn1_info([
    {vsn, '5.0.14'},
    {module, 'Relsyn'},
    {options, [
        {i, "/home/joaohf/work/projetos/relsyncro/apps/relsync_lib/asngen"},
        ber,
        {outdir, "/home/joaohf/work/projetos/relsyncro/apps/relsync_lib/asngen"},
        {i, "."},
        {i, "/home/joaohf/work/projetos/relsyncro/apps/relsync_lib/asn1"}
    ]}
]).

-export([
    encoding_rule/0,
    maps/0,
    bit_string_format/0,
    legacy_erlang_types/0
]).

-export(['dialyzer-suppressions'/1]).
-export([
    enc_RelsynMessage/2,
    enc_RelsyncResult/2,
    enc_SyncRequest/2,
    enc_ClearHookRequest/2,
    enc_ClearHookResponse/2,
    enc_SetHookRequest/2,
    enc_SetHookResponse/2,
    enc_SyncResponse/2,
    enc_ReloadResponse/2,
    enc_ReloadListEntry/2,
    enc_ReloadEntry/2,
    enc_ReloadRequest/2,
    enc_FileListRequest/2,
    enc_FileListResponseDone/2,
    enc_FileListEntry/2,
    enc_PathList/2,
    enc_FileList/2,
    enc_FileAttribute/2,
    enc_FileInfo/2,
    enc_Hash/2,
    enc_FilePathString/2,
    enc_AbsoluteDirectoryPath/2
]).

-export([
    dec_RelsynMessage/2,
    dec_RelsyncResult/2,
    dec_SyncRequest/2,
    dec_ClearHookRequest/2,
    dec_ClearHookResponse/2,
    dec_SetHookRequest/2,
    dec_SetHookResponse/2,
    dec_SyncResponse/2,
    dec_ReloadResponse/2,
    dec_ReloadListEntry/2,
    dec_ReloadEntry/2,
    dec_ReloadRequest/2,
    dec_FileListRequest/2,
    dec_FileListResponseDone/2,
    dec_FileListEntry/2,
    dec_PathList/2,
    dec_FileList/2,
    dec_FileAttribute/2,
    dec_FileInfo/2,
    dec_Hash/2,
    dec_FilePathString/2,
    dec_AbsoluteDirectoryPath/2
]).

-export([info/0]).

-export([encode/2, decode/2]).

encoding_rule() -> ber.

maps() -> false.

bit_string_format() -> bitstring.

legacy_erlang_types() -> false.

encode(Type, Data) ->
    try iolist_to_binary(element(1, encode_disp(Type, Data))) of
        Bytes ->
            {ok, Bytes}
    catch
        Class:Exception:Stk when Class =:= error; Class =:= exit ->
            case Exception of
                {error, {asn1, Reason}} ->
                    {error, {asn1, {Reason, Stk}}};
                Reason ->
                    {error, {asn1, {Reason, Stk}}}
            end
    end.

decode(Type, Data) ->
    try
        Result = decode_disp(Type, element(1, ber_decode_nif(Data))),
        {ok, Result}
    catch
        Class:Exception:Stk when Class =:= error; Class =:= exit ->
            case Exception of
                {error, {asn1, Reason}} ->
                    {error, {asn1, {Reason, Stk}}};
                Reason ->
                    {error, {asn1, {Reason, Stk}}}
            end
    end.

encode_disp('RelsynMessage', Data) -> enc_RelsynMessage(Data);
encode_disp('RelsyncResult', Data) -> enc_RelsyncResult(Data);
encode_disp('SyncRequest', Data) -> enc_SyncRequest(Data);
encode_disp('ClearHookRequest', Data) -> enc_ClearHookRequest(Data);
encode_disp('ClearHookResponse', Data) -> enc_ClearHookResponse(Data);
encode_disp('SetHookRequest', Data) -> enc_SetHookRequest(Data);
encode_disp('SetHookResponse', Data) -> enc_SetHookResponse(Data);
encode_disp('SyncResponse', Data) -> enc_SyncResponse(Data);
encode_disp('ReloadResponse', Data) -> enc_ReloadResponse(Data);
encode_disp('ReloadListEntry', Data) -> enc_ReloadListEntry(Data);
encode_disp('ReloadEntry', Data) -> enc_ReloadEntry(Data);
encode_disp('ReloadRequest', Data) -> enc_ReloadRequest(Data);
encode_disp('FileListRequest', Data) -> enc_FileListRequest(Data);
encode_disp('FileListResponseDone', Data) -> enc_FileListResponseDone(Data);
encode_disp('FileListEntry', Data) -> enc_FileListEntry(Data);
encode_disp('PathList', Data) -> enc_PathList(Data);
encode_disp('FileList', Data) -> enc_FileList(Data);
encode_disp('FileAttribute', Data) -> enc_FileAttribute(Data);
encode_disp('FileInfo', Data) -> enc_FileInfo(Data);
encode_disp('Hash', Data) -> enc_Hash(Data);
encode_disp('FilePathString', Data) -> enc_FilePathString(Data);
encode_disp('AbsoluteDirectoryPath', Data) -> enc_AbsoluteDirectoryPath(Data);
encode_disp(Type, _Data) -> exit({error, {asn1, {undefined_type, Type}}}).

decode_disp('RelsynMessage', Data) -> dec_RelsynMessage(Data);
decode_disp('RelsyncResult', Data) -> dec_RelsyncResult(Data);
decode_disp('SyncRequest', Data) -> dec_SyncRequest(Data);
decode_disp('ClearHookRequest', Data) -> dec_ClearHookRequest(Data);
decode_disp('ClearHookResponse', Data) -> dec_ClearHookResponse(Data);
decode_disp('SetHookRequest', Data) -> dec_SetHookRequest(Data);
decode_disp('SetHookResponse', Data) -> dec_SetHookResponse(Data);
decode_disp('SyncResponse', Data) -> dec_SyncResponse(Data);
decode_disp('ReloadResponse', Data) -> dec_ReloadResponse(Data);
decode_disp('ReloadListEntry', Data) -> dec_ReloadListEntry(Data);
decode_disp('ReloadEntry', Data) -> dec_ReloadEntry(Data);
decode_disp('ReloadRequest', Data) -> dec_ReloadRequest(Data);
decode_disp('FileListRequest', Data) -> dec_FileListRequest(Data);
decode_disp('FileListResponseDone', Data) -> dec_FileListResponseDone(Data);
decode_disp('FileListEntry', Data) -> dec_FileListEntry(Data);
decode_disp('PathList', Data) -> dec_PathList(Data);
decode_disp('FileList', Data) -> dec_FileList(Data);
decode_disp('FileAttribute', Data) -> dec_FileAttribute(Data);
decode_disp('FileInfo', Data) -> dec_FileInfo(Data);
decode_disp('Hash', Data) -> dec_Hash(Data);
decode_disp('FilePathString', Data) -> dec_FilePathString(Data);
decode_disp('AbsoluteDirectoryPath', Data) -> dec_AbsoluteDirectoryPath(Data);
decode_disp(Type, _Data) -> exit({error, {asn1, {undefined_type, Type}}}).

info() ->
    case ?MODULE:module_info(attributes) of
        Attributes when is_list(Attributes) ->
            case lists:keyfind(asn1_info, 1, Attributes) of
                {_, Info} when is_list(Info) ->
                    Info;
                _ ->
                    []
            end;
        _ ->
            []
    end.

%%================================
%%  RelsynMessage
%%================================
enc_RelsynMessage(Val) ->
    enc_RelsynMessage(Val, []).

enc_RelsynMessage(Val, TagIn) ->
    {EncBytes, EncLen} =
        case element(1, Val) of
            request ->
                'enc_RelsynMessage_request'(element(2, Val), [<<160>>]);
            response ->
                'enc_RelsynMessage_response'(element(2, Val), [<<161>>]);
            Else ->
                exit({error, {asn1, {invalid_choice_type, Else}}})
        end,

    encode_tags(TagIn, EncBytes, EncLen).

%%================================
%%  RelsynMessage_request
%%================================
enc_RelsynMessage_request(Val, TagIn) ->
    {EncBytes, EncLen} =
        case element(1, Val) of
            syncRequest ->
                'enc_SyncRequest'(element(2, Val), [<<160>>]);
            fileListRequest ->
                'enc_FileListRequest'(element(2, Val), [<<161>>]);
            reloadRequest ->
                'enc_ReloadRequest'(element(2, Val), [<<162>>]);
            setHookRequest ->
                'enc_SetHookRequest'(element(2, Val), [<<163>>]);
            clearHookRequest ->
                'enc_ClearHookRequest'(element(2, Val), [<<164>>]);
            Else ->
                exit({error, {asn1, {invalid_choice_type, Else}}})
        end,

    encode_tags(TagIn, EncBytes, EncLen).

%%================================
%%  RelsynMessage_response
%%================================
enc_RelsynMessage_response(Val, TagIn) ->
    {EncBytes, EncLen} =
        case element(1, Val) of
            syncResponse ->
                'enc_SyncResponse'(element(2, Val), [<<160>>]);
            reloadResponse ->
                'enc_ReloadResponse'(element(2, Val), [<<161>>]);
            fileListResponseEntry ->
                'enc_FileListEntry'(element(2, Val), [<<162>>]);
            fileListResponseDone ->
                'enc_FileListResponseDone'(element(2, Val), [<<163>>]);
            setHookResponse ->
                'enc_SetHookResponse'(element(2, Val), [<<164>>]);
            clearHookResponse ->
                'enc_ClearHookResponse'(element(2, Val), [<<165>>]);
            Else ->
                exit({error, {asn1, {invalid_choice_type, Else}}})
        end,

    encode_tags(TagIn, EncBytes, EncLen).

dec_RelsynMessage(Tlv) ->
    dec_RelsynMessage(Tlv, []).

dec_RelsynMessage(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case
        (case Tlv1 of
            [CtempTlv1] -> CtempTlv1;
            _ -> Tlv1
        end)
    of
        %% 'request'
        {131072, V1} ->
            {request, 'dec_RelsynMessage_request'(V1, [])};
        %% 'response'
        {131073, V1} ->
            {response, 'dec_RelsynMessage_response'(V1, [])};
        Else ->
            exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

'dec_RelsynMessage_request'(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case
        (case Tlv1 of
            [CtempTlv1] -> CtempTlv1;
            _ -> Tlv1
        end)
    of
        %% 'syncRequest'
        {131072, V1} ->
            {syncRequest, 'dec_SyncRequest'(V1, [])};
        %% 'fileListRequest'
        {131073, V1} ->
            {fileListRequest, 'dec_FileListRequest'(V1, [])};
        %% 'reloadRequest'
        {131074, V1} ->
            {reloadRequest, 'dec_ReloadRequest'(V1, [])};
        %% 'setHookRequest'
        {131075, V1} ->
            {setHookRequest, 'dec_SetHookRequest'(V1, [])};
        %% 'clearHookRequest'
        {131076, V1} ->
            {clearHookRequest, 'dec_ClearHookRequest'(V1, [])};
        Else ->
            exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

'dec_RelsynMessage_response'(Tlv, TagIn) ->
    Tlv1 = match_tags(Tlv, TagIn),
    case
        (case Tlv1 of
            [CtempTlv1] -> CtempTlv1;
            _ -> Tlv1
        end)
    of
        %% 'syncResponse'
        {131072, V1} ->
            {syncResponse, 'dec_SyncResponse'(V1, [])};
        %% 'reloadResponse'
        {131073, V1} ->
            {reloadResponse, 'dec_ReloadResponse'(V1, [])};
        %% 'fileListResponseEntry'
        {131074, V1} ->
            {fileListResponseEntry, 'dec_FileListEntry'(V1, [])};
        %% 'fileListResponseDone'
        {131075, V1} ->
            {fileListResponseDone, 'dec_FileListResponseDone'(V1, [])};
        %% 'setHookResponse'
        {131076, V1} ->
            {setHookResponse, 'dec_SetHookResponse'(V1, [])};
        %% 'clearHookResponse'
        {131077, V1} ->
            {clearHookResponse, 'dec_ClearHookResponse'(V1, [])};
        Else ->
            exit({error, {asn1, {invalid_choice_tag, Else}}})
    end.

%%================================
%%  RelsyncResult
%%================================
enc_RelsyncResult(Val) ->
    enc_RelsyncResult(Val, [<<48>>]).

enc_RelsyncResult(Val, TagIn) ->
    {_, Cindex1} = Val,

    %%-------------------------------------------------
    %% attribute resultCode(1) with type ENUMERATED
    %%-------------------------------------------------
    {EncBytes1, EncLen1} =
        case Cindex1 of
            ok -> encode_tags([<<128>>], [0], 1);
            error -> encode_tags([<<128>>], [1], 1);
            Enumval1 -> exit({error, {asn1, {enumerated_not_in_range, Enumval1}}})
        end,

    BytesSoFar = [EncBytes1],
    LenSoFar = EncLen1,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_RelsyncResult(Tlv) ->
    dec_RelsyncResult(Tlv, [16]).

dec_RelsyncResult(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),

    %%-------------------------------------------------
    %% attribute resultCode(1) with type ENUMERATED
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 =
        case decode_integer(V1, [131072]) of
            0 -> ok;
            1 -> error;
            Default1 -> exit({error, {asn1, {illegal_enumerated, Default1}}})
        end,

    case Tlv2 of
        % extra fields not allowed
        [] -> true;
        _ -> exit({error, {asn1, {unexpected, Tlv2}}})
    end,
    Res1 = {'RelsyncResult', Term1},
    Res1.

%%================================
%%  SyncRequest
%%================================
enc_SyncRequest(Val) ->
    enc_SyncRequest(Val, [<<48>>]).

enc_SyncRequest(Val, TagIn) ->
    {_, Cindex1} = Val,

    %%-------------------------------------------------
    %% attribute operation(1) with type ENUMERATED
    %%-------------------------------------------------
    {EncBytes1, EncLen1} =
        case Cindex1 of
            pre -> encode_tags([<<128>>], [0], 1);
            update -> encode_tags([<<128>>], [1], 1);
            post -> encode_tags([<<128>>], [2], 1);
            Enumval1 -> exit({error, {asn1, {enumerated_not_in_range, Enumval1}}})
        end,

    BytesSoFar = [EncBytes1],
    LenSoFar = EncLen1,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_SyncRequest(Tlv) ->
    dec_SyncRequest(Tlv, [16]).

dec_SyncRequest(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),

    %%-------------------------------------------------
    %% attribute operation(1) with type ENUMERATED
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 =
        case decode_integer(V1, [131072]) of
            0 -> pre;
            1 -> update;
            2 -> post;
            Default1 -> exit({error, {asn1, {illegal_enumerated, Default1}}})
        end,

    case Tlv2 of
        % extra fields not allowed
        [] -> true;
        _ -> exit({error, {asn1, {unexpected, Tlv2}}})
    end,
    Res1 = {'SyncRequest', Term1},
    Res1.

%%================================
%%  ClearHookRequest
%%================================
enc_ClearHookRequest(Val) ->
    enc_ClearHookRequest(Val, [<<48>>]).

enc_ClearHookRequest(Val, TagIn) ->
    {_} = Val,

    BytesSoFar = [],
    LenSoFar = 0,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_ClearHookRequest(Tlv) ->
    dec_ClearHookRequest(Tlv, [16]).

dec_ClearHookRequest(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    match_tags(Tlv, TagIn),

    {'ClearHookRequest'}.

%%================================
%%  ClearHookResponse
%%================================
enc_ClearHookResponse(Val) ->
    enc_ClearHookResponse(Val, [<<48>>]).

enc_ClearHookResponse(Val, TagIn) ->
    enc_RelsyncResult(Val, TagIn).

dec_ClearHookResponse(Tlv) ->
    dec_ClearHookResponse(Tlv, [16]).

dec_ClearHookResponse(Tlv, TagIn) ->
    'dec_RelsyncResult'(Tlv, TagIn).

%%================================
%%  SetHookRequest
%%================================
enc_SetHookRequest(Val) ->
    enc_SetHookRequest(Val, [<<48>>]).

enc_SetHookRequest(Val, TagIn) ->
    {_, Cindex1, Cindex2, Cindex3} = Val,

    %%-------------------------------------------------
    %% attribute module(1) with type OCTET STRING
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_restricted_string(Cindex1, [<<128>>]),

    %%-------------------------------------------------
    %% attribute bin(2) with type OCTET STRING
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_restricted_string(Cindex2, [<<129>>]),

    %%-------------------------------------------------
    %% attribute file(3) with type OCTET STRING
    %%-------------------------------------------------
    {EncBytes3, EncLen3} = encode_restricted_string(Cindex3, [<<130>>]),

    BytesSoFar = [EncBytes1, EncBytes2, EncBytes3],
    LenSoFar = EncLen1 + EncLen2 + EncLen3,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_SetHookRequest(Tlv) ->
    dec_SetHookRequest(Tlv, [16]).

dec_SetHookRequest(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),

    %%-------------------------------------------------
    %% attribute module(1) with type OCTET STRING
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_octet_string(V1, [131072]),

    %%-------------------------------------------------
    %% attribute bin(2) with type OCTET STRING
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_octet_string(V2, [131073]),

    %%-------------------------------------------------
    %% attribute file(3) with type OCTET STRING
    %%-------------------------------------------------
    [V3 | Tlv4] = Tlv3,
    Term3 = decode_octet_string(V3, [131074]),

    case Tlv4 of
        % extra fields not allowed
        [] -> true;
        _ -> exit({error, {asn1, {unexpected, Tlv4}}})
    end,
    Res1 = {'SetHookRequest', Term1, Term2, Term3},
    Res1.

%%================================
%%  SetHookResponse
%%================================
enc_SetHookResponse(Val) ->
    enc_SetHookResponse(Val, [<<48>>]).

enc_SetHookResponse(Val, TagIn) ->
    enc_RelsyncResult(Val, TagIn).

dec_SetHookResponse(Tlv) ->
    dec_SetHookResponse(Tlv, [16]).

dec_SetHookResponse(Tlv, TagIn) ->
    'dec_RelsyncResult'(Tlv, TagIn).

%%================================
%%  SyncResponse
%%================================
enc_SyncResponse(Val) ->
    enc_SyncResponse(Val, [<<48>>]).

enc_SyncResponse(Val, TagIn) ->
    enc_RelsyncResult(Val, TagIn).

dec_SyncResponse(Tlv) ->
    dec_SyncResponse(Tlv, [16]).

dec_SyncResponse(Tlv, TagIn) ->
    'dec_RelsyncResult'(Tlv, TagIn).

%%================================
%%  ReloadResponse
%%================================
enc_ReloadResponse(Val) ->
    enc_ReloadResponse(Val, [<<48>>]).

enc_ReloadResponse(Val, TagIn) ->
    {_, Cindex1} = Val,

    %%-------------------------------------------------
    %% attribute paths(1)   External Relsyn:ReloadListEntry
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = 'enc_ReloadListEntry'(Cindex1, [<<160>>]),

    BytesSoFar = [EncBytes1],
    LenSoFar = EncLen1,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_ReloadResponse(Tlv) ->
    dec_ReloadResponse(Tlv, [16]).

dec_ReloadResponse(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),

    %%-------------------------------------------------
    %% attribute paths(1)   External Relsyn:ReloadListEntry
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = 'dec_ReloadListEntry'(V1, [131072]),

    case Tlv2 of
        % extra fields not allowed
        [] -> true;
        _ -> exit({error, {asn1, {unexpected, Tlv2}}})
    end,
    Res1 = {'ReloadResponse', Term1},
    Res1.

%%================================
%%  ReloadListEntry
%%================================
enc_ReloadListEntry(Val) ->
    enc_ReloadListEntry(Val, [<<48>>]).

enc_ReloadListEntry(Val, TagIn) ->
    {EncBytes, EncLen} = 'enc_ReloadListEntry_components'(Val, [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

'enc_ReloadListEntry_components'([], AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
'enc_ReloadListEntry_components'([H | T], AccBytes, AccLen) ->
    {EncBytes, EncLen} = 'enc_ReloadEntry'(H, [<<48>>]),
    'enc_ReloadListEntry_components'(T, [EncBytes | AccBytes], AccLen + EncLen).

dec_ReloadListEntry(Tlv) ->
    dec_ReloadListEntry(Tlv, [16]).

dec_ReloadListEntry(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    ['dec_ReloadEntry'(V1, [16]) || V1 <- Tlv1].

%%================================
%%  ReloadEntry
%%================================
enc_ReloadEntry(Val) ->
    enc_ReloadEntry(Val, [<<48>>]).

enc_ReloadEntry(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,

    %%-------------------------------------------------
    %% attribute path(1) with type OCTET STRING
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_restricted_string(Cindex1, [<<128>>]),

    %%-------------------------------------------------
    %% attribute status(2) with type ENUMERATED
    %%-------------------------------------------------
    {EncBytes2, EncLen2} =
        case Cindex2 of
            skipped -> encode_tags([<<129>>], [0], 1);
            notloaded -> encode_tags([<<129>>], [1], 1);
            sticky -> encode_tags([<<129>>], [2], 1);
            ok -> encode_tags([<<129>>], [3], 1);
            Enumval2 -> exit({error, {asn1, {enumerated_not_in_range, Enumval2}}})
        end,

    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_ReloadEntry(Tlv) ->
    dec_ReloadEntry(Tlv, [16]).

dec_ReloadEntry(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),

    %%-------------------------------------------------
    %% attribute path(1) with type OCTET STRING
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_octet_string(V1, [131072]),

    %%-------------------------------------------------
    %% attribute status(2) with type ENUMERATED
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 =
        case decode_integer(V2, [131073]) of
            0 -> skipped;
            1 -> notloaded;
            2 -> sticky;
            3 -> ok;
            Default1 -> exit({error, {asn1, {illegal_enumerated, Default1}}})
        end,

    case Tlv3 of
        % extra fields not allowed
        [] -> true;
        _ -> exit({error, {asn1, {unexpected, Tlv3}}})
    end,
    Res1 = {'ReloadEntry', Term1, Term2},
    Res1.

%%================================
%%  ReloadRequest
%%================================
enc_ReloadRequest(Val) ->
    enc_ReloadRequest(Val, [<<48>>]).

enc_ReloadRequest(Val, TagIn) ->
    {_, Cindex1} = Val,

    %%-------------------------------------------------
    %% attribute paths(1)   External Relsyn:PathList
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = 'enc_PathList'(Cindex1, [<<160>>]),

    BytesSoFar = [EncBytes1],
    LenSoFar = EncLen1,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_ReloadRequest(Tlv) ->
    dec_ReloadRequest(Tlv, [16]).

dec_ReloadRequest(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),

    %%-------------------------------------------------
    %% attribute paths(1)   External Relsyn:PathList
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = 'dec_PathList'(V1, [131072]),

    case Tlv2 of
        % extra fields not allowed
        [] -> true;
        _ -> exit({error, {asn1, {unexpected, Tlv2}}})
    end,
    Res1 = {'ReloadRequest', Term1},
    Res1.

%%================================
%%  FileListRequest
%%================================
enc_FileListRequest(Val) ->
    enc_FileListRequest(Val, [<<48>>]).

enc_FileListRequest(Val, TagIn) ->
    {_, Cindex1} = Val,

    %%-------------------------------------------------
    %% attribute absoluteDirectoryPath(1) with type OCTET STRING
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_restricted_string(Cindex1, [<<128>>]),

    BytesSoFar = [EncBytes1],
    LenSoFar = EncLen1,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_FileListRequest(Tlv) ->
    dec_FileListRequest(Tlv, [16]).

dec_FileListRequest(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),

    %%-------------------------------------------------
    %% attribute absoluteDirectoryPath(1) with type OCTET STRING
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_octet_string(V1, [131072]),

    case Tlv2 of
        % extra fields not allowed
        [] -> true;
        _ -> exit({error, {asn1, {unexpected, Tlv2}}})
    end,
    Res1 = {'FileListRequest', Term1},
    Res1.

%%================================
%%  FileListResponseDone
%%================================
enc_FileListResponseDone(Val) ->
    enc_FileListResponseDone(Val, [<<48>>]).

enc_FileListResponseDone(Val, TagIn) ->
    enc_RelsyncResult(Val, TagIn).

dec_FileListResponseDone(Tlv) ->
    dec_FileListResponseDone(Tlv, [16]).

dec_FileListResponseDone(Tlv, TagIn) ->
    'dec_RelsyncResult'(Tlv, TagIn).

%%================================
%%  FileListEntry
%%================================
enc_FileListEntry(Val) ->
    enc_FileListEntry(Val, [<<48>>]).

enc_FileListEntry(Val, TagIn) ->
    {_, Cindex1} = Val,

    %%-------------------------------------------------
    %% attribute files(1)   External Relsyn:FileList
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = 'enc_FileList'(Cindex1, [<<160>>]),

    BytesSoFar = [EncBytes1],
    LenSoFar = EncLen1,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_FileListEntry(Tlv) ->
    dec_FileListEntry(Tlv, [16]).

dec_FileListEntry(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),

    %%-------------------------------------------------
    %% attribute files(1)   External Relsyn:FileList
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = 'dec_FileList'(V1, [131072]),

    case Tlv2 of
        % extra fields not allowed
        [] -> true;
        _ -> exit({error, {asn1, {unexpected, Tlv2}}})
    end,
    Res1 = {'FileListEntry', Term1},
    Res1.

%%================================
%%  PathList
%%================================
enc_PathList(Val) ->
    enc_PathList(Val, [<<48>>]).

enc_PathList(Val, TagIn) ->
    {EncBytes, EncLen} = 'enc_PathList_components'(Val, [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

'enc_PathList_components'([], AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
'enc_PathList_components'([H | T], AccBytes, AccLen) ->
    {EncBytes, EncLen} = encode_restricted_string(H, [<<4>>]),
    'enc_PathList_components'(T, [EncBytes | AccBytes], AccLen + EncLen).

dec_PathList(Tlv) ->
    dec_PathList(Tlv, [16]).

dec_PathList(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    [decode_octet_string(V1, [4]) || V1 <- Tlv1].

%%================================
%%  FileList
%%================================
enc_FileList(Val) ->
    enc_FileList(Val, [<<48>>]).

enc_FileList(Val, TagIn) ->
    {EncBytes, EncLen} = 'enc_FileList_components'(Val, [], 0),
    encode_tags(TagIn, EncBytes, EncLen).

'enc_FileList_components'([], AccBytes, AccLen) ->
    {lists:reverse(AccBytes), AccLen};
'enc_FileList_components'([H | T], AccBytes, AccLen) ->
    {EncBytes, EncLen} = 'enc_FileAttribute'(H, [<<48>>]),
    'enc_FileList_components'(T, [EncBytes | AccBytes], AccLen + EncLen).

dec_FileList(Tlv) ->
    dec_FileList(Tlv, [16]).

dec_FileList(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),
    ['dec_FileAttribute'(V1, [16]) || V1 <- Tlv1].

%%================================
%%  FileAttribute
%%================================
enc_FileAttribute(Val) ->
    enc_FileAttribute(Val, [<<48>>]).

enc_FileAttribute(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,

    %%-------------------------------------------------
    %% attribute file(1) with type OCTET STRING
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_restricted_string(Cindex1, [<<128>>]),

    %%-------------------------------------------------
    %% attribute fileInfo(2)   External Relsyn:FileInfo
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = 'enc_FileInfo'(Cindex2, [<<161>>]),

    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_FileAttribute(Tlv) ->
    dec_FileAttribute(Tlv, [16]).

dec_FileAttribute(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),

    %%-------------------------------------------------
    %% attribute file(1) with type OCTET STRING
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_octet_string(V1, [131072]),

    %%-------------------------------------------------
    %% attribute fileInfo(2)   External Relsyn:FileInfo
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = 'dec_FileInfo'(V2, [131073]),

    case Tlv3 of
        % extra fields not allowed
        [] -> true;
        _ -> exit({error, {asn1, {unexpected, Tlv3}}})
    end,
    Res1 = {'FileAttribute', Term1, Term2},
    Res1.

%%================================
%%  FileInfo
%%================================
enc_FileInfo(Val) ->
    enc_FileInfo(Val, [<<48>>]).

enc_FileInfo(Val, TagIn) ->
    {_, Cindex1, Cindex2} = Val,

    %%-------------------------------------------------
    %% attribute mode(1) with type INTEGER
    %%-------------------------------------------------
    {EncBytes1, EncLen1} = encode_integer(Cindex1, [<<128>>]),

    %%-------------------------------------------------
    %% attribute hash(2) with type OCTET STRING
    %%-------------------------------------------------
    {EncBytes2, EncLen2} = encode_restricted_string(Cindex2, [<<129>>]),

    BytesSoFar = [EncBytes1, EncBytes2],
    LenSoFar = EncLen1 + EncLen2,
    encode_tags(TagIn, BytesSoFar, LenSoFar).

dec_FileInfo(Tlv) ->
    dec_FileInfo(Tlv, [16]).

dec_FileInfo(Tlv, TagIn) ->
    %%-------------------------------------------------
    %% decode tag and length
    %%-------------------------------------------------
    Tlv1 = match_tags(Tlv, TagIn),

    %%-------------------------------------------------
    %% attribute mode(1) with type INTEGER
    %%-------------------------------------------------
    [V1 | Tlv2] = Tlv1,
    Term1 = decode_integer(V1, [131072]),

    %%-------------------------------------------------
    %% attribute hash(2) with type OCTET STRING
    %%-------------------------------------------------
    [V2 | Tlv3] = Tlv2,
    Term2 = decode_octet_string(V2, [131073]),

    case Tlv3 of
        % extra fields not allowed
        [] -> true;
        _ -> exit({error, {asn1, {unexpected, Tlv3}}})
    end,
    Res1 = {'FileInfo', Term1, Term2},
    Res1.

%%================================
%%  Hash
%%================================
enc_Hash(Val) ->
    enc_Hash(Val, [<<4>>]).

enc_Hash(Val, TagIn) ->
    encode_restricted_string(Val, TagIn).

dec_Hash(Tlv) ->
    dec_Hash(Tlv, [4]).

dec_Hash(Tlv, TagIn) ->
    decode_octet_string(Tlv, TagIn).

%%================================
%%  FilePathString
%%================================
enc_FilePathString(Val) ->
    enc_FilePathString(Val, [<<4>>]).

enc_FilePathString(Val, TagIn) ->
    encode_restricted_string(Val, TagIn).

dec_FilePathString(Tlv) ->
    dec_FilePathString(Tlv, [4]).

dec_FilePathString(Tlv, TagIn) ->
    decode_octet_string(Tlv, TagIn).

%%================================
%%  AbsoluteDirectoryPath
%%================================
enc_AbsoluteDirectoryPath(Val) ->
    enc_AbsoluteDirectoryPath(Val, [<<4>>]).

enc_AbsoluteDirectoryPath(Val, TagIn) ->
    encode_restricted_string(Val, TagIn).

dec_AbsoluteDirectoryPath(Tlv) ->
    dec_AbsoluteDirectoryPath(Tlv, [4]).

dec_AbsoluteDirectoryPath(Tlv, TagIn) ->
    decode_octet_string(Tlv, TagIn).

%%%
%%% Run-time functions.
%%%

'dialyzer-suppressions'(Arg) ->
    ok.

ber_decode_nif(B) ->
    asn1rt_nif:decode_ber_tlv(B).

collect_parts(TlvList) ->
    collect_parts(TlvList, []).

collect_parts([{_, L} | Rest], Acc) when is_list(L) ->
    collect_parts(Rest, [collect_parts(L) | Acc]);
collect_parts([{3, <<Unused, Bits/binary>>} | Rest], _Acc) ->
    collect_parts_bit(Rest, [Bits], Unused);
collect_parts([{_T, V} | Rest], Acc) ->
    collect_parts(Rest, [V | Acc]);
collect_parts([], Acc) ->
    list_to_binary(lists:reverse(Acc)).

collect_parts_bit([{3, <<Unused, Bits/binary>>} | Rest], Acc, Uacc) ->
    collect_parts_bit(Rest, [Bits | Acc], Unused + Uacc);
collect_parts_bit([], Acc, Uacc) ->
    list_to_binary([Uacc | lists:reverse(Acc)]).

decode_integer(Tlv, TagIn) ->
    Bin = match_tags(Tlv, TagIn),
    Len = byte_size(Bin),
    <<Int:Len/signed-unit:8>> = Bin,
    Int.

decode_octet_string(Tlv, TagsIn) ->
    Bin = match_and_collect(Tlv, TagsIn),
    binary:copy(Bin).

encode_integer(Val) ->
    Bytes =
        if
            Val >= 0 ->
                encode_integer_pos(Val, []);
            true ->
                encode_integer_neg(Val, [])
        end,
    {Bytes, length(Bytes)}.

encode_integer(Val, Tag) when is_integer(Val) ->
    encode_tags(Tag, encode_integer(Val));
encode_integer(Val, _Tag) ->
    exit({error, {asn1, {encode_integer, Val}}}).

encode_integer_neg(-1, [B1 | _T] = L) when B1 > 127 ->
    L;
encode_integer_neg(N, Acc) ->
    encode_integer_neg(N bsr 8, [N band 255 | Acc]).

encode_integer_pos(0, [B | _Acc] = L) when B < 128 ->
    L;
encode_integer_pos(N, Acc) ->
    encode_integer_pos(N bsr 8, [N band 255 | Acc]).

encode_length(L) when L =< 127 ->
    {[L], 1};
encode_length(L) ->
    Oct = minimum_octets(L),
    Len = length(Oct),
    if
        Len =< 126 ->
            {[128 bor Len | Oct], Len + 1};
        true ->
            exit({error, {asn1, too_long_length_oct, Len}})
    end.

encode_restricted_string(OctetList, TagIn) when is_binary(OctetList) ->
    encode_tags(TagIn, OctetList, byte_size(OctetList));
encode_restricted_string(OctetList, TagIn) when is_list(OctetList) ->
    encode_tags(TagIn, OctetList, length(OctetList)).

encode_tags(TagIn, {BytesSoFar, LenSoFar}) ->
    encode_tags(TagIn, BytesSoFar, LenSoFar).

encode_tags([Tag | Trest], BytesSoFar, LenSoFar) ->
    {Bytes2, L2} = encode_length(LenSoFar),
    encode_tags(
        Trest,
        [Tag, Bytes2 | BytesSoFar],
        LenSoFar + byte_size(Tag) + L2
    );
encode_tags([], BytesSoFar, LenSoFar) ->
    {BytesSoFar, LenSoFar}.

match_and_collect(Tlv, TagsIn) ->
    Val = match_tags(Tlv, TagsIn),
    case Val of
        [_ | _] = PartList ->
            collect_parts(PartList);
        Bin when is_binary(Bin) ->
            Bin
    end.

match_tags({T, V}, [T]) ->
    V;
match_tags({T, V}, [T | Tt]) ->
    match_tags(V, Tt);
match_tags([{T, V}], [T | Tt]) ->
    match_tags(V, Tt);
match_tags([{T, _V} | _] = Vlist, [T]) ->
    Vlist;
match_tags(Tlv, []) ->
    Tlv;
match_tags({Tag, _V} = Tlv, [T | _Tt]) ->
    exit({error, {asn1, {wrong_tag, {{expected, T}, {got, Tag, Tlv}}}}}).

minimum_octets(0, Acc) ->
    Acc;
minimum_octets(Val, Acc) ->
    minimum_octets(Val bsr 8, [Val band 255 | Acc]).

minimum_octets(Val) ->
    minimum_octets(Val, []).
