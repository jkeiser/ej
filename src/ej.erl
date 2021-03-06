% License: Apache License, Version 2.0
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
%% @author Seth Falcon <seth@userprimary.net>
%% @copyright Copyright 2011-2012 Seth Falcon
%%
%% @doc Tools for working with Erlang terms representing JSON.
%%
%% The ej module is intended to make it easy to work with the Erlang
%% structure used by `mochijson2' to represent JSON.  You can use
%% `ej:get' to walk an object and return a particular value, or
%% `ej:set' to update a value.
%%
%% @end
-module(ej).
-author('Seth Falcon <seth@userprimary.net').
-export([
         get/2,
         get/3,
         set/3,
         delete/2,
         valid/2
         ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("ej.hrl").

-export_type([json_object/0,
              json_plist/0,
              json_term/0]).

%% @doc Extract a value from `Obj'
%%
%% `Keys' is a tuple specifying a path into the JSON structure.  Each
%% string or binary element of `Keys' will act like a Javascript
%% property lookup.  Elements of JSON arrays can be accessed by
%% including an integer as an element of `Keys'.  In addition, the
%% atoms `` 'first' '' and `` 'last' '' can be used to access the
%% first and last elements of a list, respectively.
%%
-spec(get(key_tuple(), json_object() | json_plist()) -> json_term() | undefined).

get(Keys, Obj) when is_tuple(Keys) ->
   get0(tuple_to_list(Keys), Obj).

%% @doc same as get/2, but returns `Default' if the specified value was not found.
-spec get(key_tuple(), json_object() | json_plist(), json_term()) -> json_term().
get(Keys, Obj, Default) when is_tuple(Keys) ->
    case get(Keys, Obj) of
        undefined ->
            Default;
        Value ->
            Value
    end.

get0([Key | Rest], Obj) ->
    case get_value(Key, Obj) of
        undefined -> undefined;
        AValue -> get0(Rest, AValue)
    end;
get0([], Value) ->
    Value.


get_value(Key, Obj) when is_list(Key) ->
    get_value(iolist_to_binary(Key), Obj);
get_value(Key, {struct, L}) when is_binary(Key) ->
    get_value(Key, L);
get_value(Key, {L}) when is_binary(Key) -> % alt form
    get_value(Key, L);
get_value(Key, PL=[{_, _}|_T]) when is_binary(Key) ->
    proplists:get_value(Key, PL);
get_value(Key, [_H|_T]) when is_binary(Key) ->
    undefined;
get_value(Key, []) when is_binary(Key) ->
    undefined;
get_value(first, [H|_T]) ->
    H;
get_value(last, List=[_H|_T]) ->
    lists:last(List);
get_value(Index, List=[_H|_T]) when is_integer(Index) ->
    lists:nth(Index, List);
get_value(Index, Obj) ->
    erlang:error({index_for_non_list, {Index, Obj}}).

as_binary(Key) when is_binary(Key) ->
    Key;
as_binary(Key) when is_list(Key) ->
    iolist_to_binary(Key);
as_binary(Key) when is_integer(Key) orelse is_atom(Key) ->
    Key.

%% @doc Set a value in `Obj'
%%
%% Replaces the value at the path specified by `Keys' with `Value' and
%% returns the new structure.  If `Value' is the atom `EJ_DELETE',
%% then the path specified by `Keys' is removed (but see `delete/2').
%%
-spec(set(key_tuple(), json_object(), json_term()) -> json_term()).
set(Keys, Obj, Value) when is_tuple(Keys) ->
    set0([ as_binary(X) || X <- tuple_to_list(Keys) ], Obj, Value).

set0([], _, Value) ->
    Value;
set0([Key | Rest], {struct, P}, Value)
  when is_binary(Key) orelse Key == 'EJ_DELETE' ->
    case {get_value(Key, P), length(Rest), Value} of
        {undefined, Len, _} when Len > 0 ->
            erlang:error({no_path, Key});
        {_, Len, 'EJ_DELETE'} when Len == 0 ->
            {struct, lists:keydelete(Key, 1, P)};
        {Downstream, _, _} ->
            {struct, lists:keystore(Key, 1, P,
                                    {Key, set0(Rest, Downstream, Value)})}
    end;
set0([Key | Rest], {P}, Value) % clean this up? alt form
  when is_binary(Key) orelse Key == 'EJ_DELETE' ->
    case {get_value(Key, P), length(Rest), Value} of
        {undefined, Len, _} when Len > 0 ->
            erlang:error({no_path, Key});
        {_, Len, 'EJ_DELETE'} when Len == 0 ->
            {lists:keydelete(Key, 1, P)};
        {Downstream, _, _} ->
            {lists:keystore(Key, 1, P,
                            {Key, set0(Rest, Downstream, Value)})}
    end;
set0([new | []], P, Value) when is_list(P) ->
    [Value|P];
set0([Idx | Rest], P, Value)
  when is_integer(Idx) orelse is_atom(Idx); is_list(P) ->
    case {get_value(Idx, P), length(Rest), Value} of
        {undefined, Len, _} when Len > 0 ->
            erlang:error({no_path, Idx});
        {_, Len, 'EJ_DELETE'} when Len == 0 ->
            set_nth(Idx, P, 'EJ_DELETE');
        {Downstream, _, _} ->
            set_nth(Idx, P, set0(Rest, Downstream, Value))
end.

set_nth(first, [_H|T], 'EJ_DELETE') ->
    T;
set_nth(first, [_H|T], V) ->
    [V|T];
set_nth(last, L, 'EJ_DELETE') ->
    [_H|T] = lists:reverse(L),
    lists:reverse(T);
set_nth(last, L, V) ->
    [_H|T] = lists:reverse(L),
    lists:reverse([V|T]);
set_nth(N, L, 'EJ_DELETE') ->
    {L1, [_H|L2]} = lists:split(N - 1, L),
    lists:concat([L1, L2]);
set_nth(N, L, V) ->
    {L1, [_H|L2]} = lists:split(N - 1, L),
    lists:concat([L1, [V|L2]]).


% TODO: support setting list elements as well as a means to add new
% elements to a list.

%% @doc Remove the item specified by `Keys'.
-spec(delete(key_tuple(), json_object()) -> json_object()).

delete(Keys, Obj) when is_tuple(Keys) ->
    set0([ as_binary(X) || X <- tuple_to_list(Keys) ], Obj, 'EJ_DELETE').

%% valid - JSON term validation via spec

%% context threaded through validity checking
-record(spec_ctx, {
          %% List of keys keeping track of where we are in a nested
          %% JSON object.
          path = [] :: [binary()],
          %% Future use: use this to collect errors so that validation
          %% can report a list of errors rather than just the first
          %% one.
          errors = [] :: [term()]
         }).

%% An re module regex (compiled) and a message that will be
%% returned when nomatch is triggered.
-type ej_string_match() :: {'string_match', {re:mp(), _}}.

%% User supplied validation function. This must be an arity 1 fun that
%% will be given the value and should return 'ok' if the value is
%% good. Any other return is treated as an invalid result. The type
%% name describes the expected type of the value. We might want to
%% change this or remove it if we want to support a notion of 'any_of'
%% matching. The advantage for now is that we can auto-generate a
%% better missing message.
-type ej_fun_match() :: {fun_match, {fun((json_term()) -> ok | error),
                                        ej_json_type_name(), _}}.

%% Map a value spec over each element of an array value.
-type ej_array_map() :: {array_map, ej_json_val_spec()}.

%% Walk the key/value pairs of a JSON object and execute the
%% corresponding key and value specs for each pair.
-type ej_object_map() :: {object_map, {{keys, ej_json_val_spec()},
                                       {values, ej_json_val_spec()}}}.

-type ej_json_spec() :: {[ej_json_spec_rule()]}.
-type ej_json_spec_rule() :: {ej_json_key_spec(), ej_json_val_spec()}.
-type ej_json_key_spec() :: binary() | {opt, binary()}.
-type ej_json_val_spec() :: binary()             |
                            ej_json_type_name()  |
                            ej_string_match()    |
                            ej_fun_match()       |
                            ej_array_map()       |
                            ej_object_map()      |
                            {[ej_json_val_spec()]}.

-spec valid(Spec :: ej_json_spec(), Obj:: json_object()) -> ok | #ej_invalid{}.
%% @doc Validate JSON terms. Validity is determined by the
%% `ej_json_spec()` provided which has the shape of EJSON terms but
%% with keys and values describing what is expected. `Obj' is the
%% EJSON term to be validated. This function will return `ok' if all
%% validation rules succeed and a `#ej_invalid{}' record when the
%% first failure is encountered (validation specs are processed in
%% order, depth first).  NOTE: this function is experimental and the
%% API and definition of specs is subject to change.
valid({L}, Obj={OL}) when is_list(L) andalso is_list(OL) ->
    valid(L, Obj, #spec_ctx{});
valid({L}, Obj) when is_list(L) ->
    #ej_invalid{type = json_type, key = undefined,
                expected_type = object,
                found_type = json_type(Obj),
                found = Obj}.

valid([{{Opt, Key}, ValSpec}|Rest], Obj, Ctx = #spec_ctx{path = Path} = Ctx)
  when is_binary(Key) andalso (Opt =:= opt orelse Opt =:= req) ->
    case {Opt, ej:get({Key}, Obj)} of
        {opt, undefined} ->
            valid(Rest, Obj, Ctx);
        {req, undefined} ->
            #ej_invalid{type = missing, key = make_key(Key, Path),
                        expected_type = type_from_spec(ValSpec)};
        {_, Val} ->
            case check_value_spec(Key, ValSpec, Val, Ctx) of
                ok ->
                    valid(Rest, Obj, Ctx);
                Error ->
                    Error
            end
    end;
valid([{Key, ValSpec}|Rest], Obj, #spec_ctx{} = Ctx) when is_binary(Key) ->
    %% required key literal
    valid([{{req, Key}, ValSpec}|Rest], Obj, Ctx);
valid([], _Obj, _Ctx) ->
    ok.

make_path(Key, Path) ->
    list_to_tuple(lists:reverse([Key | Path])).

make_key(Key, Path) ->
    join_path(make_path(Key, Path)).

join_path(Path) ->
    join_bins(tuple_to_list(Path), <<".">>).

type_from_spec({string_match, _}) ->
    string;
type_from_spec({array_map, _}) ->
    array;
type_from_spec({object_map, _}) ->
    object;
type_from_spec({fun_match, {_, Type, _}}) ->
    Type;
type_from_spec(Literal) when is_binary(Literal) ->
    string;
type_from_spec(Literal) when is_integer(Literal) orelse is_float(Literal) ->
    number;
type_from_spec({L}) when is_list(L) ->
    object;
type_from_spec({any_of, {Specs, _ErrorMsg}}) ->
    type_from_any_of(Specs);
type_from_spec(Type) when Type =:= string;
                          Type =:= number;
                          Type =:= boolean;
                          Type =:= array;
                          Type =:= object;
                          Type =:= null;
                          Type =:= any_value ->
    Type;
type_from_spec(Type) ->
    error({unknown_spec, type_from_spec, Type}).

type_from_any_of([]) ->
    any_value;
type_from_any_of([Spec]) ->
    type_from_spec(Spec);
type_from_any_of([Spec|OtherSpecs]) ->
    NewType = type_from_spec(Spec),
    TailType = type_from_any_of(OtherSpecs),
    if
        NewType == TailType -> NewType;
        % TODO we could return an array of all the types if we wanted,
        % which might be more accurate.
        true -> any_value
    end.

json_type(Val) when is_binary(Val) ->
    string;
json_type({L}) when is_list(L) ->
    object;
json_type(L) when is_list(L) ->
    array;
json_type(null) ->
    null;
json_type(Bool) when Bool =:= true; Bool =:= false ->
    boolean;
json_type(N) when is_integer(N) orelse is_float(N) ->
    number.

check_value_spec(Key, {L}, Val={V}, #spec_ctx{path = Path} = Ctx) when is_list(L) andalso is_list(V) ->
    %% traverse nested spec here
    valid(L, Val, Ctx#spec_ctx{path = [Key|Path]});
check_value_spec(Key, {L}, Val, #spec_ctx{path = Path}) when is_list(L) ->
    %% was expecting nested spec, found non-object
    #ej_invalid{type = json_type, key = make_key(Key, Path),
                expected_type = object,
                found = Val,
                found_type = json_type(Val)};
check_value_spec(Key, {string_match, {Regex, Msg}}, Val, #spec_ctx{path = Path}) when is_binary(Val) ->
    %% string_match
    case re:run(Val, Regex) of
        nomatch ->
            #ej_invalid{type = string_match, key = make_key(Key, Path),
                        expected_type = string,
                        found = Val,
                        found_type = string,
                        msg = Msg};
        {match, _} ->
            ok
    end;
check_value_spec(Key, {string_match, _}, Val, #spec_ctx{path = Path}) ->
    %% expected string for string_match, got wrong type
    #ej_invalid{type = json_type, key = make_key(Key, Path),
                expected_type = string,
                found_type = json_type(Val),
                found = Val};

check_value_spec(Key, {fun_match, {Fun, Type, Msg}}, Val, #spec_ctx{path = Path}) ->
    %% user supplied fun
    FoundType = json_type(Val),
    case FoundType =:= Type of
        false ->
            #ej_invalid{type = json_type, key = make_key(Key, Path),
                        expected_type = Type,
                        found_type = FoundType,
                        found = Val};
        true ->
            case Fun(Val) of
                ok ->
                    ok;
                _ ->
                    #ej_invalid{type = fun_match, key = make_key(Key, Path),
                                expected_type = Type,
                                found = Val,
                                found_type = json_type(Val),
                                msg = Msg}
            end
    end;

check_value_spec(Key, {array_map, ItemSpec}, Val, #spec_ctx{path = Path}) when is_list(Val) ->
    case do_array_map(ItemSpec, Val) of
        ok ->
            ok;
        {bad_item, InvalidItem} ->
            #ej_invalid{type = array_elt,
                        key = make_key(Key, Path),
                        expected_type = InvalidItem#ej_invalid.expected_type,
                        found_type = InvalidItem#ej_invalid.found_type,
                        found = InvalidItem#ej_invalid.found,
                        msg = InvalidItem#ej_invalid.msg}
    end;
check_value_spec(Key, {array_map, _ItemSpec}, Val, #spec_ctx{path = Path}) ->
    %% expected an array for array_map, found wrong type
    #ej_invalid{type = json_type, key = make_key(Key, Path),
                expected_type = array,
                found_type = json_type(Val),
                found = Val};

check_value_spec(Key, {object_map, {{keys, KeySpec}, {values, ValSpec}}},
                 Val={L}, #spec_ctx{path = Path}) when is_list(L) ->
    case do_object_map(KeySpec, ValSpec, Val) of
        ok ->
            ok;
        {bad_item, Type, InvalidItem} ->
            #ej_invalid{type = Type,
                        key = make_key(Key, Path),
                        expected_type = InvalidItem#ej_invalid.expected_type,
                        found_type = InvalidItem#ej_invalid.found_type,
                        found = InvalidItem#ej_invalid.found,
                        msg = InvalidItem#ej_invalid.msg}
    end;
check_value_spec(Key, {object_map, _ItemSpec}, Val, #spec_ctx{path = Path}) ->
    %% expected an object for object_map, found wrong type
    #ej_invalid{type = json_type, key = make_key(Key, Path),
                expected_type = object,
                found_type = json_type(Val),
                found = Val};

check_value_spec(Key, {any_of, {Specs, ErrorMsg}}, Val, Ctx) ->
    check_any_of_value_specs(Key, Val, Ctx, Specs, ErrorMsg);

check_value_spec(_Key, any_value, _Val, _Ctx) ->
    ok;

check_value_spec(_Key, string, Val, _Ctx) when is_binary(Val) ->
    ok;
check_value_spec(Key, string, Val, #spec_ctx{path = Path}) ->
    invalid_for_type(string, Val, Key, Path);

check_value_spec(_Key, object, {VL}, _Ctx) when is_list(VL) ->
    ok;
check_value_spec(Key, object, Val, #spec_ctx{path = Path}) ->
    invalid_for_type(object, Val, Key, Path);

check_value_spec(_Key, number, Val, _Ctx) when is_number(Val) ->
    ok;
check_value_spec(Key, number, Val, #spec_ctx{path = Path}) ->
    invalid_for_type(number, Val, Key, Path);

check_value_spec(_Key, array, Val, _Ctx) when is_list(Val) ->
    ok;
check_value_spec(Key, array, Val, #spec_ctx{path = Path}) ->
    invalid_for_type(array, Val, Key, Path);

check_value_spec(_Key, null, null, _Ctx) ->
    ok;
check_value_spec(Key, null, Val, #spec_ctx{path = Path}) ->
    invalid_for_type(null, Val, Key, Path);

check_value_spec(_Key, boolean, Val, _Ctx) when Val =:= true; Val =:= false ->
    ok;
check_value_spec(Key, boolean, Val, #spec_ctx{path = Path}) ->
    invalid_for_type(boolean, Val, Key, Path);

check_value_spec(_Key, Val, Val, _Ctx) when is_binary(Val) ->
    %% exact match desired
    ok;
check_value_spec(Key, SpecVal, Val, #spec_ctx{path = Path}) when is_binary(SpecVal) ->
    %% exact match failed
    #ej_invalid{type = exact,
                key = make_key(Key, Path),
                found = Val,
                expected_type = string,
                found_type = json_type(Val),
                msg = SpecVal};
check_value_spec(Key, SpecVal, Val, #spec_ctx{path = Path}) ->
    %% catch all
    error({unknown_spec, SpecVal, {key, make_key(Key, Path)}, {value, Val}, {path, Path}}).


invalid_for_type(ExpectType, Val, Key, Path) ->
    #ej_invalid{type = json_type,
                expected_type = ExpectType,
                found_type = json_type(Val),
                found = Val,
                key = make_key(Key, Path)}.

do_array_map(_ItemSpec, []) ->
    ok;
do_array_map(ItemSpec, [Item|Rest]) ->
    %% FIXME: do we want to record element index?
    case check_value_spec(<<"item_fake_key">>, ItemSpec, Item, #spec_ctx{}) of
        ok ->
            do_array_map(ItemSpec, Rest);
        Error ->
            {bad_item, Error}
    end.

do_object_map(KeySpec, ValSpec, {L}) when is_list(L) ->
    do_object_map(KeySpec, ValSpec, L);
do_object_map(_KeySpec, _ValSpec, []) ->
    ok;
do_object_map(KeySpec, ValSpec, [{Key, Val}|Rest]) ->
    case check_value_spec(<<"item_fake_key_key">>, KeySpec, Key, #spec_ctx{}) of
        ok ->
            case check_value_spec(<<"item_fake_key_value">>, ValSpec, Val, #spec_ctx{}) of
                ok ->
                    do_object_map(KeySpec, ValSpec, Rest);
                ValueError ->
                    {bad_item, object_value, ValueError}
            end;
        KeyError ->
            {bad_item, object_key, KeyError}
    end.

check_any_of_value_specs(Key, Val, #spec_ctx{path = Path}, [], ErrorMsg) ->
    #ej_invalid{type = any_of,
                key = make_key(Key, Path),
                found = Val,
                expected_type = any_value,
                found_type = json_type(Val),
                msg = ErrorMsg};
check_any_of_value_specs(Key, Val, Ctx, [Spec1|OtherSpecs], ErrorMsg) ->
    case check_value_spec(Key, Spec1, Val, Ctx) of
        ok -> ok;
        _Error -> check_any_of_value_specs(Key, Val, Ctx, OtherSpecs, ErrorMsg)
    end.

join_bins([], _Sep) ->
    <<>>;
join_bins(Bins, Sep) when is_binary(Sep) ->
    join_bins(Bins, Sep, []).

join_bins([B], _Sep, Acc) ->
    iolist_to_binary(lists:reverse([B|Acc]));
join_bins([B|Rest], Sep, Acc) ->
    join_bins(Rest, Sep, [Sep, B | Acc]).


%% end valid
-ifdef(TEST).

ej_test_() ->
{setup,
 fun() ->
         {ok, [Widget]} = file:consult("../test/widget.terms"),
         {ok, [Glossary]} = file:consult("../test/glossary.terms"),
         {ok, [Menu]} = file:consult("../test/menu.terms"),
         ObjList = {struct, [{<<"objects">>,
                              [ {struct, [{<<"id">>, I}]} ||
                                  I <- lists:seq(1, 5) ]}]},
         {Widget, Glossary, Menu, ObjList}
 end,
 fun({Widget, Glossary, Menu, ObjList}) ->
         [{"ej:get",
           [
            ?_assertMatch({struct, [{_, _}|_]}, ej:get({"widget"}, Widget)),
            ?_assertEqual(<<"1">>, ej:get({"widget", "version"}, Widget)),
            ?_assertEqual(250, ej:get({"widget", "image", "hOffset"}, Widget)),
            ?_assertEqual([1,2,3,4,5], ej:get({"widget", "values"}, Widget)),
            ?_assertEqual(2, ej:get({"widget", "values", 2}, Widget)),
            ?_assertEqual(4, ej:get({"widget", "values", 4}, Widget)),
            ?_assertEqual(1, ej:get({"widget", "values", first}, Widget)),
            ?_assertEqual(5, ej:get({"widget", "values", last}, Widget)),
            ?_assertEqual({struct, [{<<"id">>, 5}]},
                          ej:get({<<"objects">>, last}, ObjList)),
            ?_assertEqual({struct, [{<<"id">>, 1}]},
                          ej:get({<<"objects">>, first}, ObjList)),
            ?_assertEqual(undefined, ej:get({"fizzle"}, Widget)),
            ?_assertEqual(undefined, ej:get({"widget", "fizzle"}, Widget)),
            ?_assertEqual(undefined,
                          ej:get({"widget", "values", "fizzle"},Widget)),

            ?_assertEqual(<<"SGML">>,
                          ej:get({"glossary", "GlossDiv", "GlossList",
                                  "GlossEntry", "Acronym"}, Glossary)),

            ?_assertEqual(undefined,
                          ej:get({"glossary", "GlossDiv", "GlossList",
                                  "GlossEntry", "fizzle"}, Glossary)),

            ?_assertEqual(undefined,
                          ej:get({"not_present"}, {[]})),

            ?_assertEqual(undefined,
                          ej:get({"not_present"}, {struct, []})),


            ?_assertException(error, {index_for_non_list, _},
                              ej:get({"glossary", "GlossDiv", "GlossList",
                                      "GlossEntry", 1}, Glossary)),

            ?_assertException(error, {index_for_non_list, _},
                              ej:get({"glossary", "title", 1}, Glossary))]},

          {"ej:get with default",
           [
            ?_assertEqual(<<"1">>, ej:get({"widget", "version"}, Widget, "you'll never see this default")),
            ?_assertEqual(<<"defaults rock">>, ej:get({"widget", "NOT_PRESENT"}, Widget, <<"defaults rock">>))
           ]},

          {"ej:get with json_plist",
           [
            ?_assertEqual(<<"1">>, ej:get({"a"}, [{<<"a">>, <<"1">>}])),
            ?_assertEqual(undefined, ej:get({"x"}, [{<<"a">>, <<"1">>}])),
            ?_assertEqual(undefined, ej:get({"x"}, []))
           ]},

          {"ej:set, replacing existing value",
           fun() ->
                   Path = {"widget", "window", "name"},
                   CurrentValue = ej:get(Path, Widget),
                   NewValue = <<"bob">>,
                   ?assert(NewValue /= CurrentValue),
                   Widget1 = ej:set(Path, Widget, NewValue),
                   ?assertEqual(NewValue, ej:get(Path, Widget1)),
                   % make sure the structure hasn't been disturbed
                   Widget2 = ej:set(Path, Widget1, <<"main_window">>),
                   ?assertEqual(Widget, Widget2)
           end},

          {"ej:set, creating new value",
           fun() ->
                   Path = {"widget", "image", "newOffset"},
                   Value = <<"YYY">>,
                   ?assertEqual(undefined, ej:get(Path, Widget)),
                   Widget1 = ej:set(Path, Widget, Value),
                   ?assertEqual(Value, ej:get(Path, Widget1))
           end},

          {"ej:set, missing intermediate path",
           fun() ->
                   Path = {"widget", "middle", "nOffset"},
                   Value = <<"YYY">>,
                   ?assertEqual(undefined, ej:get(Path, Widget)),
                   ?assertException(error, {no_path, _},
                                    ej:set(Path, Widget, Value))
           end},

          {"ej:set top-level",
           fun() ->
                   OrigVal = ej:get({"widget", "version"}, Widget),
                   NewVal = <<"2">>,
                   NewWidget = ej:set({"widget", "version"}, Widget, NewVal),
                   ?assertEqual(NewVal, ej:get({"widget", "version"}, NewWidget)),
                   Reset = ej:set({"widget", "version"}, NewWidget, OrigVal),
                   ?assertEqual(Widget, Reset)
           end},

          {"ej:set nested",
           fun() ->
                   NewVal = <<"JSON">>,
                   Path = {"glossary", "GlossDiv", "GlossList", "GlossEntry",
                           "ID"},
                   Unchanged = ej:get({"glossary", "GlossDiv", "GlossList",
                                       "GlossEntry", "SortAs"}, Glossary),
                   Glossary1 = ej:set(Path, Glossary, NewVal),
                   ?assertEqual(NewVal, ej:get(Path, Glossary1)),
                   ?assertEqual(Unchanged, ej:get({"glossary", "GlossDiv",
                                                   "GlossList", "GlossEntry",
                                                   "SortAs"}, Glossary1)),
                   Reset = ej:set(Path, Glossary1, <<"SGML">>),
                   ?assertEqual(Glossary, Reset)
           end},

          {"ej:set list element",
           fun() ->
                   Orig = ej:get({"menu", "popup", "menuitem", 2}, Menu),
                   New = ej:set({"onclick"}, Orig, <<"OpenFile()">>),
                   Menu1 = ej:set({"menu", "popup", "menuitem", 2}, Menu, New),
                   ?assertEqual(New,
                                ej:get({"menu", "popup", "menuitem", 2}, Menu1)),
                   Reset = ej:set({"menu", "popup", "menuitem", 2}, Menu1, Orig),
                   ?assertEqual(Menu, Reset)
           end},

          {"ej:set list element path",
           fun() ->
                   Path = {"menu", "popup", "menuitem", 2, "onclick"},
                   Orig = ej:get(Path, Menu),
                   New = <<"OpenFile()">>,
                   Menu1 = ej:set(Path, Menu, New),
                   ?assertEqual(New, ej:get(Path, Menu1)),
                   Reset = ej:set(Path, Menu1, Orig),
                   ?assertEqual(Menu, Reset)
           end},

          {"ej:set list element path first, last",
           fun() ->
                   FPath = {"menu", "popup", "menuitem", first, "value"},
                   LPath = {"menu", "popup", "menuitem", last, "value"},
                   FMenu = ej:set(FPath, Menu, <<"create">>),
                   LMenu = ej:set(LPath, FMenu, <<"kill">>),
                   ?assertEqual(<<"create">>, ej:get(FPath, FMenu)),
                   ?assertEqual(<<"create">>, ej:get(FPath, LMenu)),
                   ?assertEqual(<<"kill">>, ej:get(LPath, LMenu))
           end},

          {"ej:set new list element",
           fun() ->
                   Path = {"menu", "popup", "menuitem", new},
                   Path1 = {"menu", "popup", "menuitem", first},
                   Menu1 = ej:set(Path, Menu, <<"first-item">>),
                   ?assertEqual(<<"first-item">>, ej:get(Path1, Menu1)),
                   List = ej:get({"menu", "popup", "menuitem"}, Menu1),
                   ?assertEqual(4, length(List))
           end},

          {"ej:remove",
           fun() ->
                   Path = {"glossary", "GlossDiv", "GlossList", "GlossEntry", "Abbrev"},
                   Orig = ej:get(Path, Glossary),
                   ?assert(undefined /= Orig),
                   Glossary1 = ej:delete(Path, Glossary),
                   ?assertEqual(undefined, ej:get(Path, Glossary1)),
                   % verify some structure
                   ?assertEqual(<<"SGML">>, ej:get({"glossary", "GlossDiv",
                                                    "GlossList", "GlossEntry",
                                                    "Acronym"}, Glossary1)),
                   ?assertEqual(<<"S">>, ej:get({"glossary", "GlossDiv",
                                                 "title"}, Glossary1))
           end}
         ]
 end
}.

-endif.
