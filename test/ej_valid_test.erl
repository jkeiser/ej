-module(ej_valid_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ej/include/ej.hrl").

nested_specs_test_() ->
    Spec = {[{<<"name">>, {string_match, regex_for(name)}},
             {<<"a">>, {[
                         {<<"a_name">>, {string_match, regex_for(name)}},
                         {<<"b">>,
                          {[{<<"b_name">>, {string_match, regex_for(name)}}]}}
                        ]}}
            ]},
    {_, RegexMsg} = regex_for(name),
    Tests = [

             %% {input,
             %%  expected}

             {{[{<<"name">>, <<"top">>}]},
              #ej_invalid{type = missing, key = <<"a">>,
                          expected_type = object}},

             {{[{<<"name">>, <<"top">>}, {<<"a">>, {[]}}]},
              #ej_invalid{type = missing, key = <<"a.a_name">>, expected_type = string}},

             {{[{<<"name">>, <<"top">>},
                {<<"a">>,
                 {[{<<"a_name">>, <<"alice">>}]}}
               ]},
              #ej_invalid{type = missing, key = <<"a.b">>, expected_type = object}},

             {{[{<<"name">>, <<"top">>},
                {<<"a">>,
                 {[{<<"a_name">>, <<"alice">>},
                   {<<"b">>, {[]}}]}}
               ]},
              #ej_invalid{type = missing, key = <<"a.b.b_name">>,
                          expected_type = string}},

             {{[{<<"name">>, <<"top">>},
                {<<"a">>,
                 {[{<<"a_name">>, <<"alice">>},
                   {<<"b">>, <<"BAD">>}]}}
               ]},
              #ej_invalid{type = json_type, key = <<"a.b">>,
                          found = <<"BAD">>, found_type = string,
                          expected_type = object}},

             {{[{<<"name">>, <<"top">>},
                {<<"a">>,
                 {[{<<"a_name">>, <<"alice">>},
                   {<<"b">>,
                    {[{<<"b_name">>, <<"___">>}]}}]}}
               ]},
              #ej_invalid{type = string_match, key = <<"a.b.b_name">>,
                          found = <<"___">>,
                          found_type = string,
                          expected_type = string,
                          msg = RegexMsg}},

             {{[{<<"name">>, <<"top">>},
                {<<"a">>,
                 {[{<<"a_name">>, <<"alice">>},
                   {<<"b">>,
                    {[{<<"b_name">>, <<"bob">>}]}}]}}
               ]},
              ok}
            ],
    [ ?_assertEqual(Expect, ej:valid(Spec, In)) || {In, Expect} <- Tests ].

basic(Name) ->
    {[{<<"name">>, Name}]}.

basic_with(Name, With) ->
    lists:foldl(fun({K, V}, Acc) ->
                        ej:set({K}, Acc, V)
                end, basic(Name), With).

basic_spec_0_test_() ->
    Spec = {[{<<"name">>, {string_match, regex_for(basic_name)}},
             {{opt, <<"description">>}, string}
            ]},
    {_, BasicRegexMsg} = regex_for(basic),
    [?_assertEqual(ok, ej:valid(Spec, basic(<<"fred">>))),

     ?_assertMatch(#ej_invalid{type = missing, key = <<"name">>},
                   ej:valid(Spec, {[]})),

     ?_assertMatch(#ej_invalid{type = string_match,
                               key = <<"name">>,
                               found = <<"&2%!">>,
                               found_type = string,
                               expected_type = string,
                               msg = BasicRegexMsg},
                   ej:valid(Spec, basic(<<"&2%!">>))),

     ?_assertEqual(ok,
                   ej:valid(Spec,
                              basic_with(<<"fred">>,
                                        [{<<"description">>, <<"blah">>}]))),

     ?_assertEqual(#ej_invalid{type = json_type,
                               expected_type = string,
                               found_type = object,
                               found = {[]},
                               key = <<"description">>},
                   ej:valid(Spec,
                              basic_with(<<"fred">>,
                                        [{<<"description">>, {[]}}])))].

basic_spec_1_test_() ->
    Spec = {[{<<"name">>, {string_match, regex_for(basic_name)}},
             {{opt, <<"description">>}, string},
             {<<"items">>, {array_map, {string_match, regex_for(item)}}}
            ]},
    BadItemList = basic_with(<<"fred">>,
                            [{<<"items">>, <<"abc">>}]),
    BadItemsType = basic_with(<<"fred">>,
                              [{<<"items">>, [1, 2, 3]}]),
    [
     ?_assertMatch(#ej_invalid{type = missing, key = <<"items">>},
                   ej:valid(Spec, basic(<<"fred">>))),

     ?_assertEqual(#ej_invalid{type = json_type,
                               expected_type = array,
                               found_type = string,
                               found = <<"abc">>,
                               key = <<"items">>},
                   ej:valid(Spec, BadItemList)),

     ?_assertMatch(#ej_invalid{type = array_elt,
                               expected_type = string,
                               found_type = number,
                               key = <<"items">>},
                   ej:valid(Spec, BadItemsType))
    ].

basic_spec_2_test_() ->
    %% tests for simple JSON type validation
    Spec = {[{<<"name">>, {string_match, regex_for(basic_name)}},
             {<<"objects">>, object}
            ]},
    [
     ?_assertEqual(ok, ej:valid(Spec,
                                basic_with(<<"fred">>, [{<<"objects">>, {[]}}]))),

     ?_assertEqual(#ej_invalid{type = json_type,
                               key = <<"objects">>,
                               expected_type = object,
                               found = [],
                               found_type = array},
                   ej:valid(Spec, basic_with(<<"fred">>, [{<<"objects">>, []}]))),

     ?_assertEqual(#ej_invalid{type = json_type,
                               key = <<"objects">>,
                               expected_type = object,
                               found_type = string,
                               found = <<"blah">>},
                   ej:valid(Spec,
                            basic_with(<<"fred">>, [{<<"objects">>, <<"blah">>}])))

    ].

literal_key_and_value_test_() ->
    Spec = {[
             {<<"class">>, <<"Memo">>}
            ]},
    [
     ?_assertEqual(ok, ej:valid(Spec, {[{<<"class">>, <<"Memo">>}]})),

     ?_assertEqual(#ej_invalid{type = exact,
                               key = <<"class">>,
                               expected_type = string,
                               found_type = string,
                               found = <<"Blah">>,
                               msg = <<"Memo">>},
                   ej:valid(Spec, {[{<<"class">>, <<"Blah">>}]})),

     ?_assertEqual(#ej_invalid{type = missing, key = <<"class">>, expected_type = string},
                   ej:valid(Spec, {[]}))
    ].

object_map_test_() ->
    Spec = {[
             {<<"object">>,
              {object_map,
               {keys, {string_match, regex_for(key)}},
               {values, {string_match, regex_for(value)}}}}
            ]},
        
    Good = {[{<<"object">>,
              {[
                {<<"k1">>, <<"v1">>},
                {<<"k2">>, <<"v2">>},
                {<<"k3">>, <<"v3">>}
               ]}}
            ]},

    GoodEmpty = {[{<<"object">>,
                   {[]}}
                 ]},

    BadKey = {[
               {<<"object">>,
                {[
                  {<<"k1">>, <<"v1">>},
                  {<<"___">>, <<"v2">>},
                  {<<"k3">>, <<"v3">>}
                 ]}}
            ]},

    BadValue = {[
                 {<<"object">>,
                  {[
                    {<<"k1">>, <<"v1">>},
                    {<<"k2">>, <<"v2">>},
                    {<<"k3">>, <<"___">>}
                   ]}}
                ]},

    BadMissing = {[]},

    [
     ?_assertEqual(ok, ej:valid(Spec, Good)),
     ?_assertEqual(ok, ej:valid(Spec, GoodEmpty)),

     ?_assertEqual(#ej_invalid{type = object_key, key = <<"object">>,
                               expected_type = string,
                               found_type = string,
                               found = <<"___">>,
                               msg = <<"^[[:alpha:][:digit:]]+$">>},
                   ej:valid(Spec, BadKey)),

     ?_assertEqual(#ej_invalid{type = object_value, key = <<"object">>,
                               expected_type = string,
                               found_type = string,
                               found = <<"___">>,
                               msg = <<"^[[:alpha:][:digit:]]+$">>},
                   ej:valid(Spec, BadValue)),
     
     ?_assertEqual(#ej_invalid{type = missing, key = <<"object">>,
                               expected_type = object},
                   ej:valid(Spec, BadMissing))
    ].

regex_for(key) ->
    Pat = <<"^[[:alpha:][:digit:]]+$">>,
    {ok, Regex} = re:compile(Pat),
    {Regex, Pat};
regex_for(value) ->
    Pat = <<"^[[:alpha:][:digit:]]+$">>,
    {ok, Regex} = re:compile(Pat),
    {Regex, Pat};
regex_for(_) ->
    Pat = <<"^[[:alpha:]]+$">>,
    {ok, Regex} = re:compile(Pat),
    {Regex, Pat}.

