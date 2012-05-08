-module(ej_valid_test).

-include_lib("eunit/include/eunit.hrl").

basic_spec(0) ->
    {[{<<"name">>, {string_match, regex_for(basic_name)}},
      {{opt, <<"description">>}, string}
     ]};
basic_spec(1) ->
    {[{<<"name">>, {string_match, regex_for(basic_name)}},
      {{opt, <<"description">>}, string},
      {<<"items">>, {array_map, {string_match, regex_for(item)}}}
     ]};
basic_spec(2) ->
    {[{<<"name">>, {string_match, regex_for(basic_name)}},
      {<<"objects">>, object}
     ]};
basic_spec(e) ->
    {[{<<"name">>, {string_match, regex_for(basic_name)}},
      {{opt, <<"description">>}, string},
      {{opt, <<"json_class">>}, <<"Chef::Basic">>},
      {<<"items">>, {array_map, regex_for(item)}},
      {<<"map_items">>,
       {object_map,
        %% key spec
        {string_match, regex_for(item_name)},
        %% value spec
        {array_map, regex_for(item)}}}
     ]}.

regex_for(_) ->
    Pat = <<"^[[:alpha:]]+$">>,
    {ok, Regex} = re:compile(Pat),
    {Regex, Pat}.

nested_spec(0) ->
    {[{<<"name">>, {string_match, regex_for(name)}},
      {<<"a">>, {[
                  {<<"a_name">>, {string_match, regex_for(name)}},
                  {<<"b">>,
                   {[{<<"b_name">>, {string_match, regex_for(name)}}]}}
                 ]}}
     ]}.

nested_spec_0_test_() ->
    Spec = nested_spec(0),
    {_, RegexMsg} = regex_for(name),
    Tests = [

             %% {input,
             %%  expected}

             {{[{<<"name">>, <<"top">>}]},
              {missing, {<<"a">>}}},

             {{[{<<"name">>, <<"top">>}, {<<"a">>, {[]}}]},
              {missing, {<<"a">>, <<"a_name">>}}}, % <<"a.a_name">>

             {{[{<<"name">>, <<"top">>},
                {<<"a">>,
                 {[{<<"a_name">>, <<"alice">>}]}}
               ]},
              {missing, {<<"a">>, <<"b">>}}},

             {{[{<<"name">>, <<"top">>},
                {<<"a">>,
                 {[{<<"a_name">>, <<"alice">>},
                   {<<"b">>, {[]}}]}}
               ]},
              {missing, {<<"a">>, <<"b">>, <<"b_name">>}}},

             {{[{<<"name">>, <<"top">>},
                {<<"a">>,
                 {[{<<"a_name">>, <<"alice">>},
                   {<<"b">>, <<"BAD">>}]}}
               ]},
              {bad_value, {<<"a">>, <<"b">>}, object}},

             {{[{<<"name">>, <<"top">>},
                {<<"a">>,
                 {[{<<"a_name">>, <<"alice">>},
                   {<<"b">>,
                    {[{<<"b_name">>, <<"___">>}]}}]}}
               ]},
              {bad_value, {<<"a">>, <<"b">>, <<"b_name">>}, RegexMsg}
             },

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
    Spec = basic_spec(0),
    {_, BasicRegexMsg} = regex_for(basic),
    [?_assertEqual(ok, ej:valid(Spec, basic(<<"fred">>))),

     ?_assertEqual({missing, {<<"name">>}},
                   ej:valid(Spec, {[]})),

     ?_assertEqual({bad_value, {<<"name">>}, BasicRegexMsg},
                   ej:valid(Spec, basic(<<"&2%!">>))),

     ?_assertEqual(ok,
                   ej:valid(Spec,
                              basic_with(<<"fred">>,
                                        [{<<"description">>, <<"blah">>}]))),

     ?_assertEqual({bad_value, {<<"description">>}, string},
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
    %% {_, BasicRegexMsg} = regex_for(basic),
    [
     ?_assertEqual({missing, {<<"items">>}},
                   ej:valid(Spec, basic(<<"fred">>))),

     ?_assertEqual({bad_value, {<<"items">>}, array},
                   ej:valid(Spec, BadItemList)),

     ?_assertEqual({bad_value,{<<"items">>},{bad_value,{item_key},string}},
                   ej:valid(Spec, BadItemsType))
    ].

basic_spec_2_test_() ->
    %% tests for simple JSON type validation
    Spec = basic_spec(2),
    [
     ?_assertEqual(ok, ej:valid(Spec,
                                basic_with(<<"fred">>, [{<<"objects">>, {[]}}]))),

     ?_assertEqual({bad_value, {<<"objects">>}, object},
                   ej:valid(Spec,
                            basic_with(<<"fred">>, [{<<"objects">>, []}]))),

     ?_assertEqual({bad_value, {<<"objects">>}, object},
                   ej:valid(Spec,
                            basic_with(<<"fred">>, [{<<"objects">>, <<"blah">>}])))

    ].

literal_key_and_value_test_() ->
    Spec = {[
             {<<"class">>, <<"Memo">>}
            ]},
    [
     ?_assertEqual(ok, ej:valid(Spec, {[{<<"class">>, <<"Memo">>}]})),

     ?_assertEqual({bad_value, {<<"class">>}, <<"Memo">>},
                   ej:valid(Spec, {[{<<"class">>, <<"Blah">>}]})),

     ?_assertEqual({missing, {<<"class">>}},
                   ej:valid(Spec, {[]}))

    ].
