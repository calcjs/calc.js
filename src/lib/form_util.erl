-module(form_util).

-export([get_binary_string_value/2]).
-export([get_boolean_value/2]).
-export([get_integer_value/2]).
-export([get_timestamp/2]).


-spec get_binary_string_value(Name, [{atom(), any()}]) -> iodata() when Name::list();
                             (Name, [{atom(), any()}]) -> iodata() when Name::binary().
get_binary_string_value(Name, Params) when is_list(Name) ->
    get_binary_string_value(iolist_to_binary(Name), Params);
get_binary_string_value(Name, Params) when is_binary(Name) ->
    proplists:get_value(Name, Params).


-spec get_boolean_value(Name, [{atom(), any()}]) -> iodata() when Name::list();
                       (Name, [{atom(), any()}]) -> iodata() when Name::binary().
get_boolean_value(Name, Params) when is_list(Name) ->
    get_boolean_value(iolist_to_binary(Name), Params);
get_boolean_value(Name, Params) when is_binary(Name) ->
    case proplists:get_value(Name, Params) of
        <<"true">> ->
            true;
        <<"on">> ->
            true;
        _ ->
            false
    end.


-spec get_integer_value(Name, [{atom(), any()}]) -> iodata() when Name::list();
                       (Name, [{atom(), any()}]) -> iodata() when Name::list().
get_integer_value(Name, Params) when is_list(Name) ->
    get_integer_value(iolist_to_binary(Name), Params);
get_integer_value(Name, Params) when is_binary(Name) ->
    case proplists:get_value(Name, Params) of
        undefined ->
            undefined;
        <<>> ->
            undefined;
        Value when is_binary(Value) ->
            binary_to_integer(Value);
        Value when is_list(Value) ->
            list_to_integer(Value)
    end.


-spec get_timestamp(Name::list(), [{atom(), any()}]) -> {MegaSecs::integer(), Secs::integer(), MicroSecs::integer()}.
get_timestamp(Name, Params) ->
    {get_integer_value(iolist_to_binary(Name ++ "_megasecs"), Params),
     get_integer_value(iolist_to_binary(Name ++ "_secs"), Params),
     get_integer_value(iolist_to_binary(Name ++ "_microsecs"), Params)
    }.
