-module(calcjs_custom_tags).

-behaviour(erlydtl_library).

-export([date/2]).
-export([hidden_timestamp/2]).
-export([inventory/1]).
-export([js_calc_value/2]).
-export([populate_options/2]).
-export([remove_null/1]).
-export([version/0]).


convert_labels_to_list({Value, Label}) ->
    {Value, binary:bin_to_list(Label)}.


%% Create HTML select options for some proplists defined in the variables module.
%% params:
%%		Value: The value to be selected in the options
%%		ValueList: The lists of {Value, Label} options
create_options(_Value, undefined) ->
    "";
create_options(Value, ValueList) ->
    create_options_(Value, ValueList, []).
create_options_(_, [], R) ->
    R;
create_options_(Value, ValueList, R) ->
    [{Val, Label} | Rest] = ValueList,
    ValString = to_list(Val),
    case Val of
        undefined ->
            create_options_(Value, Rest, R ++ "<option value=\"\">" ++ Label ++ "</option>");
        Value ->
            create_options_(Value, Rest, R ++ "<option value=\"" ++ ValString ++ "\" selected>" ++ Label ++ "</option>");
        _ ->
            create_options_(Value, Rest, R ++ "<option value=\"" ++ ValString ++ "\">" ++ Label ++ "</option>")
    end.


date(Variables, _Options) ->
    case proplists:get_value(value, Variables) of
        {MegaSecs, Secs, MicroSecs} when is_integer(MegaSecs) and
                                         is_integer(Secs) and
                                         is_integer(MicroSecs) ->
            {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
            io_lib:format("<span>UTC ~B/~2..0B/~2..0B ~2..0B:~2..0B:~2..0B</span>", [Year, Month, Day, Hour, Minute, Second]);
         _ ->
             ""
    end.


hidden_timestamp(Variables, _Options) ->
    Name = proplists:get_value(name, Variables),
    case proplists:get_value(value, Variables) of
        {MegaSecs, Secs, MicroSecs} when is_integer(MegaSecs) and
                                         is_integer(Secs) and
                                         is_integer(MicroSecs) ->
            io_lib:format("<input name=\"~s_megasecs\" type=\"hidden\" value=\"~B\" /><input name=\"~s_secs\" type=\"hidden\" value=\"~B\" /><input name=\"~s_microsecs\" type=\"hidden\" value=\"~B\" />", [Name, MegaSecs, Name, Secs, Name, MicroSecs]);
        _ ->
            ""
    end.


inventory(filters) ->
    [remove_null];
inventory(tags) ->
    [date,
     hidden_timestamp,
     js_calc_value,
     populate_options].



js_calc_value(Variables, _Options) ->
    Value = proplists:get_value(value, Variables),
    Repeating = proplists:get_value(repeating, Variables),
    Type = proplists:get_value(type, Variables),
    Output = case Repeating of
        true -> 
            map_calc_js_repeating_value(Value, Type, []);
        _->
            get_calc_js_value(Value, Type)    
    end,
    unicode:characters_to_list(Output).
    


map_calc_js_repeating_value(null, _Type, _Acc) ->
    [$[, $]];
map_calc_js_repeating_value([], _Type, Acc) ->
    [$[, lists:reverse(Acc), $]];
map_calc_js_repeating_value([Value | []], Type, Acc) ->
    map_calc_js_repeating_value([], Type, [get_calc_js_value(Value, Type) | Acc]);
map_calc_js_repeating_value([Value | Rest], Type, Acc) ->
    map_calc_js_repeating_value(Rest, Type, [$,, get_calc_js_value(Value, Type) | Acc]).
    
    
get_calc_js_value(Value, Type) ->
    case Type of 
        0 -> %integer
            case is_integer(Value) of
                true ->
                   integer_to_list(Value);
                false  ->
                   [$", $"] 
            end; 
        1 -> %float
            case is_float(Value) of
                true ->
                   float_to_list(Value);
                false  ->
                   [$", $"]
            end;
        2 -> %string
            case is_binary(Value) of
                true ->
                   [$", binary_to_list(Value), $"];
                false  ->
                   [$", $"]
            end;
        3 -> %boolean
            case is_boolean(Value) of
                true ->
                   atom_to_list(Value);
                false  ->
                   [$", $"]
            end;
        4 -> %zipcode
            case Value of 
                  [[{<<"zip1">>, Zip1}],[{<<"zip2">>, Zip2}]] ->
                      ["{\"zip1\":\"", binary_to_list(Zip1), "\",\"zip2\":\"", binary_to_list(Zip2), "\"}"];
                  _ ->
                    "{}"  
            end;
        5 -> %jdate
            case Value of
                [{<<"era">>, Era}, {<<"year">>, Year}, {<<"month">>, Month}, {<<"day">>, Day}] ->
                    ["{\"era\":\"", integer_to_list(Era), "\",",
                     "\"year\":\"", integer_to_list(Year), "\",",
                     "\"month\":\"", integer_to_list(Month), "\",",
                     "\"day\":\"", integer_to_list(Day), "\"}"];
                 _ ->
                    "{}"  
            end;
        6 -> %yymm jdate
            case Value of
                [{<<"era">>, Era}, {<<"yy">>, Year}, {<<"mm">>, Month}] ->
                    ["{\"era\":\"", integer_to_list(Era), "\",",
                     "\"yy\":\"", integer_to_list(Year), "\",",
                     "\"mm\":\"", integer_to_list(Month), "\"}"];
                _ ->
                    "{}"
            end;
        7 -> %value label 
            case Value of
                [{<<"value">>, Value}, {<<"label">>, Label}] ->
                    ["{\"value\":\"", integer_to_list(Value), "\",",
                     "\"label\":\"", binary_to_list(Label), "\"}"];
                _ ->
                    "{}"
            end;
        8 -> %yy jdate
            case Value of
                [{<<"era">>, Era}, {<<"yy">>, Year}] ->
                    ["{\"era\":\"", integer_to_list(Era), "\",",
                     "\"yy\":\"", integer_to_list(Year), "\"}"];
                _ ->
                    "{}"
            end;
        9 -> %id ref
            case is_binary(Value) of
                true ->
                   [$", binary_to_list(Value), $"];
                false  ->
                   [$", $"]
            end;
        10 -> %tel num
            case Value of
                [{<<"tel1">>, Tel1}, {<<"tel2">>, Tel2}, {<<"tel3">>, Tel3}] ->
                    ["{\"tel1\":\"", binary_to_list(Tel1), "\",",
                     "\"tel2\":\"", binary_to_list(Tel2), "\",",
                     "\"tel3\":\"", binary_to_list(Tel3), "\"}"];
                _ ->
                    "{}"
            end;
        11 -> %mmdd jdate
            case Value of
                [ {<<"mm">>, Month}, {<<"dd">>, Day}] ->
                    ["{\"mm\":\"", integer_to_list(Month), "\",",
                     "\"dd\":\"", integer_to_list(Day), "\"}"];
                _ ->
                    "{}"
            end;
        12 -> %yymmdd jdate
            case Value of
                [{<<"era">>, Era}, {<<"yy">>, Year}, {<<"mm">>, Month}, {<<"dd">>, Day}] ->
                    ["{\"era\":\"", integer_to_list(Era), "\",",
                     "\"yy\":\"", integer_to_list(Year), "\",",
                     "\"mm\":\"", integer_to_list(Month), "\",",
                     "\"dd\":\"", integer_to_list(Day), "\"}"];
                _ ->
                    "{}"
            end;
        _ -> %unknown type
            [$", $"]
    end.


%% populate_options
%% Used to populate options in Django templates.
populate_options(Variables, _Options) ->
    Value = proplists:get_value(value, Variables),
    ValueList = proplists:get_value(valueList, Variables),
    create_options(Value, lists:map(fun convert_labels_to_list/1, ValueList)).


to_list(undefined) ->
    "";
to_list(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_list(Value) when is_float(Value) ->
	Value;
to_list(Value) when is_integer(Value) ->
    integer_to_list(Value);
to_list(Value) when is_binary(Value) ->
    binary_to_list(Value);
to_list(Value) when is_list(Value) ->
    Value.


%% Filters
remove_null(null) ->
    undefined;
remove_null(Value) ->
    Value.


version() ->
    1.
