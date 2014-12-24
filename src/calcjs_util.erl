-module(calcjs_util).

-include("macro/models.hrl").

-export([to_proplist/1]).


convert_calc_data(undefined) ->
    [];
convert_calc_data([]) ->
    [];
convert_calc_data(Data) when is_list(Data) ->
    convert_calc_data(Data, []).

convert_calc_data([] , R) ->
    lists:reverse(R);
convert_calc_data([{FormId, Order, Field, Repeating, Formula, AlwaysExecute, Label, Active} | Rest], R) ->
    convert_calc_data(Rest,
                      [[{form_id, FormId},
                        {order, Order},
                        {field, Field}, 
                        {repeating, Repeating},
                        {formula, Formula},
                        {always_execute, AlwaysExecute},  
                        {label, Label},    
                        {active, Active}] | R]).


convert_dependent_forms(undefined) ->
    [];
convert_dependent_forms([]) ->
    [];
convert_dependent_forms(Data) when is_list(Data) ->
    convert_dependent_forms(Data, []).

convert_dependent_forms([], R) ->
    lists:reverse(R);
convert_dependent_forms([{FormId, DataRetrievalAPI, PostAPI, FieldInfoAPI} | Rest], R) ->
    convert_dependent_forms(Rest,
                            [[{form_id, FormId},
                              {data_retrieval_api, DataRetrievalAPI},
                              {post_api, PostAPI},
                              {field_info_api, FieldInfoAPI}] | R]).


to_proplist([]) ->
    [];
to_proplist(Data) when is_list(Data) ->
    lists:foldr(fun(D, R) -> [to_proplist(D) | R] end, [], Data);
to_proplist(Data) when is_record(Data, calcjs_conf)->
    [{input_attribute_type, Data#calcjs_conf.input_attribute_type},
     {last_updated_date, Data#calcjs_conf.last_updated_date}];
to_proplist(Data) when is_record(Data, calcjs_form)->
    [{id, Data#calcjs_form.id},
     {active, Data#calcjs_form.active},
     {description, Data#calcjs_form.description},
     {dependent_forms, convert_dependent_forms(Data#calcjs_form.dependent_forms)},
     {calc_data, convert_calc_data(Data#calcjs_form.calc_data)},
     {data_retrieval_api, Data#calcjs_form.data_retrieval_api},
     {post_api, Data#calcjs_form.post_api},
     {field_info_api, Data#calcjs_form.field_info_api},
     {creation_date, Data#calcjs_form.creation_date},
     {last_updated_date, Data#calcjs_form.last_updated_date},
     {num_calculations, case Data#calcjs_form.calc_data of
                            <<>> ->
                                0;
                            undefined ->
                                0;
                            Val when is_list(Val) ->
                                length(Val);
                            _ ->
                                0
                        end },
    {num_dependent_forms, case Data#calcjs_form.dependent_forms of
                              <<>> ->
                                  0;
                              undefined ->
                                  0;
                              Val when is_list(Val) ->
                                  length(Val);
                              _ ->
                                  0
                          end }].
