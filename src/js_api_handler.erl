-module(js_api_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([get_js/2]).

-include("macro/models.hrl").

init(_, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.


content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"javascript">>, '*'}, get_js},
      {{<<"text">>, <<"html">>, '*'}, get_js}
     ], Req, State}.


%allowed_methods(Req, State) ->
%    [<<"GET">>, <<"POST">>].


%charsets_provided() ->
%    ok.


get_js(Req, State) ->
    {FormId, Req2} = cowboy_req:binding(form_id, Req),
    DepObjectId = get_dependent_object_id(Req),
    CalcjsData = get_calcjs_data(FormId),
    Conf = get_conf_data(),
    FieldDataApi = extract_field_data_api(CalcjsData),
    Calcs = extract_calc_data(CalcjsData),
    CurrentFormInfo = get_form_field_info(FieldDataApi),
    DependentForms = extract_dependent_forms(CalcjsData),
    DependentFormsData = get_dependent_forms_data(CalcjsData, DepObjectId),
    DependentFormsCalcs = get_dependent_forms_calcs(FormId, Calcs),
    Data = [{form_id, FormId},
            {conf, Conf},
            {calcs, Calcs},
            {current_form_info, CurrentFormInfo},
            {dependent_forms, DependentForms},
            {dependent_forms_data, DependentFormsData},
            {dependent_forms_calcs, DependentFormsCalcs}
           ],
    {ok, Body} = js_dtl:render(Data),
    {Body, Req2, State}.


%is_authorized(Req, State) ->
%    {ok, Req, State}


%resource_exists(Req, State) ->
%    {ok, Req, State}.


%rest_init(Req, _Options) ->
%    {ok, Req, State}.


%rest_terminate(Req, State) ->
%    ok.


%%
%% Internal methods
%%


get_dependent_object_id(Req) ->
    {DepObjectId, _} = cowboy_req:qs_val(<<"dependent_object_id">>, Req),
    DepObjectId.


get_calcjs_data(FormId) ->
    case form_dao:read(FormId) of 
        [] ->
            [];
        [Data | _] ->
            case Data#calcjs_form.active of
                <<"true">> ->
                    Data;
                <<"on">> ->
                    Data;
                _ ->
                    []
            end
    end.


get_conf_data() ->
    case conf_dao:read() of 
        [] ->
            [];
        [Data | _] ->
            calcjs_util:to_proplist(Data)
    end. 
    
    
extract_field_data_api([]) ->
    <<>>;
extract_field_data_api(CalcjsData) ->
    CalcjsData#calcjs_form.field_info_api.
    

extract_calc_data([]) ->
    [];
extract_calc_data(CalcjsData) ->
    extract_calc_data(CalcjsData#calcjs_form.calc_data, []).

extract_calc_data([], Res) ->
    lists:reverse(Res);
extract_calc_data([Value | Rest], Res) ->
    {FormId, Order, Field, Repeating, Formula, AlwaysExecute, Label, Active} = Value,
    CalcData = [{form_id, FormId},
                {order, Order},
                {field, Field}, 
                {repeating, Repeating},
                {formula, Formula},
                {always_execute, AlwaysExecute},  
                {label, Label},    
                {active, Active}],
    extract_calc_data(Rest, [CalcData | Res]).


extract_dependent_forms([]) ->
    [];
extract_dependent_forms(CalcjsData) ->
    extract_dependent_forms(CalcjsData#calcjs_form.dependent_forms, []).

extract_dependent_forms([], Res) ->
    lists:reverse(Res);
extract_dependent_forms([{FormId, _, _, _} | Rest], Res) ->
    extract_dependent_forms(Rest, [[{form_id, FormId}] | Res]).
    
    
get_dependent_forms_data([], _) ->
    [];
get_dependent_forms_data(CalcjsData, DepObjectId) ->
    get_dependent_forms_data(CalcjsData#calcjs_form.dependent_forms, DepObjectId, []).

get_dependent_forms_data([], _, Res) ->
    lists:reverse(Res);
get_dependent_forms_data([{FormId, DataRetrievalApi , PostApi, _} | Rest], DepObjectId, Res) ->
    DataRetrievalApiUrl = format_form_data_url(DataRetrievalApi, DepObjectId),
    PostApiUrl = format_form_data_url(PostApi, DepObjectId),
    FieldData = get_form_field_data(DataRetrievalApiUrl),
    FormData = [{form_id, FormId},
                {data_retrieval_url, DataRetrievalApiUrl}, 
                {form_url, PostApiUrl}, 
                {field_data, FieldData}],
    get_dependent_forms_data(Rest, DepObjectId, [FormData | Res]).


format_form_data_url(URL, undefined) ->
    URL;
format_form_data_url(URL, DepObjectId) ->
    binary:replace(URL, <<"{dependent_object_id}">>, DepObjectId).     
    

get_form_field_info(<<>>) ->
    []; 
get_form_field_info(FieldDataAPI) ->
    ReqResult = ext_api:make_get_request(FieldDataAPI),
    case ReqResult of 
        {ok, JsonData} ->
            {Status, Message, Data} = extract_json_data(JsonData, <<"fieldInfo">>),
            [{status, Status},
             {message, Message},
             {data, Data}];
        {error, Reason} ->
            [{status, "error"},
             {message, Reason},
             {data, []}]
    end.


get_form_field_data(<<>>) ->
    [];    
get_form_field_data(DataRetrievalAPI) ->
    ReqResult = ext_api:make_get_request(DataRetrievalAPI),
    case ReqResult of 
        {ok, JsonData} ->
            {Status, Message, Data} = extract_json_data(JsonData, <<"data">>),
            case Status of
                "ok" ->
                    {Status2, Message2, Data2} = extract_json_data(JsonData, <<"correctedFields">>),
                    [{status, Status2},
                     {message, Message2},
                     {data, Data},
                     {corrected_fields, Data2}];
                "error" ->
                    [{status, Status},
                     {message, Message},
                     {data, []},
                     {corrected_fields, []}]
            end;
        {error, Reason} ->
            [{status, "error"},
             {message, Reason},
             {data, []},
             {corrected_fields, []}]
    end. 
     
     
extract_json_data(<<>>, _) ->
   {"ok", "", []}; 
extract_json_data(JsonData, Key) ->
    try
        {List} = jiffy:decode(JsonData),
        {_, Data} = lists:keyfind(Key, 1, List),
        {"ok", "", Data} 
    catch
        _ ->
            {"error", "Incorrect JSON format. Missing key: " ++ binary_to_list(Key), []}
    end.


get_dependent_forms_calcs(_, []) ->
    [];
get_dependent_forms_calcs(FormId, Calcs) ->
    get_dependent_forms_calcs(FormId, Calcs, []).

get_dependent_forms_calcs(_, [], Res) ->
    lists:reverse(Res);
get_dependent_forms_calcs(FormId, [Data | Rest], Res) ->
    CalcFormId = proplists:get_value(form_id, Data),
    Active = proplists:get_value(active, Data),
    case FormId of
        CalcFormId ->
            get_dependent_forms_calcs(FormId, Rest, Res);
        _ ->
            case Active of
                true ->
                      CalcjsData = get_calcjs_data(CalcFormId),
                      Calcs = extract_calc_data(CalcjsData),
                      CalcData = [{form_id, CalcFormId},
                                  {calcs, Calcs}],
                      get_dependent_forms_calcs(FormId, Rest, [CalcData | Res]);
                false ->
                    get_dependent_forms_calcs(FormId, Rest, Res)
            end
    end.
