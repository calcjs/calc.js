-module(admin_handler).

-behaviour(cowboy_http_handler).

-include("macro/models.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

-opaque state() :: #state{}.
-export_type([state/0]).


-spec extract_calc_data(N::integer(), Params::[{atom(), any()}]) -> [calc_data()].
extract_calc_data(undefined, _Params) ->
    [];
extract_calc_data(N, Params) when is_integer(N) ->
    extract_calc_data(N, Params, []).

-spec extract_calc_data(N::integer(), Params::[{atom(), any()}], R::list()) -> [calc_data()].
extract_calc_data(0, _Params, R) ->
    lists:keysort(2, R);
extract_calc_data(N, Params, R) when is_integer(N) ->
    %% form_id, order, field_name, repeating, formula, always_exec, label, active
    extract_calc_data(N - 1,
                      Params,
                      [{form_util:get_binary_string_value("form_id_" ++ integer_to_list(N), Params),
                        form_util:get_integer_value("order_" ++ integer_to_list(N), Params),
                        form_util:get_binary_string_value("field_" ++ integer_to_list(N), Params),
                        form_util:get_boolean_value("repeating_" ++ integer_to_list(N), Params),
                        form_util:get_binary_string_value("formula_" ++ integer_to_list(N), Params),
                        form_util:get_boolean_value("always_execute_" ++ integer_to_list(N), Params),
                        form_util:get_boolean_value("label_" ++ integer_to_list(N), Params),
                        form_util:get_boolean_value("active_" ++ integer_to_list(N), Params)} | R]).


-spec extract_dependent_forms(N::integer(), Params::[{atom(), any()}]) -> [dependent_form()].
extract_dependent_forms(undefined, _Params) ->
    [];
extract_dependent_forms(N, Params) when is_integer(N) ->
    extract_dependent_forms(N, Params, []).

-spec extract_dependent_forms(N::integer(), Params::[{atom(), any()}], R::list()) -> [dependent_form()].
extract_dependent_forms(0, _Params, R) ->
    lists:keysort(1, R);
extract_dependent_forms(N, Params, R) ->
    extract_dependent_forms(N - 1,
                            Params,
                            [{form_util:get_binary_string_value("dependent_form_id_" ++ integer_to_list(N), Params),
                              form_util:get_binary_string_value("dep_form_data_retrieval_api_" ++ integer_to_list(N), Params),
                              form_util:get_binary_string_value("dep_form_post_api_" ++ integer_to_list(N), Params),
                              form_util:get_binary_string_value("dep_form_field_info_api_" ++ integer_to_list(N), Params)} | R]).


init(_, Req, _Opts) ->
	{ok, Req, #state{}}.


handle(Req, State=#state{}) ->
    {Domain, Req2} = cowboy_req:binding(domain, Req),
    {Id, Req3} = cowboy_req:binding(id, Req2),
    {Method, Req4} = cowboy_req:method(Req3),
    case Method of
        <<"GET">> ->
            case Domain of
                undefined ->
                    show_admin_list(Req4, State);
                <<"calc">> ->
                    case Id of
                        undefined ->
                            show_list(Req4, State);
                        <<"new">> ->
                            show_new(Req4, State);
                        _ ->
                            show_calc(Req4, State, Id)
                    end;
                <<"conf">> ->
                    show_conf(Req4, State);
                _ ->
                	{ok, Req5} = cowboy_req:reply(200,
                        [{<<"content-type">>, <<"text/plain">>}],
                        <<"not implemented">>,
                        Req4),
                    {ok, Req5, State}
        	end;
        <<"POST">> ->
            case Domain of
                <<"calc">> ->
                    process_calc(Req4, State);
                <<"conf">> ->
                    process_conf(Req4, State);
                _ ->
                	{ok, Req5} = cowboy_req:reply(200,
                        [{<<"content-type">>, <<"text/plain">>}],
                        <<"not implemented">>,
                        Req4),
                    {ok, Req5, State}
        	end;
        <<"DELETE">> ->
            case Domain of
                <<"calc">> ->
                    delete_calc(Req4, State, Id);
                _ ->
                	{ok, Req5} = cowboy_req:reply(200,
                        [{<<"content-type">>, <<"text/plain">>}],
                        <<"not implemented">>,
                        Req4),
                    {ok, Req5, State}
            end
    end.


-spec show_admin_list(Req::cowboy_req:req(), State::state()) -> {ok, Req2::cowboy_req:req(), State::state()}.
show_admin_list(Req, State) ->
    {ok, Body} = admin_list_dtl:render([{data, []}]),
    {ok, Req2} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/html">>}],
        Body,
        Req),
    {ok, Req2, State}.
    

-spec show_list(Req::cowboy_req:req(), State::state()) -> {ok, Req2::cowboy_req:req(), State::state()}.
show_list(Req, State) ->
    {ok, Body} = calc_list_dtl:render([{data, calcjs_util:to_proplist(form_dao:list())}]),
    {ok, Req2} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/html">>}],
        Body,
        Req),
    {ok, Req2, State}.


-spec show_new(Req::cowboy_req:req(), State::state()) -> {ok, Req2::cowboy_req:req(), State::state()}.
show_new(Req, State) ->
    {ok, Body} = calc_edit_dtl:render([{data, [{creation_date, erlang:now()}]}]),
    {ok, Req2} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/html">>}],
        Body,
        Req),
    {ok, Req2, State}.


-spec show_calc(Req::cowboy_req:req(), State::state(), Id::binary()) -> {ok, Req2::cowboy_req:req(), State::state()}.
show_calc(Req, State, Id) ->
    [Data | _] = calcjs_util:to_proplist(form_dao:read(Id)),
    {ok, Body} = calc_edit_dtl:render([{data, Data}]),
    {ok, Req2} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/html">>}],
        Body,
        Req),
    {ok, Req2, State}.

-spec show_conf(Req::cowboy_req:req(), State::state()) -> {ok, Req2::cowboy_req:req(), State::state()}.
show_conf(Req, State) ->
    [Data | _] = calcjs_util:to_proplist(conf_dao:read()),
    {ok, Body} = conf_edit_dtl:render([{data, Data}]),
    {ok, Req2} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/html">>}],
        Body,
        Req),
    {ok, Req2, State}.


-spec process_calc(Req::cowboy_req:req(), State::state()) -> {ok, Req2::cowboy_req:req(), State::state()}.
process_calc(Req, State) ->
    {Length, Req2} = cowboy_req:body_length(Req),
    {ok, Params, Req3} = cowboy_req:body_qs(Req2, [{length, Length}, {read_timeout, 5000}]),
    form_dao:save(%%id
                  proplists:get_value(<<"id">>, Params),
                  %%active
                  proplists:get_value(<<"active">>, Params),
                  %%description
                  proplists:get_value(<<"description">>, Params),
                  %%dependent_forms
                  extract_dependent_forms(form_util:get_integer_value("numDependentForms", Params), Params),
                  %%calc_data
                  extract_calc_data(form_util:get_integer_value("numCalculations", Params), Params),
                  %%data_retrieval_api
                  proplists:get_value(<<"data_retrieval_api">>, Params),
                  %%post_api
                  proplists:get_value(<<"post_api">>, Params),
                  %%field_info_api
                  proplists:get_value(<<"field_info_api">>, Params),
                  %%creation_date
                  form_util:get_timestamp("creation_date", Params)
    ),
    show_list(Req3, State).

-spec process_conf(Req::cowboy_req:req(), State::state()) -> {ok, Req2::cowboy_req:req(), State::state()}.
process_conf(Req, State) ->
    {Length, Req2} = cowboy_req:body_length(Req),
    {ok, Params, Req3} = cowboy_req:body_qs(Req2, [{length, Length}, {read_timeout, 5000}]),
    conf_dao:save(%%id
                  proplists:get_value(<<"input_attribute_type">>, Params)
    ),
    show_admin_list(Req3, State).


-spec delete_calc(Req::cowboy_req:req(), State::state(), Id::binary()) -> {ok, Req2::cowboy_req:req(), State::state()}.
delete_calc(Req, State, Id) ->
    ok = form_dao:delete(Id),
    {ok, Req2} = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/html">>}],
        "",
        Req),
    {ok, Req2, State}.


terminate(_Reason, _Req, _State) ->
	ok.
