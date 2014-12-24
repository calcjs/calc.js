-module(form_dao).

-include("macro/models.hrl").

-export([delete/1]).
-export([list/0]).
-export([read/1]).
-export([save/9]).


delete(Id) ->
    F = fun() ->
        mnesia:delete({calcjs_form, Id})
    end,
    mnesia:activity(transaction, F).


%% TODO: implement with skip, limit
list() ->
    lists:sort(fun(A, B) -> A#calcjs_form.id =< B#calcjs_form.id end, dao_util:list(calcjs_form)).


read(Id) ->
    F = fun() ->
        mnesia:read({calcjs_form, Id})
    end,
    mnesia:activity(transaction, F).


save(Id, Active, Description, DependentForms, CalcData, DataRetrievalAPI, PostAPI, FieldInfoAPI, CreationDate) ->
    F = fun() ->
        mnesia:write(#calcjs_form{id=Id,
                                  active=Active,
                                  description=Description,
                                  dependent_forms=DependentForms,
                                  calc_data=CalcData,
                                  data_retrieval_api=DataRetrievalAPI,
                                  post_api=PostAPI,
                                  field_info_api=FieldInfoAPI,
                                  creation_date=CreationDate,
                                  last_updated_date=erlang:now()})
    end,
    mnesia:activity(transaction, F).
