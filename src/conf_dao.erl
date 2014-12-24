-module(conf_dao).

-include("macro/models.hrl").


-export([read/0]).
-export([save/1]).



read() ->
    F = fun() ->
        mnesia:read({calcjs_conf, "conf"})
    end,
    mnesia:activity(transaction, F).


save(InputAttributeType) ->
    F = fun() ->
        mnesia:write(#calcjs_conf{id="conf",
                                  input_attribute_type=InputAttributeType,
                                  last_updated_date=erlang:now()})
    end,
    mnesia:activity(transaction, F).
