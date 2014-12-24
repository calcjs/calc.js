-module(dao_util).

-export([list/1]).


%% TODO: implement with skip, limit
list(TableName) ->
    Iterator = fun(Rec, List) ->
        [Rec | List]
    end,
    case mnesia:is_transaction() of
        true -> mnesia:foldl(Iterator, [], TableName);
        false ->
            Exec = fun({Fun, Tab}) -> mnesia:foldl(Fun, [], Tab) end,
            mnesia:activity(transaction, Exec, [{Iterator, TableName}], mnesia_frag)
    end.
