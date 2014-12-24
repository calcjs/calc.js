-module(ext_api).

-export([make_get_request/1]).


make_get_request(URL) ->
   Response = hackney:request(get, URL, [], <<>>, []),
   case Response of
       {ok, _StatusCode, _RespHeaders, Client} ->
           Result = hackney:body(Client),
           hackney:close(Client),
           Result;
       {error, Reason} ->
           {error, Reason}
   end.