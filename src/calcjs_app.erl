-module(calcjs_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).
-export([install/1]).

-include("macro/models.hrl").

install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(calcjs_conf,
            [{attributes, record_info(fields, calcjs_conf)},
             {disc_copies, Nodes}]),
    conf_dao:save("name"),
    mnesia:create_table(calcjs_form,
            [{attributes, record_info(fields, calcjs_form)},
             {disc_copies, Nodes}]),
    
    rpc:multicall(Nodes, application, stop, [mnesia]).
    

start(_Type, _Args) ->
    TableWaitTimeout = application:get_env(table_wait_timeout),
    mnesia:wait_for_tables([calcjs_admin], TableWaitTimeout),
    Dispatch = cowboy_router:compile([
        {'_', [{"/js/:form_id", js_api_handler, []},
               {"/assets/[...]", cowboy_static, {priv_dir, calcjs, "static", [{mimetypes, cow_mimetypes, all}]}}
        ]}
    ]),
    {ok, Port} = application:get_env(http_port),
    cowboy:start_http(calcjs_http, 100, [{port, Port}],
        [{env, [{dispatch, Dispatch}]},
        {onresponse, fun error_hook/4}]
    ),
    AdminDispatch = cowboy_router:compile([
        {'_', [{"/admin/:domain/[:id]", admin_handler, []},
               {"/admin", admin_handler, []},
               {"/assets/[...]", cowboy_static, {priv_dir, calcjs, "static", [{mimetypes, cow_mimetypes, all}]}}
        ]}
    ]),
    {ok, AdminPort} = application:get_env(admin_port),
    cowboy:start_http(calcjs_admin, 100, [{port, AdminPort}],
        [{env, [{dispatch, AdminDispatch}]},
        {onresponse, fun error_hook/4}]
    ),
	calcjs_sup:start_link().

stop(_State) ->
	ok.

error_hook(404, Headers, <<>>, Req) ->
    {Path, Req2} = cowboy_req:path(Req),
    Body = ["404 Not Found: \"", Path,
        "\" is not the path you are looking for.\n"],
    Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
        {<<"content-length">>, integer_to_list(iolist_size(Body))}),
    {ok, Req3} = cowboy_req:reply(404, Headers2, Body, Req2),
    Req3;
error_hook(Code, Headers, <<>>, Req) when is_integer(Code), Code >= 400 ->
    Body = ["HTTP Error ", integer_to_list(Code), $\n],
    Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
        {<<"content-length">>, integer_to_list(iolist_size(Body))}),
    {ok, Req2} = cowboy_req:reply(Code, Headers2, Body, Req),
    Req2;
error_hook(_Code, _Headers, _Body, Req) ->
    Req.
