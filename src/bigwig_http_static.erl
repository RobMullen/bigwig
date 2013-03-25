%%
%% show details on the VM, releases, apps, etc.
%%
-module(bigwig_http_static).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

-export([html/1, css/1, js/1]).

-compile(export_all).

init({tcp, http}, Req, []) ->
    {ok, Req, undefined_state};
init({tcp, http}, Req, OnlyFile) ->
    {ok, Req, OnlyFile}.

handle(Req, undefined_state = State) ->
    {Path, Req2} = cowboy_req:path_info(Req), % strip <<"static">>
    %% io:format("~p:handle Path is:~p~nReq2 is:~p~n",[?MODULE,Path, Req2]),
    send(Req2, Path, State);

handle(Req, OnlyFile = State) ->
    %% io:format("~p:handle OnlyFile is:~p~nReq2 is:~p~n",[?MODULE,OnlyFile, Req]),
    send(Req, OnlyFile, State).

send(Req, PathBins, State) ->
    %%    io:format("bigwig_http_static:send processing PathBins:~p~n",[PathBins]),
    Path = [ binary_to_list(P) || P <- PathBins ],
    %%    io:format("bigwig_http_static:send processing Path:~p~n",[Path]),
    FileName = case filename:join(Path) of
                   ["/",BaseName] -> BaseName;
                   Other -> Other
               end,

    case string:substr(FileName, string:rstr(FileName,".")) of
        ".css" ->
            Headers = [{<<"Content-Type">>, <<"text/css">>}];
        ".js" ->
            Headers = [{<<"Content-Type">>, <<"text/javascript">>}];
        ".png" ->
            Headers = [{<<"Content-Type">>, <<"image/png">>}];
        ".gif" ->
            Headers = [{<<"Content-Type">>, <<"image/gif">>}];
        ".jpg" ->
            Headers = [{<<"Content-Type">>, <<"image/jpg">>}];
        _ ->
            Headers = [{<<"Content-Type">>, <<"text/html">>}]
    end,

    case file(FileName) of
        {ok, Body} ->
            {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
            {ok, Req2, State};
        _ ->
            {ok, Req2} = cowboy_req:reply(404, [], <<"404'd">>, Req),
            {ok, Req2, State}
    end.

terminate(_Reason, _Req, _State) ->
  ok.

html(Name) ->
  type("html", Name).
css(Name) ->
  type("css", Name).
js(Name) ->
  type("js", Name).

type(Type, Name) ->
  file(filename:join([Type, Name ++ Type])).

file(Path) ->
  Priv = priv(),
  file:read_file(filename:join(Priv, Path)).

priv() ->
  case code:priv_dir(bigwig) of
    {error,_} -> "priv";
    Priv -> Priv
  end.
