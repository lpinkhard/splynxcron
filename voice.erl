%%%-------------------------------------------------------------------
%%% @author Lionel Pinkhard
%%% @doc
%%% Handles communication with PBXware API
%%% @end
%%% Created : 08. Oct 2020 16:14
%%%-------------------------------------------------------------------
-module(voice).
-author("Lionel Pinkhard").

%% API
-export([retrieve_tenant_cids/1, retrieve_cdrs/1]).

-define(SERVICE_URL, "INSERT_PBXWARE_URL_HERE").

retrieve_tenant_config(ApiKey, Tenant) ->
  {_, _, Result} = request_json("/?apikey=" ++ ApiKey ++ "&action=pbxware.tenant.configuration&id=" ++ Tenant, get, []),
  jsone:decode(list_to_binary(Result)).

process_tenants(_, [], Map) -> Map;
process_tenants(ApiKey, [H|T], Map) ->
  {Key, Value} = H,
  Tid = binary_to_list(maps:get(<<"tenantcode">>, Value)),
  Config = retrieve_tenant_config(ApiKey, binary_to_list(Key)),
  CidRaw = maps:get(<<"tenantcid">>, Config, <<"">>),
  if
    CidRaw == null ->
      Map;
    true ->
      Cid = re:replace(re:replace(CidRaw, "^00", "", [global, {return, list}]), "^0", "27", [global, {return, list}]),
      process_tenants(ApiKey, T, maps:put(Tid, Cid, Map))
  end.

retrieve_tenant_cids(ApiKey) ->
  {_, _, Result} = request_json("/?apikey=" ++ ApiKey ++ "&action=pbxware.tenant.list", get, []),
  Map = maps:new(),
  process_tenants(ApiKey, maps:to_list(jsone:decode(list_to_binary(Result))), Map).

retrieve_cdrs(ApiKey) ->
  {_, _, Result} = request_json("/?apikey=" ++ ApiKey 
    ++ "&action=pbxware.cdr.download&start=" ++ dates:days_before_flip(31) ++ "&end=" 
    ++ dates:current_date_flip() ++ "&limit=1000", get, []),
  Result.  

request_json(Uri, Method, Data) ->
  case Method of
    get ->
      Request = {?SERVICE_URL ++ Uri, []};
    post ->
      Request = {?SERVICE_URL ++ Uri, [], "application/json", Data}
  end,
  {ok, Result} = httpc:request(Method, Request, [], []),
  Result.
