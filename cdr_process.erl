%%%-------------------------------------------------------------------
%%% @author Lionel Pinkhard
%%% @doc
%%% Handles FNB inContact notifications and allocates payments
%%% @end
%%% Created : 08. Oct 2020 17:31
%%%-------------------------------------------------------------------
-module(cdr_process).
-author("Lionel Pinkhard").

%% API
-export([run/0]).

-define(PBX_KEY, "INSERT_PBX_KEY_HERE").

-define(API_KEY, "INSERT_API_KEY_HERE").
-define(API_SECRET, "INSERT_API_SECRET_HERE").

run() ->
  inets:start(),
  ssl:start(),
  Services = process_services(splynx:voice_services(?API_KEY, ?API_SECRET), maps:new()),
  CDR = voice:retrieve_cdrs(?PBX_KEY),
  ssl:stop(),
  inets:stop(),
  CDR.

process_services([], Map) -> Map;
process_services([H|T], Map) ->
  Phone = binary_to_list(maps:get(<<"phone">>, H)),
  ServiceId = binary_to_list(maps:get(<<"id">>, H)),
  CustId = binary_to_list(maps:get(<<"customer_id">>, H)),

  TmpMap = maps:put("service_id", ServiceId, maps:new()),
  InnerMap = maps:put("customer_id", CustId, TmpMap),

  if
    Phone == null ->
      Map;
    true ->
      process_services(T, maps:put(Phone, InnerMap, Map))
  end.

