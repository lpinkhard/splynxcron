%%%-------------------------------------------------------------------
%%% @author Lionel Pinkhard
%%% @doc
%%% Handles communication with Splynx API
%%% @end
%%% Created : 24. Sep 2020 12:18
%%%-------------------------------------------------------------------
-module(splynx).
-author("Lionel Pinkhard").

%% API
-export([try_post_payment/6, get_payments/2, voice_services/2]).

-define(SERVICE_URL, "https://portal.bioniq.co.za/api/2.0/admin/").

find_customer(Token, Ref) ->
  RefId = customer_by_ref(Token, string:strip(Ref)),
  if
    RefId /= "" ->
      CustId = RefId;
    Ref == "" ->
      CustId = "";
    true ->
      RefNo = get_number(Ref),
      CustValid = valid_customer(Token, RefNo),
      if
        CustValid ->
          CustId = RefNo;
        true ->
          CustId = ""
      end
  end,
  CustId.

get_payments(Token) ->
  {_, _, Result} = request_json("finance/payments", Token, get, []),
  Response = jsone:decode(list_to_binary(Result)),
  Response.
get_payments(ApiKey, ApiSecret) ->
  get_payments(get_token(ApiKey, ApiSecret)).

check_payment(_, _, _, []) -> false;
check_payment(CustId, Amount, Date, [H|T]) ->
  TestId = binary_to_list(maps:get(<<"customer_id">>, H, <<"">>)),
  TestAmount = binary_to_list(maps:get(<<"amount">>, H, <<"">>)),
  TestDate = binary_to_list(maps:get(<<"date">>, H, <<"">>)),

  DateMatch = dates:date_difference(Date, TestDate) < 7,

  case {TestId, TestAmount, DateMatch} of
    {CustId, Amount, true} ->
      true;
    _ -> check_payment(CustId, Amount, Date, T)
  end.

payment_exists(Payments, CustId, Amount, Date) ->
  check_payment(CustId, Amount, Date, Payments).

post_payment(Token, CustId, Amount, Date, FullRef) ->
  Suffix = integer_to_list(trunc(abs(erlang:system_time(nano_seconds)))),
  Request = jsone:encode(#{
    <<"customer_id">> => list_to_binary(CustId),
    <<"invoice_id">> => <<"0">>,
    <<"request_id">> => <<"0">>,
    <<"payment_type">> => <<"2">>,
    <<"receipt_number">> => list_to_binary(get_number(Date) ++ Suffix),
    <<"date">> => list_to_binary(Date),
    <<"amount">> => list_to_binary(Amount),
    <<"comment">> => <<"inContact Capture">>,
    <<"field_1">> => <<"FNB">>,
    <<"field_2">> => list_to_binary(FullRef),
    <<"field_3">> => <<"">>,
    <<"field_4">> => <<"">>,
    <<"field_5">> => <<"">> 
  }),
  {_, _, Result} = request_json("finance/payments", Token, post, Request),
  Response = jsone:decode(list_to_binary(Result)),
  RetId = maps:get(<<"id">>, Response, -1),
  if
    RetId /= -1 ->
      true;
    true ->
      false
  end.

try_post_payment(Payments, Ref, Amount, Date, ApiKey, ApiSecret) ->
  Token = get_token(ApiKey, ApiSecret),
  CustId = find_customer(Token, string:strip(Ref)),
  if
    CustId /= "" ->
      Exists = payment_exists(Payments, CustId, Amount, Date),
      if
        Exists ->
          true;
        true ->
          post_payment(Token, CustId, Amount, Date, Ref)
      end;
    true ->
      false
  end.
    
get_number(S) ->
  re:replace(re:replace(S, "[^0-9]", "", [global, {return, list}]), "^0*", "", [global, {return, list}]).  

valid_customer(Token, Id) ->
  if
    Id == "" ->
      false;
    true ->
      {_, _, Result} = request_json("customers/customer/" ++ Id, Token, get, []),
      Response = jsone:decode(list_to_binary(Result)),
      RetId = binary_to_list(maps:get(<<"id">>, Response, <<"">>)),
      if
        RetId == "" ->
          Outcome = false;
        true ->
          Outcome = true
      end,
      Outcome
  end.

customer_by_ref(Token, Ref) ->
  Params = "main_attributes%5Bstatus%5D=active&additional_attributes%5Bfnb_ref%5D=" ++ http_uri:encode(Ref),
  {_, _, Result} = request_json("customers/customer?" ++ Params, Token, get, []),
  Response = jsone:decode(list_to_binary(Result)),
  if
    length(Response) == 1 ->
      [Head | _] = Response,
      binary_to_list(maps:get(<<"id">>, Head, <<"">>));
    true ->
      ""
  end.

voice_services(ApiKey, ApiSecret) ->
  Token = get_token(ApiKey, ApiSecret),
  Params = "main_attributes=[status]=active",
  {_, _, Result} = request_json("customers/customer/0/voice-services?" ++ http_uri:encode(Params), Token, get, []),
  jsone:decode(list_to_binary(Result)).

get_token(ApiKey, ApiSecret) ->
  Nonce = integer_to_list(erlang:system_time()),
  Hash = bin_to_hex:bin_to_hex(crypto:hmac(sha256, ApiSecret, Nonce ++ ApiKey)),
  TokenRequest = jsone:encode(#{
    <<"auth_type">> => <<"api_key">>,
    <<"key">> => list_to_binary(ApiKey),
    <<"signature">> => Hash,
    <<"nonce">> => list_to_binary(Nonce)
  }),
  {_, _, Result} = request_json("auth/tokens", post, TokenRequest),
  Response = jsone:decode(list_to_binary(Result)),
  binary_to_list(maps:get(<<"access_token">>, Response)).

request_json(Uri, Token, Method, Data) ->
  case Method of
    get ->
      Request = {?SERVICE_URL ++ Uri,
        [{"Authorization","Splynx-EA (access_token=" ++ Token ++ ")"}]};
    post ->
      Request = {?SERVICE_URL ++ Uri,
        [{"Authorization","Splynx-EA (access_token=" ++ Token ++ ")"}], "application/json", Data}
  end,
  {ok, Result} = httpc:request(Method, Request, [], []),
  Result.
request_json(Uri, Method, Data) ->
  Request = {?SERVICE_URL ++ Uri, [], "application/json", Data},
  {ok, Result} = httpc:request(Method, Request, [], []),
  Result.
