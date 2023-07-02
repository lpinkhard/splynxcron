%%%-------------------------------------------------------------------
%%% @author Lionel Pinkhard
%%% @doc
%%% Handles FNB inContact notifications and allocates payments
%%% @end
%%% Created : 23. Sep 2020 22:42
%%%-------------------------------------------------------------------
-module(payment_process).
-author("Lionel Pinkhard").

%% API
-export([run/0]).

-define(USERNAME, "INSERT_MAIL_USERNAME_HERE").
-define(PASSWORD, "INSERT_MAIL_PASSWORD_HERE").
-define(SERVER, "INSERT_MAIL_SERVER_HERE").

-define(API_KEY, "INSERT_API_KEY_HERE").
-define(API_SECRET, "INSERT_API_SECRET_HERE").

run() ->
  inets:start(),
  ssl:start(),
  {ok, Client} = epop_client:connect(?USERNAME, ?PASSWORD, [{addr,?SERVER},{port,110}]),
  {ok, {NumMsg, _}} = epop_client:stat(Client),
  Payments = splynx:get_payments(?API_KEY, ?API_SECRET),
  process_messages(Client, NumMsg, Payments),
  epop_client:quit(Client),
  ssl:stop(),
  inets:stop(),
  done.

process_messages(_, 0, _) ->
  done;
process_messages(Client, NumMsg, Payments) ->
  {ok, MailContent} = epop_client:bin_retrieve(Client, NumMsg),
  {message, BinHeaderList, _} = epop_message:bin_parse(MailContent),
  {ok, From} = epop_message:find_header(BinHeaderList, <<"From">>),
  {ok, Subject} = epop_message:find_header(BinHeaderList, <<"Subject">>),
  case epop_message:find_header(BinHeaderList, <<"Date">>) of
    {ok, Date} ->
      Success = handle_data(From, Subject, Date, Payments),
      if
        Success ->
          epop_client:delete(Client, NumMsg);
        true ->
          done
      end;
    {error, not_found} ->
      done
  end,

  process_messages(Client, NumMsg - 1, Payments).

handle_values(Capture, Date, Payments) ->
  [_, Amount, Ref] = Capture,
  SRef = re:replace(Ref, "^(Absa Bank )|(Capitec )", "", [global, {return, list}]), 
  splynx:try_post_payment(Payments, SRef, Amount ++ "00", Date, ?API_KEY, ?API_SECRET).

handle_data("<inContact@fnb.co.za>", Subject, Date, Payments) ->
  Validation = "^FNB :-\\) R(.*) paid to cheq a\/c\.\.499126 .* Ref\.(.*)\.  ",
  case re:run(Subject, Validation, [{capture, all, list}]) of
    {match, Captured} -> handle_values(Captured, dates:convert_date(Date), Payments);
    nomatch -> false
  end;
handle_data(_, _, _, _) ->
  false.
