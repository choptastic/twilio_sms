-module(twilio_sms).
-export([send/3]).

-spec get_credentials() -> {string(), string()}.
get_credentials() ->
	{ok, Sid} = application:get_env(account_sid),
	{ok, Token} = application:get_env(auth_token),
	{Sid, Token}.

send(From, To, Text) ->
	{Sid, Token} = get_credentials(),
	URL = "https://api.twilio.com/2010-04-01/Accounts/" ++ Sid ++ "/Messages.json",
	
	Body = wf:to_qs([{'From', From}, {'To', To}, {'Body', Text}]),

	Request = [
		URL,
		"",
		"application/x-www-form-urlencoded",
		Body
	],

	HTTPOpts = [
		{proxy_auth, {Sid, Token}}
	],
	Opts = [
		{full_results, false}
	],

	case httpc:request(post, Request, HTTPOpts, Opts) of
		{ok, {200, Result}} ->
			{ok, Result}
	end.

