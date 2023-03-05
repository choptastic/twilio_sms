# twilio_sms

A hyper-minimal library for sending SMS messages

## Add to rebar.config

```erlang
{deps, [
	...
	twilio_sms
]}.
```

## Configure it

In your app.config file:

```erlang
[{twilio_sms, [
	{default_from, "5555555555"},
	{account_sid, "ABCDF1234"},
	{auth_token, "ABCDEF1235"}
]}].
```

## Start it

```erlang
application:ensure_all_started(twilio_sms).
```

## Send with it

```erlang
To = "6666666666",
Message = "Hello from twilio_sms",
twilio_sms:send(To, Message).
```

## Copyright

Copyright 2015-2023 Jesse Gumm

Development Sponsored by [Great States Volleyball](http://gsvb.net)

MIT License

