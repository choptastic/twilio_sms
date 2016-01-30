# twilio_sms

A hyper-minimal library for sending SMS messages

## Configure it

In your .app file:

```erlang
[{twilio_sms, [
	{default_from, "5555555555"},
	{account_sid, "ABCDF1234"},
	{auth_token, "ABCDEF1235"}
]}].
```

## Start IT

```erlang
application:start(inets). %% inets needs to be started first
application:start(twilio_sms).
```

## Send with it

```erlang
To = "6666666666",
Message = "Hello from twilio_sms",
twilio_sms:send(To, Message).
```

## Copyright

Copyright 2015-2016 Jesse Gumm

Development Sponsored by [Great States Volleyball](http://gsvb.net)

MIT License

