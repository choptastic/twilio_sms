# twilio_sms

A hyper-minimal library for sending SMS messages

## Configure it

In your .app file:

```erlang
[{twilio_sms, [
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
From = "555555555",
To = "6666666666",
Message = "Hello from twilio_sms",
twilio_sms:send(From, To, Message).
```

## Copyright

Copyright 2015 Jesse Gumm
MIT License

