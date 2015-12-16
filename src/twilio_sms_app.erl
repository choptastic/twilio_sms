%% vim: ts=4 sw=4 et
-module(twilio_sms_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    twilio_sms_sup:start_link().

stop(_State) ->
    ok.
