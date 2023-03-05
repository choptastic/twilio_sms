%% vim: ts=4 sw=4 et
-module(twilio_sms).
-behaviour(gen_server).


-define(SERVER, ?MODULE).
-define(TIMEOUT, 300).

%% API
-export([start_link/0]).
-export([send/2]).
-export([send/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(message, {from, to, text, account_sid, auth_token}).
-record(state, {queue=queue:new()}).

start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send(To, Text) ->
    {Sid, Token, From} = get_credentials(),
    queue(From, To, Text, Sid, Token).

send(From, To, Text) ->
    {Sid, Token, _} = get_credentials(),
    queue(From, To, Text, Sid, Token).

init([]) ->
        {ok, #state{}, ?TIMEOUT}.

handle_call(_Request, _From, State) ->
        Reply = ok,
        {reply, Reply, State, ?TIMEOUT}.

handle_cast({queue, Message}, State=#state{queue=Queue}) ->
    NewQueue = queue:in(Message, Queue),
    NewState = State#state{queue=NewQueue},
    {noreply, NewState, ?TIMEOUT}.

handle_info(timeout, State = #state{queue=Queue}) ->
    NewQueue = case queue:out(Queue) of
        {{value, Message}, NQ} ->
            spawn_send(Message),
            NQ;
        {empty, Queue} ->
            Queue
    end,
    {noreply, State#state{queue=NewQueue}, ?TIMEOUT}.

terminate(_Reason, _State) ->
        ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

get_credentials() ->
    {ok, Sid} = application:get_env(twilio_sms, account_sid),
    {ok, Token} = application:get_env(twilio_sms, auth_token),
    {ok, From} = application:get_env(twilio_sms, default_from),
    {Sid, Token, From}.

queue(From, To, Text, Sid, Token) ->
    spawn(fun() ->
        Parts = split_text(Text, 160),
        lists:foreach(fun(Part) ->
            Message = #message{from=From, to=To, text=Part, account_sid=Sid, auth_token=Token},
            queue(Message),
            timer:sleep(5000)
        end, Parts)
    end).

split_text(Text, Chars) when length(Text) =< Chars ->
    [Text];
split_text(Text, Chars) when length(Text) =< Chars*9 ->
    Parts = sigma:ceiling(length(Text) / (Chars - 6)),
    split_text(Text, Chars, 1, Parts).

split_text(Text, Chars, Part, Parts) when length(Text) < Chars-6 ->
    [format_part(Text, Part, Parts)];
split_text(Text, Chars, Part, Parts) ->
    {This, Rest} = lists:split(Chars - 6, Text),
    [format_part(This, Part, Parts) | split_text(Rest, Chars, Part+1, Parts)].

format_part(Text, Part, Parts) ->
    lists:flatten(io_lib:format("(~p/~p) ~s", [Part, Parts, Text])).
    

queue(Message = #message{}) ->
    gen_server:cast(?MODULE, {queue, Message}).

spawn_send(Message) ->
    spawn(fun() ->
        try
            real_send(Message)
        catch E:T:S ->
            error_logger:error_msg("~p:~p~n~p~n", [E, T, S])
        end
    end).

real_send(#message{from=From, to=To, text=Text, 
                   account_sid=Sid, auth_token=Token}) ->

    URL = "https://" ++ Sid ++ ":" ++ Token ++ "@api.twilio.com/2010-04-01/Accounts/" ++ Sid ++ "/Messages.json",
    
    Body = to_qs([{"From", From}, {"To", To}, {"Body", Text}]),

    Request = {
        URL,
        [],
        "application/x-www-form-urlencoded",
        Body
    },

    HTTPOpts = [
        {proxy_auth, {Sid, Token}},
        {ssl, [{verify,verify_none}]}
    ],
    Opts = [
        {full_result, false}
    ],

    case httpc:request(post, Request, HTTPOpts, Opts) of
        {ok, {201, Result}} ->
            {ok, Result};
        {ok, {Code, ErrorJson}} ->
            error_logger:error_msg("There was an error sending message. HTTP Response Code: ~p~nError Response: ~p", [Code, ErrorJson])
    end.

to_qs(List) ->
    KVs = [escape_kv(KV) || KV <- List],
    binary_to_list(iolist_to_binary(string:join(KVs, "&"))).

escape_kv({Key, Val}) ->
    [edoc_lib:escape_uri(Key), "=", edoc_lib:escape_uri(Val)].
