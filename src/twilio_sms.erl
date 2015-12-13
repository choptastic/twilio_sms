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
	Message = #message{from=From, to=To, text=Text, account_sid=Sid, auth_token=Token},
	queue(Message).

queue(Message = #message{}) ->
	gen_server:cast(?MODULE, {queue, Message}).

spawn_send(Message) ->
	spawn(fun() ->
		try
			real_send(Message)
		catch E:T ->
			error_logger:error_msg("~p:~p~n~p~n", [E, T, erlang:get_stacktrace()])
		end
	end).

real_send(#message{from=From, to=To, text=Text, 
				   account_sid=Sid, auth_token=Token}) ->

	URL = "https://" ++ Sid ++ ":" ++ Token ++ "@api.twilio.com/2010-04-01/Accounts/" ++ Sid ++ "/Messages.json",
	
	Body = wf:to_list(wf:to_qs([{'From', From}, {'To', To}, {'Body', Text}])),

	Request = {
		URL,
		[],
		"application/x-www-form-urlencoded",
		Body
	},

	HTTPOpts = [
		{proxy_auth, {Sid, Token}}
	],
	Opts = [
		{full_result, false}
	],

	case httpc:request(post, Request, HTTPOpts, Opts) of
		{ok, {201, Result}} ->
			{ok, Result}
	end.

