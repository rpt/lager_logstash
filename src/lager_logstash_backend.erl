%% Copyright (c) 2014 Krzysztof Rutka
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.

%% @author Krzysztof Rutka <krzysztof.rutka@gmail.com>
-module(lager_logstash_backend).
-behaviour(gen_event).

-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(DEFAULT_LEVEL, info).
-define(DEFAULT_OUTPUT, {tcp, "localhost", 5000}).
-define(DEFAULT_FORMAT, json).
-define(DEFAULT_ENCODER, jsx).

-type handle() :: gen_tcp:socket() | gen_udp:socket() | file:fd().
-type output() :: tcp() | udp() | file().
-type tcp() :: {tcp, inet:hostname(), inet:port_number()}.
-type udp() :: {udp, inet:hostname(), inet:port_number()}.
-type file() :: {file, string()}.
-type format() :: json.
-type json_encoder() :: jsx | jiffy.

-record(state, {
          handle :: handle() | undefined,
          level :: lager:log_level_number(),
          output :: output(),
          format :: format(),
          json_encoder :: json_encoder()
         }).

init(Args) ->
    Level = arg(level, Args, ?DEFAULT_LEVEL),
    LevelNumber = lager_util:level_to_num(Level),
    Output = arg(output, Args, ?DEFAULT_OUTPUT),
    Format = arg(format, Args, ?DEFAULT_FORMAT),
    Encoder = arg(json_encoder, Args, ?DEFAULT_ENCODER),

    Handle = connect(Output),

    {ok, #state{handle = Handle,
                output = Output,
                format = Format,
                json_encoder = Encoder,
                level = LevelNumber}}.

arg(Name, Args, Default) ->
    case lists:keyfind(Name, 1, Args) of
        {Name, Value} -> Value;
        false         -> Default
    end.

connect({tcp, Host, Port}) ->
    Opts = [binary, {active, false}, {keepalive, true}],
    case gen_tcp:connect(Host, Port, Opts) of
        {ok, Socket} -> Socket;
        {error, _}   -> undefined
    end;
connect({udp, _, _}) ->
    Opts = [binary],
    case gen_udp:open(0, Opts) of
        {ok, Socket} -> Socket;
        {error, _}   -> undefined
    end;
connect({file, Path}) ->
    case file:open(Path, [append]) of
        {ok, Fd}   -> Fd;
        {error, _} -> undefined
    end.

handle_call({set_loglevel, Level}, State) ->
    LevelNumber = lager_util:level_to_num(Level),
    {ok, ok, State#state{level = LevelNumber}};
handle_call(get_loglevel, #state{level = LevelNumber} = State) ->
    Level = lager_util:num_to_level(LevelNumber),
    {ok, Level, State};
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({log, _}, #state{handle = undefined} = State) ->
    {ok, State};
handle_event({log, Message}, State) ->
    _ = handle_log(Message, State),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_log(LagerMsg, #state{level = Level,
                            format = Format,
                            json_encoder = Encoder} = State) ->
    Severity = lager_msg:severity(LagerMsg),
    case lager_util:level_to_num(Severity) =< Level of
        true ->
            Config = [{json_encoder, Encoder}],
            Payload = format(Format, LagerMsg, Config),
            send_log(Payload, State);
        false -> skip
    end.

format(json, Message, Config) ->
    lager_logstash_json_formatter:format(Message, Config).

send_log(Payload, #state{output = {tcp, _, _}, handle = Socket}) ->
    ok = gen_tcp:send(Socket, Payload);
send_log(Payload, #state{output = {udp, Host, Port}, handle = Socket}) ->
    ok = gen_udp:send(Socket, Host, Port, Payload);
send_log(Payload, #state{output = {file, _}, handle = Fd}) ->
    ok = file:write(Fd, Payload).

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, #state{handle = undefined}) -> ok;
terminate(_Reason, #state{output = {tcp, _, _}, handle = Socket}) ->
    ok = gen_tcp:close(Socket);
terminate(_Reason, #state{output = {udp, _, _}, handle = Socket}) ->
    ok = gen_udp:close(Socket);
terminate(_Reason, #state{output = {file, _}, handle = Fd}) ->
    ok = file:close(Fd).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
