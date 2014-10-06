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
-module(lager_logstash_json_formatter).

-export([format/2]).
-export([format/3]).

-define(DEFAULT_JSON_FORMATTER, jsx).

format(LagerMsg, Config) ->
    Encoder = value(json_encoder, Config, ?DEFAULT_JSON_FORMATTER),
    Level = lager_msg:severity(LagerMsg),
    Timestamp = timestamp(lager_msg:datetime(LagerMsg)),
    Message = lager_msg:message(LagerMsg),
    Metadata = lager_msg:metadata(LagerMsg),
    Data = [{type, lager_logstash},
            {level, Level},
            {'@timestamp', Timestamp},
            {message, Message} | Metadata],
    [encode(Encoder, convert(Data)), $\n].

format(Message, Config, _) ->
    format(Message, Config).

value(Name, Config, Default) ->
    case lists:keyfind(Name, 1, Config) of
        {Name, Value} -> Value;
        false         -> Default
    end.

timestamp({Date, Time}) -> [Date, $T, Time].

convert(Data) -> lists:foldl(fun convert/2, [], Data).

convert({_, undefined}, Acc) -> Acc;
convert({pid, Pid}, Acc) when is_pid(Pid) ->
    [{pid, list_to_binary(pid_to_list(Pid))} | Acc];
convert({K, List}, Acc) when is_list(List) ->
    [{K, iolist_to_binary(List)} | Acc];
convert({K, Atom}, Acc) when is_atom(Atom) ->
    [{K, atom_to_binary(Atom, latin1)} | Acc];
convert(Else, Acc) -> [Else | Acc].

encode(jsx, Data)   -> jsx:encode(Data);
encode(jiffy, Data) -> jiffy:encode({Data}).
