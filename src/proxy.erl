%% Copyright (c) 2010, Michael Santos <michael.santos@gmail.com>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%%
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%%
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
-module(proxy).
-export([node/3]).

node({IP, Port}, Proxy, F) ->
    {ok,S} = gen_tcp:listen(Proxy, [
            {packet, 0},
            {active, true},
            {nodelay, true},
            {reuseaddr, true},
            binary
        ]),
    spawn(fun() -> accept(S, IP, Port, F) end).

accept(LSocket, IP, Port, F) ->
    {ok, S} = gen_tcp:accept(LSocket),
    spawn(fun() -> accept(LSocket, IP, Port, F) end),
    error_logger:info_report([{accept, element(2,inet:peername(LSocket))}, {connect, {IP,Port}}]),
    {ok, C} = gen_tcp:connect(IP, Port, [
            {packet, 0},
            binary
        ]),
    xfr(S,C,F).

xfr(Server, Client, F) ->
    receive
        {tcp, Server, Data} ->
            error_logger:info_report([{in, Data}]),
            ok = gen_tcp:send(Client, F(in, Data)),
            xfr(Server,Client, F);
        {tcp_closed, Server} ->
            error_logger:info_report([{in, tcp_close}]),
            ok = gen_tcp:close(Client);
        {tcp_error, Server} ->
            error_logger:info_report([{in, tcp_error}]),
            ok = gen_tcp:close(Client);

        {tcp, Client, Data} ->
            error_logger:info_report([{out, Data}]),
            ok = gen_tcp:send(Server, F(out, Data)),
            xfr(Server,Client, F);
        {tcp_closed, Client} ->
            error_logger:info_report([{out, tcp_close}]),
            ok = gen_tcp:close(Server);
        {tcp_error, Client} ->
            error_logger:info_report([{out, tcp_error}]),
            ok = gen_tcp:close(Server)

    end.


