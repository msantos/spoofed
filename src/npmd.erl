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
%%%
%%% Not the Erlang Port Mapper Daemon
%%%
-module(npmd).
-export([kill/2, names/2, epmd/1]).

kill(IP, Port) ->
    Packet = list_to_binary([<<107,"OK">>]),
    {ok, Socket} = gen_tcp:connect(IP, Port, [
            {packet,2},
            {active, true},
            binary
        ]),
    ok = gen_tcp:send(Socket, Packet),
    wait(Socket).

names(IP, Port) ->
    Packet = list_to_binary([<<110>>]),
    {ok, Socket} = gen_tcp:connect(IP, Port, [
            {packet,2},
            {active, true},
            binary
        ]),
    ok = gen_tcp:send(Socket, Packet),
    wait(Socket).

epmd(Port) ->
    {ok,S} = gen_tcp:listen(4369, [
            {packet, 2},
            {active, true},
            {nodelay, true},
            {reuseaddr, true},
            binary
        ]),
    spawn(fun() -> accept(S, Port) end).

wait(Socket) ->
    inet:setopts(Socket, [{packet, 0}]),
    receive
        {tcp_closed, Socket} ->
            error_logger:info_report([{received, tcp_close}]);
        {tcp_error, Socket} ->
            error_logger:info_report([{received, tcp_error}]);
        {tcp, Socket, <<Port:32, Data/binary>>} ->
            error_logger:info_report([{epmd_port, Port}, {received, binary_to_list(Data)}]),
            wait(Socket)
    end.

accept(LSocket, Port) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> accept(LSocket, Port) end),
    loop(Socket, Port).

loop(Socket, Port) ->
    receive
        % names request
        {tcp, Socket, <<110>>} ->
            inet:setopts(Socket, [{packet, 0}]),
            Response = list_to_binary([
                <<4396:32>>,
                lists:flatten(io_lib:format("I can haz ~s at port ~p~n", ["fake", Port]))
            ]),
            error_logger:info_report([{epmd, names_request}, {response, Response}]),
            ok = gen_tcp:send(Socket, Response);
        % port request
        {tcp, Socket, <<122, Node/binary>>} ->
            inet:setopts(Socket, [{packet, 0}]),
            Response = <<
                119,                    % PORT_PLEASE2_REQ response
                0,                      % Result: no error
                Port:16,                % PortNo
                77,                     % NodeType: normal Erlang node
                0,                      % Protocol: TCP
                0,5,                    % Highest Version
                0,5,                    % Lowest Version
                (byte_size(Node)):16,   % NLen
                Node/bytes,             % NodeName
                0,0                     % ELen
                >>,
            error_logger:info_report([{epmd, Node}, {response, Response}]),
            ok = gen_tcp:send(Socket, Response);
        {tcp_closed, Socket} ->
            error_logger:info_report([{epmd, tcp_close}]);
        {tcp_error, Socket} ->
            error_logger:info_report([{epmd, tcp_error}])
    end.


