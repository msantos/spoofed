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
-module(spoof).
-export([kill/0, kill/2,
        proxy/2, proxy/3,
        epmd/1
    ]).

kill() ->
    kill({127,0,0,1},4369).
kill(IP, Port) ->
    npmd:names(IP, Port),
    npmd:kill(IP, Port),

    % Test if the node is still up.
    % With R14B, epmd will not accept kill
    % requests from non-local clients or
    % when active nodes are connected.
    try npmd:names(IP, Port) of
        _ -> npmd:flood(IP, Port)
    catch
        _:_ -> ok
    end.

epmd(ProxyPort) ->
    npmd:epmd(ProxyPort).

proxy({IP, Port}, Proxy) ->
    F = fun(in,X) -> X; (out,X) -> X end,
    proxy({IP, Port}, Proxy, F);
proxy(Port, Proxy) when is_integer(Port) ->
    F = fun(in,X) -> X; (out,X) -> X end,
    proxy({{127,0,0,1}, Port}, Proxy, F).
proxy({IP, Port}, Proxy, Fun) ->
    proxy:node({IP, Port}, Proxy, Fun);
proxy(Port, Proxy, Fun) when is_integer(Port) ->
    proxy({{127,0,0,1}, Port}, Proxy, Fun).


