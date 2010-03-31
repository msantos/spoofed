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
-module(cookie).
-compile(export_all).

start() ->
    start(10000).
start(N) ->
    timer:tc(?MODULE, loop, [N]).

loop(N) ->
    loop(N,gb_sets:new()).
loop(0,Acc) ->
    {done, gb_sets:size(Acc), gb_sets:to_list(Acc)};
loop(N,Acc) ->
    %R = gen_digest(gen_challenge(), abcd),
    %R = gen_digest(N, abcd),
    R = gen_challenge(),
    case gb_sets:is_member(R, Acc) of
        false ->
            loop(N-1, gb_sets:add_element(R, Acc));
        true ->
            {found, {R, gb_sets:size(Acc)}, gb_sets:to_list(Acc)}
    end.

% Taken from:
%  http://github.com/erlang/otp/blob/dev/lib/kernel/src/dist_util.erl#L366
gen_digest(Challenge, Cookie) when is_integer(Challenge), is_atom(Cookie) ->
    erlang:md5([atom_to_list(Cookie)|integer_to_list(Challenge)]).

gen_challenge() ->
    {A,B,C} = erlang:now(),
    {D,_}   = erlang:statistics(reductions),
    {E,_}   = erlang:statistics(runtime),
    {F,_}   = erlang:statistics(wall_clock),
    {G,H,_} = erlang:statistics(garbage_collection),
    %% A(8) B(16) C(16)
    %% D(16),E(8), F(16) G(8) H(16)
    ( ((A bsl 24) + (E bsl 16) + (G bsl 8) + F) bxor
        (B + (C bsl 16)) bxor
        (D + (H bsl 16)) ) band 16#ffffffff.

