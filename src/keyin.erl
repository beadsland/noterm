%% CDDL HEADER START
%% -----------------------------------------------------------------------
%% The contents of this file are subject to the Common Development and
%% Distribution License, Version 1.0 (the "License"); you may not use
%% this file except in compliance with the License.  You should have
%% received a copy of the Common Development and Distribution License
%% along with this software.  If not, it can be retrieved online at
%% http://www.opensource.org/licenses/CDDL-1.0
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% When distributing Covered Code, include this CDDL Header Notice in
%% each file and include the License file at CDDL-LICENSE.  If applicable
%% add the following below the CDDL Header, with the fields enclosed
%% by brackets replaced by your own identifying information:
%% "Portions Copyright [year] [name of copyright owner]"
%%
%% Copyright 2012 Beads D. Land-Trujillo.  All Rights Reserved
%% -----------------------------------------------------------------------
%% CDDL HEADER END

%% @doc Keyboard listening `pose' process, passing keyboard input to
%% `stdout'.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% @todo io:get_char (see code in jungerl)
%% @todo implement as unique service

-define(module, keyin).

% BEGIN POSE PACKAGE PATTERN
-ifndef(package).
-module(?module).
-package(default).
-else.
-module(?package.?module).
-package(?package).
-endif.
% END POSE PACKAGE PATTERN

%%
%% Include files
%%

%-define(debug, true).
-include("pose/include/interface.hrl").

-import(gen_command).
-import(io).

%%
%% Exported Functions
%%

-behaviour(gen_command).

% API entry points
-export([start/0, start/1, run/3]).

% Hidden callbacks
-export([do_run/2]).

% Hidden fully-qualified loop
-export([loop/1]).

%%
%% API Functions
%%

-spec start() -> no_return().
%% @equiv start([])
start() -> start([]).

-spec start(Param :: [atom()]) -> no_return().
%% @doc Start as a blocking function.
start(Param) -> gen_command:start(Param, ?MODULE).

-spec run(IO :: #std{}, ARG :: #arg{}, ENV :: #env{}) -> no_return().
%% doc Start as a `pose' command.
run(IO, ARG, ENV) -> gen_command:run(IO, ARG, ENV, ?MODULE).

%%
%% Callback Functions
%%

%% @hidden Callback entry point for gen_command behaviour.
do_run(IO, _ARG) ->
  ?DEBUG("Listening to keyboard ~p~n", [self()]),
  ?MODULE:loop(IO).

%%
%% Fully-qualified loop
%%

%% @hidden Iterative loop for keyboard listening and message receiving.
loop(IO) ->
  case io:get_line("") of
    ok              -> do_receive(IO);
    eof             -> do_stop(eof);
    ".\n"           -> do_stop(eof);
    {error, Reason} -> ?STDERR("error: ~p~n", [Reason]);
    Line            -> ?STDOUT(Line)
  end,
  ?MODULE:loop(IO).

%%
%% Local Functions
%%

do_receive(IO) ->
  receive
    {purging, _Pid, _Mod}       ->
      true;
    {'EXIT', Stdin, Reason}
      when Stdin == IO#std.in   ->
      io:format("~p exit: ~p~n", [?MODULE, Reason]);
    Noise                       ->
      ?STDERR("noise: ~p ~p~n", [Noise, self()])
  after
    1 -> true
  end,
  ?MODULE:loop(IO).

do_stop(Reason) ->
  ?DEBUG("Stopping keyboard: ~p~n", [Reason]),
  exit(Reason).