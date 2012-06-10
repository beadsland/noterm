%% CDDL HEADER START    -*-Erlang-*-
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

%% @doc Line-folder for Erlang crashes and other multi-line output.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% @todo Add line folding for Erlang terms.
%% @todo Properly listen to pipelines (get keyboard if no pipe)

%% @version 0.2.0

-define(module, folderl2).

% BEGIN POSE PACKAGE PATTERN
-ifndef(package).
-module(?module).
-package(default).
-else.
-module(?package.?module).
-package(?package).
-endif.
% END POSE PACKAGE PATTERN

-version("0.2.0").

%%
%% Include files
%%

%-define(debug, true).
-include_lib("pose/include/interface.hrl").

-import(gen_command).
-import(lists).
-import(io).
-import(string).

%%
%% Exported functions
%%

-behaviour(gen_command).

% API entry points
-export([start/0, start/1, run/3]).

% private callbacks
-export([do_run/2, until_flushline/2]).

% private fully-qualified loop
-export([loop/4]).

%%
%% API functions
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

%% @private Callback entry point for io_server requests.
until_flushline([], eof) -> {done, eof, []};
until_flushline(ThisFar, eof) -> {done, ThisFar, eof};
until_flushline(ThisFar, CharList) ->
  case lists:splitwith(fun(X) -> X =/= $\n end, CharList) of
    {Left, []}              -> {done, ThisFar ++ Left, []};
    {Left, [$\n | Right]}   -> {done, ThisFar ++ Left ++ "\n", Right}
  end.

request_flushline() ->
  until_flushline("", ""),
  Request = {get_until, unicode, '', ?MODULE, until_flushline, []},
  user ! {io_request, self(), user, Request}.

%% @private Callback entry point for gen_command behaviour.
do_run(IO, _ARG) ->
  ?DEBUG("Running folderl2 ~p~n", [self()]),
  request_flushline(),
  ?MODULE:loop(IO, 80, "", 0).

%% @private Iterative loop for folding input.
loop(IO, Cols, String, Count) when Count >= Cols ->
  do_fold(IO, Cols, String, Count);
loop(IO, Cols, String, Count) ->
  ?DEBUG("loop: ~p~n", [{String, Count}]),
  receive
    {purging, _Pid, _Mod}                              ->
      ?MODULE:loop(IO, Cols, String, Count);
    {'EXIT', ExitPid, Reason}                          ->
      do_exit(IO, Cols, String, Count, ExitPid, Reason);
    {io_reply, user, Reply}                            ->
      do_input(IO, Cols, String, Count, Reply);
    Noise                                              ->
      ?DEBUG("noise: ~p ~p~n", [Noise, self()]),
      ?MODULE:loop(IO, Cols, String, Count)
  after
    100 -> ?STDOUT("~s", [String]),
           ?MODULE:loop(IO, Cols, "", Count)
  end.

% Handle exit messages
do_exit(IO, Cols, String, Count, ExitPid, Reason) ->
  if ExitPid == IO#std.out,
     Reason == ok           -> exit(ok);
     ExitPid == IO#std.out  -> exit(Reason);
     true                   -> ?DEBUG("Saw ~p exit: ~p~n", [ExitPid, Reason]),
                               ?MODULE:loop(IO, Cols, String, Count)
  end.

% Handle input from io_server
do_input(IO, Cols, String, Count, eof) ->
  ?DEBUG("eof\n"),
  ?STDOUT(String),
  do_exit(IO, Cols, String, Count, IO#std.in, ok);
do_input(IO, Cols, String, Count, {error, Error}) ->
  ?STDERR({?module, Error}),
  request_flushline(),
  ?MODULE:loop(IO, Cols, String, Count);
do_input(IO, Cols, String, Count, Data) ->
  ?DEBUG("input: ~p~n", [{String, Count, Data}]),
  request_flushline(),
  NewString = lists:append(String, Data),
  case string:right(NewString, 1) of
    "\n"    -> ?STDOUT(NewString),
               ?MODULE:loop(IO, Cols, "", 0);
    _Else   -> ?STDOUT(NewString),
               ?MODULE:loop(IO, Cols, "", string:len(NewString))
  end.

% Fold lines that have reached maximum column length.
do_fold(IO, Cols, String, Count) ->
  {ok, MP} = re:compile("^(.*[\\ \\,])([^\\ \\,]*)\$"),
  String1 = string:substr(String, 1, Cols - (string:len(String) - Count)),
  String2 = string:substr(String, string:len(String1) + 1),
  case re:run(String1, MP, [{capture, [1,2], list}]) of
    nomatch                 -> ?STDOUT("~s~n", [String]),
                               ?MODULE:loop(IO, Cols, "", 0);
    {match, [Above, Below]} -> ?STDOUT("~s~n", [Above]),
                               NewString = lists:append(["   ",
                                                         Below, String2]),
                               ?MODULE:loop(IO, Cols, NewString,
                                            string:len(NewString))
  end.