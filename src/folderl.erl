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

%% @version 0.1.4

-define(module, folderl).

% BEGIN POSE PACKAGE PATTERN
-ifndef(package).
-module(?module).
-package(default).
-else.
-module(?package.?module).
-package(?package).
-endif.
% END POSE PACKAGE PATTERN

-version("0.1.4").

%%
%% Include files
%%

%-define(debug, true).
-include_lib("pose/include/interface.hrl").

-import(io).
-import(re).
-import(lists).
-import(gen_command).

%%
%% Exported functions
%%

-behaviour(gen_command).

% API entry points
-export([start/0, start/1, run/3]).

% private callbacks
-export([do_run/2]).

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

%% @private Callback entry point for gen_command behaviour.
do_run(IO, _ARG) ->
  ?DEBUG("Running folderl ~p~n", [self()]),
  Command = charin,
  case gen_command:load_command(IO, Command) of
    {module, Module}    ->
      CharPid = spawn_link(Module, run, [?IO(self()), ?ARG(Command), ?ENV]),
      ?DEBUG("Spawned charin ~p~n", [CharPid]),
      ?MODULE:loop(?IO(CharPid), 80, "", 0);
    {error, What}       ->
      exit({Command, What})
  end.

%% @private Iterative loop for folding characters received from `stdin'.
loop(IO, Cols, String, Count) when Count == Cols ->
  do_fold(IO, Cols, String, Count);
loop(IO, Cols, String, Count) ->
  receive
    {purging, _Pid, _Mod}                               ->
      ?MODULE:loop(IO, Cols, String, Count);
    {'EXIT', ExitPid, Reason}                           ->
      do_exit(IO, Cols, String, Count, ExitPid, Reason);
    {MsgTag, OutPid, Payload}                           ->
      do_output(IO, Cols, String, Count, MsgTag, OutPid, Payload);
    Noise                                              ->
      ?DEBUG("noise: ~p ~p~n", [Noise, self()]),
      ?MODULE:loop(IO, Cols, String, Count)
  after
    100 -> io:format("~s", [String]),
           ?MODULE:loop(IO, Cols, "", Count)
  end.

do_exit(IO, Cols, String, Count, ExitPid, Reason) ->
  if ExitPid == IO#std.in,
     Reason == ok           -> exit(ok);
     ExitPid == IO#std.in   -> exit(Reason);
     true                   -> ?MODULE:loop(IO, Cols, String, Count)
  end.

% Fold lines once they reach maximum column length.
do_fold(IO, Cols, String, _Count) ->
  {ok, MP} = re:compile("^(.*[\\ \\,])([^\\ \\,]*)\$"),
  case re:run(String, MP, [{capture, [1,2], list}]) of
    nomatch                 -> io:format("~s~n", [String]),
                               ?MODULE:loop(IO, Cols, "", 0);
    {match, [Above, Below]} -> io:format("~s~n", [Above]),
                               NewString = lists:append("   ", Below),
                               ?MODULE:loop(IO, Cols, NewString,
                                            length(NewString))
  end.

% Handle messages from executing command.
do_output(IO, Cols, String, Count, MsgTag, OutPid, Payload) ->
  case MsgTag of
    stdin when Payload == eof                          ->
      io:format("~s~n", [String]),
      exit(ok);
    stdout when OutPid == IO#std.in, Payload == "\n"   ->
      io:format("~s~n", [String]),
      ?MODULE:loop(IO, Cols, "", 0);
    stdout when OutPid == IO#std.in                    ->
      ?MODULE:loop(IO, Cols, lists:append(String, Payload), Count + 1);
    stderr when OutPid == IO#std.in                    ->
      io:format(standard_error, "** ~s", [Payload]),
      ?MODULE:loop(IO, Cols, String, Count);
    debug when OutPid == self()                        ->
      io:format(standard_error, "-- ~s", [Payload]),
      ?MODULE:loop(IO, Cols, String, Count)
  end.