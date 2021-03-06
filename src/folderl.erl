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
%% Copyright 2012, 2013 Beads D. Land-Trujillo.  All Rights Reserved.
%% -----------------------------------------------------------------------
%% CDDL HEADER END

%% @doc Line-folder for Erlang crashes and other multi-line output.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012, 2013 Beads D. Land-Trujillo

%% @todo Add line folding for Erlang terms.
%% @todo Properly listen to pipelines (get keyboard if no pipe)

%% @version 0.2.3

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

-version("0.2.3").

%%
%% Include files
%%

%-define(debug, true).
-include_lib("pose/include/interface.hrl").

%%
%% Exported functions
%%

-behaviour(gen_command).

% API entry points
-export([start/0, start/1, run/3]).

% private callbacks
-export([do_run/2, until_flush/2]).

% private fully-qualified loop
-export([loop/3]).

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
until_flush([], eof) -> {done, eof, []};
until_flush(ThisFar, CharList) -> {done, ThisFar ++ CharList, []}.

request_flush() ->
  Request = {get_until, unicode, '', ?MODULE, until_flush, []},
  user ! {io_request, self(), user, Request}.

%% @private Callback entry point for gen_command behaviour.
do_run(IO, _ARG) ->
  ?DEBUG("Running folderl ~p~n", [self()]),
  receive
    {stdout, Stdin, eof} when Stdin == IO#std.in -> exit(ok)
  after 100 -> true
  end,
  request_flush(),
  ?MODULE:loop(IO, 80, 0).

%% @private Iterative loop for folding input.
loop(IO, Cols, Count) ->
  receive
    {purging, _Pid, _Mod}                              ->
      ?MODULE:loop(IO, Cols, Count);
    {'EXIT', ExitPid, Reason}                          ->
      do_exit(IO, Cols, Count, ExitPid, Reason);
    {io_reply, user, Reply}                            ->
      do_input(IO, Cols, Count, Reply);
    Noise                                              ->
      ?DEBUG("noise: ~p ~p~n", [Noise, self()]),
      ?MODULE:loop(IO, Cols, Count)
  end.

% Handle exit messages
do_exit(IO, Cols, Count, ExitPid, Reason) ->
  if ExitPid == IO#std.out,
     Reason == ok           -> exit(ok);
     ExitPid == IO#std.out  -> exit(Reason);
     true                   -> ?DEBUG("Saw ~p exit: ~p~n", [ExitPid, Reason]),
                               ?MODULE:loop(IO, Cols, Count)
  end.

% Handle input from io_server
do_input(IO, Cols, _Count, eof) ->
  ?DEBUG("eof\n"),
  do_exit(IO, Cols, 0, IO#std.in, ok);
do_input(IO, Cols, Count, {error, Error}) ->
  ?STDERR({?module, Error}),
  request_flush(),
  ?MODULE:loop(IO, Cols, Count);
do_input(IO, Cols, Count, Data) ->
  ?DEBUG("input/4: ~p~n", [{Cols, Count, Data}]),
  request_flush(),
  case lists:splitwith(fun(X) -> X =/= $\n end, Data) of
    {L, []}         -> do_input(IO, Cols, Count, no_eol, L);
    {L, [$\n | R]}  -> do_input(IO, Cols, Count, R, L)
  end.

% Figure out of lines are long enough to wrap.
do_input(IO, Cols, Count, Buffer, Line) ->
  ?DEBUG("input/5: ~p~n", [{Cols, Count, Line}]),
  Length = string:len(Line) + Count,
  do_input(IO, Cols, Count, Buffer, Line, Length).

% Output each line of input, folding as necessary.
do_input(IO, Cols, Count, Buffer, Line, Length) when Length >= Cols ->
  {Row, Rest} = fold_line(Cols, Count, Line),
  ?STDOUT("~s~n", [Row]),
  do_input(IO, Cols, 0, Buffer, "   " ++ Rest);
do_input(IO, Cols, _Count, no_eol, Line, Length) ->
  ?STDOUT(Line), 
  ?MODULE:loop(IO, Cols, Length);
do_input(IO, Cols, _Count, [], Line, _Length) ->
  ?STDOUT("~s~n", [Line]), 
  ?MODULE:loop(IO, Cols, 0);
do_input(IO, Cols, _Count, Buffer, Line, _Length) ->
  ?STDOUT("~s~n", [Line]), 
  do_input(IO, Cols, 0, Buffer).
  
% Fold line that has reached maximum column length.
fold_line(Cols, Count, String) ->
  ?DEBUG("fold: ~s~n", [String]),
  {ok, MP} = re:compile("^(.*[\\ \\,])([^\\ \\,]*)\$"),
  String1 = string:substr(String, 1, Cols - Count),
  String2 = string:substr(String, string:len(String1) + 1),
  case re:run(String1, MP, [{capture, [1,2], list}]) of
    nomatch                   -> {String1, String2};
    {match, [Above, Below]}   -> fold_line(Cols, Count, Above, Below, String2)
  end.

fold_line(_Cols, _Count, Above, Below, String2) ->
  case re:run(Above, "^\s*$", [{capture, none}]) of
    nomatch -> {Above, Below ++ String2};
    match   -> {Above ++ Below, String2}
  end.