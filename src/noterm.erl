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

%% @doc Terminal emulator for `nosh'.
%% Translates standard I/O to Erlang messaging.
%%
%% <i>Full terminal emulation has yet to be implemented.</i>
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012, 2013 Beads D. Land-Trujillo

%% TODO: implement "echo" as dash-style option, not parameter
%% TODO: escript and parameters (make it run like any other shell command)
%% TODO: make fully redistributable (Win/cygwin/*NIX)
%% TODO: incorporate full terminfo/ncurses support
%% TODO: notermd - telent/ssh access

%% @version 0.1.6
-module(noterm).
-version("0.1.6").

%%
%% Include files
%%

%-define(debug, true).
-include_lib("pose/include/interface.hrl").
-include_lib("pose/include/macro.hrl").

%%
%% Exported functions
%%

-behaviour(gen_command).

% API entry points
-export([start/0, start/1, run/3]).

% private callbacks
-export([do_run/2]).

% private fully-qualified loop
-export([loop/1]).

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
do_run(IO, ARG) ->
  ?STDOUT("Starting Noterm ~s terminal emulator on ~p ~p~n",
          [?VERSION(?MODULE), node(), self()]),
  error_logger:tty(false),

  Command = keyin,
  case gen_command:load_command(IO, Command) of
    {module, Module}    ->
      KeyPid = spawn_link(Module, run, [?IO(self()), ?ARG(keyin), ?ENV]),
      do_run(IO, ARG, KeyPid);
    {error, What}       ->
      exit({Command, What})
  end.

% We have a keyboard listener, now load our shell.
do_run(IO, ARG, KeyPid) ->
  case ?ARGV(1) of
    echo    -> ?STDOUT("Shell echo flag enabled.\n"),
               NoshIO = ?IO(self(), self(), self(), true, true);
    _Else   -> NoshIO = ?IO(self(), true)
  end,

  Command = nosh,
  case gen_command:load_command(IO, Command) of
    {module, Module}    ->
      NoshPid = spawn_link(Module, run, [NoshIO, ?ARG(Command), ?ENV]),
      ?MODULE:loop(?IO(KeyPid, NoshPid, NoshPid));
    {error, What}       ->
      exit({Command, What})
  end.

%%
%% Fully-qualified loop
%%

%% @private Iterative loop for passing keyboard input to shell process.
loop(IO) ->
  SelfPid = self(),
  receive
    {purging, _Pid, _Mod}		-> ?MODULE:loop(IO);
    {'EXIT', ExitPid, Reason}	-> do_exit(IO, ExitPid, Reason);
    {MsgTag, Stdin, Line}
      when Stdin == IO#std.in 	-> do_keyin(IO, MsgTag, Line);
    {MsgTag, SelfPid, Line}     -> do_output(IO, MsgTag, Line);
    {MsgTag, Stdout, Line}
      when Stdout == IO#std.out	-> do_output(IO, MsgTag, Line);
    {MsgTag, Stderr, Line}
      when Stderr == IO#std.err	-> do_output(IO, MsgTag, Line);
    Noise						-> do_noise(IO, Noise)
  end.

%%
%% Local functions
%%

% Handle output.
do_output(IO, MsgTag, Output) ->
  case MsgTag of
    stdout	-> io:format("~s", [Output]);
    erlout	-> io:format("data: ~p~n", [Output]);
    erlerr	-> Erlerr = ?FORMAT_ERLERR(Output),
               io:format(standard_error, "** ~s~n", [Erlerr]);
    stderr	-> io:format(standard_error, "** ~s", [Output]);
    debug	-> io:format(standard_error, "-- ~s", [Output])
  end,
  ?MODULE:loop(IO).

% Handle keyboard process messages.
do_keyin(IO, MsgTag, Line) ->
  Clean = strip_escapes(Line),
  case MsgTag of
    stdout  -> ?STDOUT(Clean);
    stderr	-> io:format(standard_error, "** ~s", [Line]);
    debug   -> io:format(standard_error, "-- ~s", [Line])
  end,
  ?MODULE:loop(IO).

% Handle exit signals.
do_exit(IO, ExitPid, Reason) ->
  case ExitPid of
    Stdin when Stdin == IO#std.in		->
      ?DEBUG("Stopping shell on keyboard exit: ~p~n", [Reason]),
      ?STDOUT("stop\n"),
      ?MODULE:loop(IO);
    Stdout when Stdout == IO#std.out	->
      grace("Stopping terminal on shell exit", Reason),
      exit(ok);
    OtherPid                            ->
      ?DEBUG("Saw ~p exit: ~s~n", [OtherPid, ?FORMAT_ERLERR(Reason)]),
      ?MODULE:loop(IO)
  end.

% Handle message queue noise.
do_noise(IO, Noise) ->
  io:format(standard_error, "noise: ~p ~p~n", [Noise, self()]),
  ?MODULE:loop(IO).

grace(Message, Reason) ->
  io:format(standard_error, "** ~s: ~s~n", [Message, ?FORMAT_ERLERR(Reason)]).

strip_escapes(Subject) ->
  {ok, MP} = re:compile("\e\[[\d,\s]+[A-Z]"),
  re:replace(Subject, MP, "", [global, {return, list}]).
