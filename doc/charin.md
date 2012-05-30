

#Module charin#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Keyboard listening `pose` process, passing character sequences
to `stdout`.

Copyright (c) 2012 Beads D. Land-Trujillo

__Behaviours:__ [`gen_command`](gen_command.md).

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__<font color="red">To do</font>__
<br></br>
* <font color="red">swap in in place of keyin</font>
* <font color="red">implement as unique service</font>

<a name="types"></a>

##Data Types##




###<a name="type-env_prop">env_prop()</a>##



	env_prop() = atom() | {atom(), string()}
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#run-3">run/3</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Equivalent to <a href="#start-1"><tt>start([])</tt></a>.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>Start as a blocking function.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="run-3"></a>

###run/3##


	run(IO::#std{in = pid(), out = pid(), err = pid(), echo = boolean()}, ARG::#arg{cmd = atom(), v = list()}, ENV::#env{plist = [<a href="#type-env_prop">env_prop()</a>]}) -> no_return()
<br></br>


<a name="start-0"></a>

###start/0##


	start() -&gt; no_return()
<br></br>


Equivalent to [`start([])`](#start-1).<a name="start-1"></a>

###start/1##


	start(Param::[atom()]) -&gt; no_return()
<br></br>


Start as a blocking function.