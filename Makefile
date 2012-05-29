# -------------------
# Makefile for noterm
# -------------------

# CDDL HEADER START
# -----------------------------------------------------------------------
# The contents of this file are subject to the Common Development and 
# Distribution License, Version 1.0 (the "License"); you may not use 
# this file except in compliance with the License.  You should have 
# received a copy of the Common Development and Distribution License 
# along with this software.  If not, it can be retrieved online at 
# http://www.opensource.org/licenses/CDDL-1.0
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
# the License for the specific language governing rights and limitations
# under the License.
# 
# When distributing Covered Code, include this CDDL Header Notice in
# each file and include the License file at CDDL-LICENSE.  If applicable
# add the following below the CDDL Header, with the fields enclosed
# by brackets replaced by your own identifying information.
# "Portions Copyright [year] [name of copyright owner]"
# 
# Copyright 2012 Beads D. Land-Trujillo.  All Rights Reserved
# -----------------------------------------------------------------------
# CDDL HEADER END

SHELL	= 	/bin/sh

ifeq ($(COMPUTERNAME),GOVMESH-BOOK)
	DEV		=	yes
else
	DEV		=	no
endif

ifeq ($(shell which ping),/cygdrive/c/Windows/system32/ping)
	PING	=	ping -n 1
else
	PING	=	ping -c1
endif

ONLINE	=	`$(PING) www.google.com 2>&1 >/dev/null; \
			if [ "$$?" -eq "0" ]; then (echo yes); \
			else (echo no); fi`
TTY	=	`tty`


SUCCINCT	=	grep -v "Entering directory" \
				| grep -v "Leaving directory"
CROWBAR		=	rebar _cmds_ | $(SUCCINCT) 

POSE	=	-pa deps/pose/ebin
ERL	=	erl -noshell -i deps $(POSE)
POSURE	=	-s pose start posure
SUPERL	=	-s pose start superl
NOTERM	=	-s pose start noterm
STOP	=	-s init stop

BOOT	= 	echo Successful folderl load. | $(FOLD); echo $$?
STRAP	=	cat

FOLDERL	= 	$(ERL) -s pose start folderl $(STOP)
FOLDTEST	= echo Fold | $(FOLD); echo $$?
ifeq (`$(FOLDTEST)`,0)
	FOLD	=  	$(FOLDERL)
else
	FOLD	=	cat
endif

#
# Execution rules start
#

all:		current push-nosh noterm

run:		compile noterm

noterm:	nodump tabs
	@if [ "$(TTY)" == "not a tty" ]; \
		then ($(ERL) $(SUPERL) $(POSURE) $(NOTERM) echo $(STOP)); \
		else ($(ERL) $(SUPERL) $(POSURE) $(NOTERM) $(STOP)); fi

nodump:
	@if [ -e erl_crash.dump ]; then (rm erl_crash.dump); fi

tabs:
	@if [ "$(TTY)" != "not a tty" ]; then (tabs -1 >/dev/null); fi

#
# Build rules start
#

good:	compile
	@$(ERL) $(SUPERL) $(POSURE) $(STOP)

doc:	compile
	
compile:
	@rm -f *.dump doc/*.md doc/*.html
	@$(CROWBAR:_cmds_=compile doc)

current:
	@if [ "$(ONLINE)" == yes ]; then \
		$(CROWBAR:_cmds_=update-deps compile doc); else \
		$(CROWBAR:_cmds_=compile doc); fi

clean: 		online
	@if [ "$(ONLINE)" == yes ]; \
		then (rm -rf deps; rebar clean get-deps | $(SUCCINCT)); \
		else (rebar clean | $(SUCCINCT)); fi
	
online:	
	@if [ "$(ONLINE)" == yes ]; \
		then (echo "Working online"); \
		else (echo "Working offline"); fi

#
# Development rules start
#

push:	online
	@if [ "$(DEV)" == yes -a "$(ONLINE)" == yes ]; \
			then (git push origin master); fi
