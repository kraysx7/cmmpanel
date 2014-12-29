#  File:	 Makefile
#  Author:	 Ilya Troshkov
#  Created:	 Fri Mar 30 13:00:34 2012


VSN = 1.0

DESTDIR=/srv/cmmpanel

INSTALL_DIR=$(DESTDIR)
FULL_INSTALL_DIR=$(DESTDIR)

CC		 = gcc

WARNING_OPTIONS  = 
LANGUAGE_OPTIONS = -finput-charset="UTF-8"
COMPILER_OPTIONS = -g -I /usr/local/lib/erlang/usr/include -fPIC

CFLAGS = $(WARNING_OPTIONS) $(LANGUAGE_OPTIONS) $(COMPILER_OPTIONS)
LFLAGS = -L/usr/local/lib/erlang/usr/lib -shared -fpic
######################################################################

CECHO_LIBS = -lpanel -lncursesw -lpthread -lerl_interface -lei
CECHO_SOURCES = c_src/cecho.c
CECHO_OBJECT_FILES   = $(CECHO_SOURCES:.c=.o)

######################################################################

ERL_FILES = $(wildcard src/*.erl)
BEAM_FILES = $(patsubst src/%.erl, ebin/%.beam, $(ERL_FILES))

######################################################################

all: priv/bin priv/bin/cecho $(BEAM_FILES)

install: all
	@[ -n "$(DESTDIR)" ] || (echo "Set DESTDIR before running the install target."; false)
	install -d $(FULL_INSTALL_DIR)/ebin
	install -d $(FULL_INSTALL_DIR)/priv/bin
	install -d $(FULL_INSTALL_DIR)/src
	install -m 644 src/*.app ebin/
	install -m 644 ebin/* $(FULL_INSTALL_DIR)/ebin
	install -m 755 priv/bin/* $(FULL_INSTALL_DIR)/priv/bin
	install -m 644 src/* $(FULL_INSTALL_DIR)/src

ebin/%.beam: src/%.erl ebin
	erlc -o ebin $<

ebin:
	mkdir -p ebin

priv/bin : 
	mkdir -p priv/bin

priv/bin/cecho: $(CECHO_OBJECT_FILES)
	$(CC) -o $@.so $(CECHO_OBJECT_FILES) $(LFLAGS) $(CECHO_LIBS)

clean:
	rm -f  $(BEAM_FILES) 

echo-version:
	@echo $(VSN)
