.SUFFIXES: .erl .beam .yrl

include vsn.mk
VSN = $(EFS_VSN)

.erl.beam:
	erlc -Wall +debug_info $<

.yrl.erl:
	erlc -Wall +debug_info $<

ERL=erl -boot start_clean

MODS=

all:compile

compile:${MODS:%=%.beam} subdirs

subdirs:
	@echo "subdirs here ..."
	make -C src
	make -C priv/api_c
	make -C priv/inter
	make -C priv/fuse/src

clean:
	rm -rf *.dump
	make -C src clean
	make -C priv/api_c clean
	make -C priv/inter clean
	make -C priv/fuse/src clean

