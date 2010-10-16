.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -Ddebug -Wall +debug_info -o ../ebin/ $<

.yrl.erl:
	erlc -Ddebug -Wall +debug_info -o ../ebin/ $<

ERL=erl -boot start_clean 

MODS=efs_fuse\
	 efs_fuse_sup\
	 efs_fuse_srv

all:compile

compile:${MODS:%=%.beam}

subdirs:
	@echo "subdirs here ..."

clean:
	rm -rf *.beam
	rm -rf ../ebin/*.beam
	rm -rf ../ebin/*.boot
	rm -rf ../ebin/*.script
	rm -rf erl_crash.dump

