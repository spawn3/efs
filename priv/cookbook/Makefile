.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -Ddebug -Wall +debug_info -o ../ebin/ $<

.yrl.erl:
	erlc -Ddebug -Wall +debug_info -o ../ebin/ $<

ERL=erl -boot start_clean 

MODS=libefs\
	 efs_mds\
	 efs_mds_model\
	 efs_mds_chkpool\
	 efs_mds_diskpool\
	 efs_cds\
	 efs_cds_model\
	 efs_cds_hb\
	 efs_app\
	 efs_sup\
	 efs_shell\
	 efs

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

