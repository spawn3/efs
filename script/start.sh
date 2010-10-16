EFS_HOME=/home/gj/mylove/erlang/efs
erl \
    -pa $EFS_HOME/ebin \
    -pa $EFS_HOME/include \
    -config $EFS_HOME/conf/elog4 \
    -name e1@uss \
    -eval "application:start(efs)"



