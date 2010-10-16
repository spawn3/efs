#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <errno.h>

#include "erl_interface.h"
#include "ei.h"

#include "efs.h"

int erlang_fd = -1;

/* -------------------------------------------------- */
/* API                                                */
/* -------------------------------------------------- */
int efs_init()
{
        int ret, fd;

        erl_init(NULL, 0);
        
        if (erl_connect_init(1, "secretcookie", 0) == -1)
                erl_err_quit("erl_connect_init");

        if ((fd = erl_connect("e1@uss")) < 0)
                erl_err_quit("erl_connect");

        fprintf(stderr, "Connected to e1@uss\n\r");
        erlang_fd = fd;

        return 0;
}

int efs_destroy()
{
        return 0;
}

int efs_create(const char *path, int mode)
{
        int ret;
        ETERM *args;
        ETERM *reply;

        args = erl_format("[~s]", path);

        reply = erl_rpc(erlang_fd, "efs", "create", args);
        if (reply == NULL) {
                ret = EINVAL;
                fprintf(stderr, "error %d\n", ret);
                goto err_ret;
        }

        erl_free_term(reply);
        erl_free_term(args);

        return 0;
err_ret:
        erl_free_term(args);
        return ret;
}

int efs_close(int fd)
{
        return 0;
}

/**
 * test utilities.
 * create fcount files, every file size is fsize.
 */
int efs_shell_ww(int fcount, int fsize)
{
        int ret;
        ETERM *args;
        ETERM *reply;

        args = erl_format("[~i,~i]", fcount, fsize);

        reply = erl_rpc(erlang_fd, "efs_shell", "ww", args);
        if (reply == NULL) {
                ret = EINVAL;
                fprintf(stderr, "error %d\n", ret);
                goto err_ret;
        }

        erl_free_term(reply);
        erl_free_term(args);

        return 0;
err_ret:
        erl_free_term(args);
        return ret;
}

/* -------------------------------------------------- */
/* Entry point                                        */
/* -------------------------------------------------- */
int main(int argc, char **argv)
{
        int ret;

        efs_init();

        ret = efs_create("afile", 0644);

        ret = efs_shell_ww(100, 1024*1024);


        return 0;
}
