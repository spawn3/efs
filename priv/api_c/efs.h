#ifndef __EFS_H
#define __EFS_H

int efs_init();
int efs_destroy();

int efs_create(const char *path, int mode);
int efs_close(int fd);

#endif

