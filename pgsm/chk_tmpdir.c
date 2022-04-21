#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

//! Verify if the folder given by the path in the TMPDIR environment variable exists
//! If the path in the TMPDIR environment variable does not point to a valid folder,
//! the value is changed to /tmp
//! \return Always zero; on error, execution is aborted
int chk_tmpdir(void) {
    char tmpdir[512];
    struct stat le_buffer;

    strcpy(tmpdir, "");
    strcat(tmpdir, (char *)getenv("TMPDIR"));
    //   printf("%s\n", tmpdir);
    lstat(tmpdir, &le_buffer);
    int res = S_ISDIR(le_buffer.st_mode);
    //   printf("%d\n", res);
    if (res != 1) {
        fprintf(stderr, "    **************************************************************\n");
        fprintf(stderr, "        Warning : There is a problem with TMPDIR\n");
        fprintf(stderr, "        TMPDIR is %s\n", tmpdir);
        fprintf(stderr, "        Setting TMPDIR=/tmp\n");
        fprintf(stderr, "    **************************************************************\n");
        strcpy(tmpdir, "/tmp/");
        res = putenv("TMPDIR=/tmp");
        if (res != 0) {
            fprintf(stderr, "    **************************************************************\n");
            fprintf(stderr, "       Warning : cannot set TMPDIR to /tmp\n");
            fprintf(stderr, "       Program will now exit\n");
            fprintf(stderr, "    **************************************************************\n");
            exit(-13);
        }
    }
    return 0;
}
