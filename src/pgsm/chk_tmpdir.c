#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <App.h>


//! Verify if the folder given by the path in the TMPDIR environment variable exists
//! If the path in the TMPDIR environment variable does not point to a valid folder,
//! the value is changed to /tmp
void chk_tmpdir(void) {
    char tmpdir[512] = "";
    struct stat fstat;

    strcat(tmpdir, (char *)getenv("TMPDIR"));
    lstat(tmpdir, &fstat);
    int res = S_ISDIR(fstat.st_mode);
    if (res != 1) {
        App_Log(APP_WARNING, "%s: There is a problem with TMPDIR (%s). Setting TMPDIR=/tmp\n", __func__, tmpdir);
        strcpy(tmpdir, "/tmp/");
        res = putenv("TMPDIR=/tmp");
        if (res != 0) {
           App_Log(APP_ERROR, "%s: Cannot set TMPDIR to /tmp, exiting\n", __func__);
           App_End(-13);
           exit(-13);
        }
    }
}
