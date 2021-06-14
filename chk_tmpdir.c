#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <rmnlib.h>

/* Cette fonction verifie si le repertoire $TMPDIR existe.
 * Si oui, la fonction retourne sans problemes
 * Si non, on fait un "putenv(TMPDIR=/tmp/)
 * */

int f77name(chk_tmpdir)(void)
  {
  return chk_tmpdir();
  }

int chk_tmpdir(void)
  {
  char tmpdir[512];
  struct stat le_buffer;
  int res;
  
  strcpy(tmpdir, "");
  strcat(tmpdir, (char *)getenv("TMPDIR"));
//   printf("%s\n", tmpdir);
  lstat(tmpdir, &le_buffer);
  res = S_ISDIR(le_buffer.st_mode);
//   printf("%d\n", res);
  if (res != 1)
   {
   fprintf(stderr, "    **************************************************************\n");
   fprintf(stderr, "        Warning : There is a problem with TMPDIR\n");
   fprintf(stderr, "        TMPDIR is %s\n", tmpdir);
   fprintf(stderr, "        Setting TMPDIR=/tmp\n");
   fprintf(stderr, "    **************************************************************\n");
   strcpy(tmpdir, "/tmp/");
   res = putenv("TMPDIR=/tmp");
   if (res != 0)
    {
   fprintf(stderr, "    **************************************************************\n");
    fprintf(stderr, "       Warning : cannot set TMPDIR to /tmp\n");
    fprintf(stderr, "       Program will now exit\n");
   fprintf(stderr, "    **************************************************************\n");
    exit(-13);
    }
   res = strcat(tmpdir, (char *)getenv("TMPDIR"));
//    printf("%s\n", tmpdir);
   }
  return 0;
  }
  
  