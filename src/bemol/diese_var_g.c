#include <stdint.h>
#include <stdlib.h>

#include "diese.h"

int32_t *dieskeys = NULL;
int32_t ndieskeys = 0;
int32_t igrd = -1;

_Diese grd[256];
_Fldlst *flist = NULL;
int lng_flist = 0;

int32_t ndiesemax = 256;
int32_t currentdiese = -1;
int32_t ndiese = 0;

int32_t gni_start, gni_end, gni, gnj_start, gnj_end, gnj;
