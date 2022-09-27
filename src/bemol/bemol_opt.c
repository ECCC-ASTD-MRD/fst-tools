#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <rmn/rpnmacros.h>

#include "diese.h"
#include "diese_funct.h"
#include "diese_var_e.h"


typedef struct
{
    char grtyp_out;
    int iun_in, iun_dst, iun_cfs, iun_core, iun_coarse;
    int ni_start, ni_end, ni;
    int nj_start, nj_end, nj;
    int verbose_level;
    int fill_mode;
    int compression_code;
    int nbits;
    int avg_factor;
    int ig3core, ig3coarse;
} _Bemol_options;

static _Bemol_options bemolOpt;

void f77name(init_bemol_options)()
{
    bemolOpt.iun_in     = -1;
    bemolOpt.iun_dst    = -1;
    bemolOpt.iun_cfs    = -1;
    bemolOpt.iun_core   = -1;
    bemolOpt.iun_coarse = -1;
    bemolOpt.grtyp_out  = ' ';
    bemolOpt.ni_start   = -1;
    bemolOpt.ni_end     = -1;
    bemolOpt.ni         = -1;
    bemolOpt.nj_start   = -1;
    bemolOpt.nj_end     = -1;
    bemolOpt.nj         = -1;
    bemolOpt.verbose_level = 1;
    bemolOpt.fill_mode  = ABORT;
    bemolOpt.compression_code = -1;
    bemolOpt.nbits      = -1;
    bemolOpt.avg_factor = -1;
    bemolOpt.ig3core    = -1;
    bemolOpt.ig3coarse  = -1;
}


void f77name(bemol_set_grtyp_out)(char grtyp, F2Cl lng)
{
    bemolOpt.grtyp_out = grtyp;
}

void f77name(bemol_set_user_domain)(int *ni_start, int *nj_start, int *ni_end, int *nj_end)
{
    bemolOpt.ni_start = *ni_start;
    bemolOpt.nj_start = *nj_start;
    bemolOpt.ni_end   = *ni_end;
    bemolOpt.nj_end   = *nj_end;
    bemolOpt.ni       = *ni_end - *ni_start + 1;
    bemolOpt.nj       = *nj_end - *nj_start + 1;
}

void f77name(bemol_set_iun_in)(int *iun)
{
    bemolOpt.iun_in = *iun;
}

void f77name(bemol_set_iun_dst)(int *iun)
{
    bemolOpt.iun_dst = *iun;
}

void f77name(bemol_set_iun_cfs)(int *iun)
{
    bemolOpt.iun_cfs = *iun;
}

void f77name(bemol_set_iun_core)(int *iun)
{
    bemolOpt.iun_core = *iun;
}

void f77name(bemol_set_iun_coarse)(int *iun)
{
    bemolOpt.iun_coarse = *iun;
}

void f77name(bemol_set_verbose_level)(int *verbose_level)
{
    bemolOpt.verbose_level = *verbose_level;
}

void f77name(bemol_set_nbits)(int *nbits)
{
    bemolOpt.nbits = *nbits;
}

void f77name(bemol_set_compression)(int *compression_code)
{
    bemolOpt.compression_code = *compression_code;
}

void f77name(bemol_set_fill_mode)(int *fill_mode)
{
    bemolOpt.fill_mode = *fill_mode;
}

void f77name(bemol_set_avgfactor)(int *avg_factor)
{
    bemolOpt.avg_factor = *avg_factor;
}

void f77name(bemol_set_ig3core)(int *ig3core)
{
    bemolOpt.ig3core = *ig3core;
}

void f77name(bemol_set_ig3coarse)(int *ig3coarse)
{
    bemolOpt.ig3coarse = *ig3coarse;
}

void bemol_get_grtyp_out(char *grtyp)
{
    *grtyp = bemolOpt.grtyp_out;
}

int bemol_get_user_domain(int *ni_start, int *nj_start, int *ni_end, int *nj_end)
{
    *ni_start = bemolOpt.ni_start;
    *nj_start = bemolOpt.nj_start;
    *ni_end = bemolOpt.ni_end;
    *nj_end = bemolOpt.nj_end;
}

void bemol_get_verbose_level(int *verbose_level)
{
    *verbose_level = bemolOpt.verbose_level;
}

void bemol_get_compression_code(int *compression_code)
{
    *compression_code = bemolOpt.compression_code;
}

int32_t f77name(bemol_get_compression_code)(int *compression_code)
{
    bemol_get_compression_code(compression_code);
}

void bemol_get_fill_mode(int *fill_mode)
{
    *fill_mode = bemolOpt.fill_mode;
}

void bemol_get_avgfactor(int *avg_factor)
{
    *avg_factor = bemolOpt.avg_factor;
}

void bemol_get_ig3core(int *ig3core)
{
    *ig3core = bemolOpt.ig3core;
}

void bemol_get_ig3coarse(int *ig3coarse)
{
    *ig3coarse = bemolOpt.ig3coarse;
}

void bemol_get_nbits(int *nbits)
{
    *nbits = bemolOpt.nbits;
}

void bemol_get_iun_in(int *iun)
{
    *iun = bemolOpt.iun_in;
}

void bemol_get_iun_dst(int *iun)
{
    *iun = bemolOpt.iun_dst;
}

void bemol_get_iun_cfs(int *iun)
{
    *iun = bemolOpt.iun_cfs;
}

void bemol_get_iun_core(int *iun)
{
    *iun = bemolOpt.iun_core;
}

void bemol_get_iun_coarse(int *iun)
{
    *iun = bemolOpt.iun_coarse;
}
