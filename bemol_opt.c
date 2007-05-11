#include <rpnmacros.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <diese.h>
#include <diese_funct.h>
#include <diese_var_e.h>


typedef struct
  {
  char grtyp_out;
  int iun_in, iun_dst, iun_core, iun_coarse;
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

/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
f77name(init_bemol_options)()
  {
  bemolOpt.iun_in     = -1;
  bemolOpt.iun_dst    = -1;
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
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
f77name(bemol_set_grtyp_out)(char grtyp)
  {
  bemolOpt.grtyp_out = grtyp;
  }

/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
f77name(bemol_set_user_domain)(int *ni_start, int *nj_start, int *ni_end, int *nj_end)
  {
  bemolOpt.ni_start = *ni_start;
  bemolOpt.nj_start = *nj_start;
  bemolOpt.ni_end   = *ni_end;
  bemolOpt.nj_end   = *nj_end;
  bemolOpt.ni       = *ni_end - *ni_start + 1;
  bemolOpt.nj       = *nj_end - *nj_start + 1;
  }
  
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
f77name(bemol_set_iun_in)(int *iun)
  {
  bemolOpt.iun_in = *iun;
  }
  
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
f77name(bemol_set_iun_dst)(int *iun)
  {
  bemolOpt.iun_dst = *iun;
  }
  
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
f77name(bemol_set_iun_core)(int *iun)
  {
  bemolOpt.iun_core = *iun;
  }
  
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
f77name(bemol_set_iun_coarse)(int *iun)
  {
  bemolOpt.iun_coarse = *iun;
  }
  
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
f77name(bemol_set_verbose_level)(int *verbose_level)
  {
  bemolOpt.verbose_level = *verbose_level;
  }
  
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
f77name(bemol_set_nbits)(int *nbits)
  {
  bemolOpt.nbits = *nbits;
  }
  
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
f77name(bemol_set_compression)(int *compression_code)
  {
  bemolOpt.compression_code = *compression_code;
  }
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
  
f77name(bemol_set_fill_mode)(int *fill_mode)
  {
  bemolOpt.fill_mode = *fill_mode;
  }
  
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
f77name(bemol_set_avgfactor)(int *avg_factor)
  {
  bemolOpt.avg_factor = *avg_factor;
  }
  
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
f77name(bemol_set_ig3core)(int *ig3core)
  {
  bemolOpt.ig3core = *ig3core;
  }
  
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
f77name(bemol_set_ig3coarse)(int *ig3coarse)
  {
  bemolOpt.ig3coarse = *ig3coarse;
  }
  
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
  bemol_get_grtyp_out(char *grtyp)
  {
  *grtyp = bemolOpt.grtyp_out;
  }

/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
int bemol_get_user_domain(int *ni_start, int *nj_start, int *ni_end, int *nj_end)
  {
  *ni_start = bemolOpt.ni_start;
  *nj_start = bemolOpt.nj_start;
  *ni_end = bemolOpt.ni_end;
  *nj_end = bemolOpt.nj_end;
  }
  
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
bemol_get_verbose_level(int *verbose_level)
  {
  *verbose_level = bemolOpt.verbose_level;
  }
  
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
int f77name(bemol_get_compression_code)(int *compression_code)
  {
  bemol_get_compression_code(compression_code);
  }
  
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
bemol_get_compression_code(int *compression_code)
  {
  *compression_code = bemolOpt.compression_code;
  }
  
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
bemol_get_fill_mode(int *fill_mode)
  {
  *fill_mode = bemolOpt.fill_mode;
  }
  
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
bemol_get_avgfactor(int *avg_factor)
  {
  *avg_factor = bemolOpt.avg_factor;
  }
  
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
bemol_get_ig3core(int *ig3core)
  {
  *ig3core = bemolOpt.ig3core;
  }
  
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
bemol_get_ig3coarse(int *ig3coarse)
  {
  *ig3coarse = bemolOpt.ig3coarse;
  }

/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
bemol_get_nbits(int *nbits)
  {
  *nbits = bemolOpt.nbits;
  }

/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
bemol_get_iun_in(int *iun)
  {
  *iun = bemolOpt.iun_in;
  }
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
bemol_get_iun_dst(int *iun)
  {
  *iun = bemolOpt.iun_dst;
  }
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
bemol_get_iun_core(int *iun)
  {
  *iun = bemolOpt.iun_core;
  }
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */
bemol_get_iun_coarse(int *iun)
  {
  *iun = bemolOpt.iun_coarse;
  }
