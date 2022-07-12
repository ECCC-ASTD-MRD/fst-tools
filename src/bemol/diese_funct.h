#ifndef DIESE_FUNCT_H
#define DIESE_FUNCT_H

#include <stdint.h>

/* ------1---------2---------3---------4---------5---------6---------7---------8---------9------- */
int32_t ftnstrclean(char *string, int32_t length);
int32_t f77name(diesinf)(int32_t *key, int32_t *iun, int32_t *ni, int32_t *nj, int32_t *nk, int32_t *datev, char etiket[], 
      int32_t *ip1, int32_t *ip2, int32_t *ip3, int32_t *ig1, int32_t *ig2, char typvar[], char nomvar[], 
      F2Cl flenetiket, F2Cl flentypvar, F2Cl flennomvar);
int32_t f77name(dieslir)(int *iun, int32_t *key, float *buffer, float *ax, float *ay, 
        char *grref, int32_t *ig1ref, int32_t *ig2ref, int32_t *ig3ref, int32_t *ig4ref, F2Cl lengrref);
int32_t f77name(diesfillval)(float *value);
int32_t f77name(diesfillmode)(int *mode);
int32_t f77name(diesaxay)(int *key, float *ax, float *ay);
int32_t f77name(diesaxayprm)(int *key, int32_t *ni, int32_t *nj,  int32_t *ip1, int32_t *ip2, int32_t *ip3, 
          int32_t *dateo, char *typvar, char *etiket, char *grref,
          int32_t *ig1ref, int32_t *ig2ref, int32_t *ig3ref, int32_t *ig4ref, 
          F2Cl flentypvar, F2Cl flenetiket, F2Cl flengrref);
int32_t f77name(diesclrcache)();
int32_t f77name(diesisincache)(int *key);
int32_t f77name(dies_getgridparams)(int *ni_start, int *nj_start, int *ni_end, int *nj_end);
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9------- */
int32_t c_diesFindGrid(int32_t ip1, int32_t ip2);
int32_t c_diesinf(int32_t key, int32_t iun, int32_t *ni, int32_t *nj, int32_t *nk, int32_t datev, char etiket[], 
      int32_t ip1, int32_t ip2, int32_t ip3, int32_t ig1, int32_t ig2, char typvar[], char nomvar[]);
int32_t c_dieslir(int iun, int32_t key, float *buffer, float *ax, float *ay, 
      char *grref, int32_t *ig1ref, int32_t *ig2ref, int32_t *ig3ref, int32_t *ig4ref);
int32_t c_diesfillval(float value);
int32_t c_diesfillmode(int mode);
int32_t c_diesaxay(int key, float *ax, float *ay);
int32_t c_diesaxayprm(int key, int32_t *ni, int32_t *nj,  int32_t *ip1, int32_t *ip2, int32_t *ip3, 
          int32_t *dateo, char *typvar, char *etiket, char *grref,
          int32_t *ig1ref, int32_t *ig2ref, int32_t *ig3ref, int32_t *ig4ref);
int32_t c_diesclrcache();
int32_t c_diesisincache(int key);
int32_t c_diesaddkey(int key);

#endif
