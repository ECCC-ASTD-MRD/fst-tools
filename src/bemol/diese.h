#ifndef DIESE_H
#define DIESE_H

#include <stdint.h>

#define ABORT       13
#define MINIMUM      1
#define MAXIMUM      2
#define MISSING      3

#define FTN2C(i,j,ni)  (int)((ni) * (j-1) + i-1)


typedef struct {
    int32_t presente;
    int32_t key;
    int32_t ni_start, ni_end, ni;
    int32_t nj_start, nj_end, nj;
    int32_t nk;
    int32_t ig1ref, ig2ref;
    float rmin, rmax;
} _Tuile;


typedef struct {
    int32_t key_ax, key_ay,ip1, ip2, ip3, dateo;
    int32_t nix, njx, nkx, niy, njy, nky;
    int32_t npas, deet, nbits;
    int32_t ig1ref, ig2ref, ig3ref, ig4ref;
    int32_t datyp, swa, lng, dltf, ubc, extra1, extra2, extra3;

    char nomvarx[8];
    char nomvary[8];
    char typvarx[4];
    char typvary[4];
    char etiketx[16];
    char etikety[16];

    char grref[4];

    int32_t ntuiles_total;
    int32_t ntuiles_x, ntuiles_y;
    int32_t initialized;
    int32_t *lim_x;
    int32_t *lim_y;
    _Tuile *tuiles;
    float *ax, *ay;
} _Diese;

typedef struct {
    int32_t key,ip1, ip2, ip3, dateo;
    int32_t npas, deet, nbits, datyp;
    int32_t ig1, ig2, ig3, ig4;
    int32_t ni, nj, nk;

    char nomvar[8];
    char typvar[4];
    char etiket[16];
    char grtyp[4];
} _Fld;

typedef struct {
    _Fld fldinfo;
    _Tuile *tuiles;
    int *masque;
    int *tuilesPresentes;
    int *tuilesAbsentes;
    int nb_tuiles, nbTuilesPresentes, nbTuilesAbsentes;
} _Fldlst;

#endif
