#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <rmn.h>
#include <rmn/rpnmacros.h>

#include "diese.h"
#include "diese_funct.h"
#include "diese_var_e.h"



int32_t c_dies_getgridparams(int *ni_start, int *nj_start, int *ni_end, int *nj_end);


int32_t f77name(dies_setninj)(int32_t *ni_start, int32_t *ni_end, int32_t *ni, int32_t *nj_start, int32_t *nj_end, int32_t *nj)
{
    gni_start = *ni_start;
    gnj_start = *nj_start;
    gni_end   = *ni_end;
    gnj_end   = *nj_end;
    gni       = *ni;
    gnj       = *nj;

    // fprintf(stderr, "Dies_setninj : %d-%d-%d --- %d-%d-%d\n", gni_start, gni_end, gni, gnj_start, gnj_end, gnj);
}


int32_t f77name(dies_getgridparams)(int *ni_start, int *nj_start, int *ni_end, int *nj_end)
{
    c_dies_getgridparams(ni_start, nj_start, ni_end, nj_end);
}


int32_t f77name(diesinf)(int32_t *key, int32_t *iun, int32_t *ni, int32_t *nj, int32_t *nk, int32_t *datev, char etiket[],
      int32_t *ip1, int32_t *ip2, int32_t *ip3, int32_t *ig1, int32_t *ig2, char typvar[], char nomvar[],
      F2Cl flenetiket, F2Cl flentypvar, F2Cl flennomvar)
{
    int32_t lenetiket = flenetiket;
    int32_t lentypvar = flentypvar;
    int32_t lennomvar = flennomvar;

    ftnstrclean(nomvar, lennomvar);
    ftnstrclean(typvar, lentypvar);
    ftnstrclean(etiket, lenetiket);

    return c_diesinf(*key, *iun, ni, nj, nk, *datev, etiket, *ip1, *ip2, *ip3, *ig1, *ig2, typvar, nomvar);
}


int32_t f77name(dieslir)(int *iun, int32_t *key, float *buffer, float *ax, float *ay,
      char *grref, int32_t *ig1ref, int32_t *ig2ref, int32_t *ig3ref,
      int32_t *ig4ref, F2Cl lengrref)
{
    return c_dieslir(*iun, *key, buffer, ax, ay, grref, ig1ref, ig2ref, ig3ref, ig4ref);

}


int32_t f77name(diesfillval)(float *value)
{
    return c_diesfillval(*value);
}


int32_t f77name(diesaxay)(int *key, float *ax, float *ay)
{
    return c_diesaxay(*key, ax, ay);
}


int32_t f77name(diesaxayprm)(int *key, int32_t *ni, int32_t *nj,  int32_t *ip1, int32_t *ip2, int32_t *ip3,
          int32_t *dateo, char *typvar, char *etiket, char *grref,
          int32_t *ig1ref, int32_t *ig2ref, int32_t *ig3ref, int32_t *ig4ref,
          F2Cl flentypvar, F2Cl flenetiket, F2Cl flengrref)
{
    int32_t lenetiket = flenetiket;
    int32_t lentypvar = flentypvar;
    int32_t lengrref = flengrref;

    ftnstrclean(typvar, lentypvar);
    ftnstrclean(etiket, lenetiket);
    ftnstrclean(grref, lengrref);

    return c_diesaxayprm(*key, ni, nj, ip1, ip2, ip3, dateo, typvar, etiket, grref, ig1ref, ig2ref, ig3ref, ig4ref);
}


int32_t f77name(diesclrcache)()
{
    return c_diesclrcache();
}


int32_t f77name(diesisincache)(int *key)
{
    return c_diesisincache(*key);
}


int32_t c_diesinf(int32_t key, int32_t iun, int32_t *ni, int32_t *nj, int32_t *nk, int32_t datev, char etiket[],
      int32_t ip1, int32_t ip2, int32_t ip3, int32_t ig1, int32_t ig2, char typvar[], char nomvar[])
{
    int32_t key_ax, key_ay;
    int32_t dateo, npas, deet, nit, njt, nkt, nbits, datyp;
    int32_t nix, njx, nkx, niy, njy, nky;
    char grtyp, grref;
    int32_t ig1t, ig2t, ig3t, ig4t, ig1ref, ig2ref, ig3ref, ig4ref;
    int32_t swa, lng, dltf, ubc, extra1, extra2, extra3;

    char lnomvar[5],ltypvar[3],lgrtyp[2],letiket[13];
    int32_t i, ndiese;

    strcpy(lnomvar, "    ");
    strcpy(letiket, "            ");
    strcpy(lgrtyp,  " ");
    strcpy(ltypvar, "  ");

    strncpy(lnomvar, nomvar, 4);
    strncpy(letiket, etiket, 12);
    strncpy(ltypvar, typvar, 2);

    c_fstprm(key, &dateo, &deet, &npas, &nit, &njt, &nkt, &nbits,
        &datyp, &ip1, &ip2, &ip3, ltypvar, lnomvar, letiket,
        lgrtyp, &ig1t, &ig2t, &ig3t, &ig4t, &swa, &lng, &dltf,
        &ubc, &extra1, &extra2, &extra3);

    if (lgrtyp[0] != '#') {
        return -13;
    }

    key_ax = c_fstinf(iun, &nix, &njx, &nkx, -1, "            ", ig1t, ig2t, -1, "  ", ">>  ");
    key_ay = c_fstinf(iun, &niy, &njy, &nky, -1, "            ", ig1t, ig2t, -1, "  ", "^^  ");
    if (key_ax < 0 || key_ay < 0) {
        return -1313;
    }

    ndiese = currentdiese;
    i = c_diesFindGrid(ig1t, ig2t);
    if (i > ndiese) {
        grd[i].key_ax = key_ax;
        grd[i].key_ay = key_ay;
        strcpy(grd[i].nomvarx, "    ");
        strcpy(grd[i].typvarx, " ");
        strcpy(grd[i].etiketx, "            ");
        strcpy(grd[i].grref,  " ");
        strcpy(grd[i].nomvary, "    ");
        strcpy(grd[i].typvary, " ");
        strcpy(grd[i].etikety, "            ");


        c_fstprm(grd[i].key_ax, &grd[i].dateo, &grd[i].deet, &grd[i].npas, &grd[i].nix, &grd[i].njx, &grd[i].nkx, &grd[i].nbits,
        &grd[i].datyp, &grd[i].ip1, &grd[i].ip2, &grd[i].ip3, grd[i].typvarx, grd[i].nomvarx, grd[i].etiketx,
        grd[i].grref, &grd[i].ig1ref, &grd[i].ig2ref, &grd[i].ig3ref, &grd[i].ig4ref, &grd[i].swa, &grd[i].lng, &grd[i].dltf,
        &grd[i].ubc, &grd[i].extra1, &grd[i].extra2, &grd[i].extra3);

        c_fstprm(grd[i].key_ay, &grd[i].dateo, &grd[i].deet, &grd[i].npas, &grd[i].niy, &grd[i].njy, &grd[i].nky, &grd[i].nbits,
        &grd[i].datyp, &grd[i].ip1, &grd[i].ip2, &grd[i].ip3, grd[i].typvary, grd[i].nomvary, grd[i].etikety,
        grd[i].grref, &grd[i].ig1ref, &grd[i].ig2ref, &grd[i].ig3ref, &grd[i].ig4ref, &grd[i].swa, &grd[i].lng, &grd[i].dltf,
        &grd[i].ubc, &grd[i].extra1, &grd[i].extra2, &grd[i].extra3);

        grd[i].ax = (float *) malloc(nix*sizeof(float));
        grd[i].ay = (float *) malloc(njy*sizeof(float));

        c_fstluk(grd[i].ax, grd[i].key_ax, &nix, &njx, &nkx);
        c_fstluk(grd[i].ay, grd[i].key_ay, &niy, &njy, &nky);
    }

    *ni = grd[i].nix;
    *nj = grd[i].njy;
    *nk = grd[i].nky;

    return key;
}


int32_t c_dies_getgridparams(int *ni_start, int *nj_start, int *ni_end, int *nj_end)
{
    int32_t igrd = 0;
    *ni_start = grd[igrd].lim_x[0];
    *nj_start = grd[igrd].lim_y[0];
    *ni_end   = grd[igrd].lim_x[grd[igrd].ntuiles_x]-1;
    *nj_end   = grd[igrd].lim_y[grd[igrd].ntuiles_y]-1;

    // fprintf(stderr, "getgridparams: (%d, %d)), (%d, %d)\n", *ni_start, *nj_start, *ni_end, *nj_end); 
}

int32_t c_dieslir(int iun, int32_t key, float *buffer, float *ax, float *ay,
      char *grref, int32_t *ig1ref, int32_t *ig2ref, int32_t *ig3ref, int32_t *ig4ref)
{
    float *tuile;
    int32_t ier;
    int32_t dateo, datev, npas, deet, nit, njt, nkt, nbits, datyp;
    char nomvar[5], typvar[2], etiket[13];
    int32_t nig, njg;
    char grtyp[2];
    int32_t ip1, ip2, ip3;
    int32_t ig1t, ig2t, ig3t, ig4t;
    int32_t swa, lng, dltf, ubc, extra1, extra2, extra3;
    int32_t liste[1024], infon;
    double delta_t;
    static int32_t nmax = 1024;
    int32_t startx, starty;

    strcpy(nomvar, "    ");
    strcpy(typvar, " ");
    strcpy(etiket, "            ");
    strcpy(grtyp,  " ");

    c_fstprm(key, &dateo, &deet, &npas, &nit, &njt, &nkt, &nbits,
        &datyp, &ip1, &ip2, &ip3, typvar, nomvar, etiket,
        grtyp, &ig1t, &ig2t, &ig3t, &ig4t, &swa, &lng, &dltf,
        &ubc, &extra1, &extra2, &extra3);
    igrd = c_diesFindGrid(ig1t, ig2t);

    if (grtyp[0] != '#') {
        return -13;
    }

    if (igrd == ndiese) {
        return -13;
    }

    strcpy(grref,  " ");

    delta_t = (double) (deet * npas) / 3600.0;
    f77name(incdatr)(&datev, &dateo, &delta_t);
    c_fstinl(iun, &nit, &njt, &nkt, datev, etiket, ip1, ip2, -1, typvar, nomvar, liste, &infon, nmax);
    ier = c_inventaire_tuiles(igrd, liste, infon, ig1t, ig2t);
    if (ier < 0) return ier;

    nig = grd[igrd].nix;
    njg = grd[igrd].njy;

    memset(buffer, 0, sizeof(float) * nig * njg);

    for (int i = 0; i < nig; i++) {
        ax[i] = grd[igrd].ax[i];
    }

    for (int j = 0; j < njg; j++) {
        ay[j] = grd[igrd].ay[j];
    }

    for (int i = 0; i <  infon; i++) {
        strcpy(nomvar, "    ");
        strcpy(typvar, " ");
        strcpy(etiket, "            ");
        strcpy(grtyp,  " ");

        ier = c_fstprm(liste[i], &dateo, &deet, &npas, &nit, &njt, &nkt, &nbits,
        &datyp, &ip1, &ip2, &ip3, typvar, nomvar, etiket,
        grtyp, &ig1t, &ig2t, &ig3t, &ig4t, &swa, &lng, &dltf,
        &ubc, &extra1, &extra2, &extra3);
        if (ig1t == grd[igrd].ip1 && ig2t == grd[igrd].ip2) {
            startx = ig3t;
            starty = ig4t;
            if (nbits > 32) {
                tuile = (float *) malloc(nit * njt * sizeof(double));
                ier = c_fstluk(tuile, liste[i], &nit, &njt, &nkt);
                f77name(fillgrid8)(buffer,tuile,&nig,&njg,&nit,&njt,&startx,&starty);
                if (startx == 1 && ((startx - 1) + nit) < nig) {
                    f77name(fill_lastcol8)(buffer,tuile,&nig,&njg,&nit,&njt,&startx,&starty);
                }
            } else {
                tuile = (float *) malloc(nit*njt*sizeof(float));
                ier = c_fstluk(tuile, liste[i], &nit, &njt, &nkt);
                f77name(fillgrid)(buffer,tuile,&nig,&njg,&nit,&njt,&startx,&starty);
                if (startx == 1 && ((startx - 1) + nit) < nig) {
                    f77name(fill_lastcol)(buffer,tuile,&nig,&njg,&nit,&njt,&startx,&starty);
                }
            }

            c_diesaddkey(liste[i]);
            free(tuile);
        }
    }

    grref[0] = grd[igrd].grref[0];
    *ig1ref  = grd[igrd].ig1ref;
    *ig2ref  = grd[igrd].ig2ref;
    *ig3ref  = grd[igrd].ig3ref;
    *ig4ref  = grd[igrd].ig4ref;

    return 0;
}

int32_t c_diesfillval(float value)
{
    return 0;
}


int32_t c_diesfillmode(int mode)
{
    return 0;
}


int32_t c_diesaxay(int key, float *ax, float *ay)
{
    return 0;
}


int32_t c_diesaxayprm(int key, int32_t *ni, int32_t *nj,  int32_t *ip1, int32_t *ip2, int32_t *ip3,
          int32_t *dateo, char *typvar, char *etiket, char *grref,
          int32_t *ig1ref, int32_t *ig2ref, int32_t *ig3ref, int32_t *ig4ref)
{
    return 0;
}



int32_t c_diesclrcache()
{
    free(dieskeys);
    dieskeys = (int32_t *) NULL;
    ndieskeys = 0;
    return 0;
}


int32_t c_diesisincache(key)
{
    if (dieskeys == NULL) return -1;

    for (int32_t i = 0; i < ndieskeys; i++)
    {
        if (key == dieskeys[i]) return key;
    }
    return -1;
}


int32_t c_diesaddkey(int key)
{
    if (dieskeys == NULL) {
        dieskeys = (int32_t *) malloc(256*sizeof(int));
    }

    int32_t i;
    for (i=0; i < ndieskeys; i++) {
        if (key == dieskeys[i]) return 0;
    }

    dieskeys[i] = key;
    ndieskeys++;

    if (0 == ndieskeys%256) {
        dieskeys = (int32_t *) realloc(dieskeys, (ndieskeys+256)*sizeof(int));
    }

    return 0;
}


int32_t c_diesFindGrid(int32_t ip1, int32_t ip2)
{
    int32_t i = 0;

    while (i < ndiese && (grd[i].ip1 != ip1 || grd[i].ip2 != ip2)) {
        i++;
    }

    if (i == ndiese) {
        c_diese_AddGrid(i, ip1, ip2);
        currentdiese++;
        ndiese++;
    } else {
        currentdiese = i;
    }

    if (i > ndiesemax) {
        fprintf(stderr, "Trop de grilles diese (#############) !\n");
        exit(1313);
    }

    return currentdiese;
}


int compare_ints(int *token1, int *token2)
{
    if (*token1 < *token2) {
        return -1;
    } else {
        if (*token1 > *token2) {
            return 1;
        } else {
            return 0;
        }
    }
}


int fstfldcmp(_Fld refrec, _Fld therec)
{
    if (0 != strcmp(refrec.nomvar, therec.nomvar))    return 0;
    if (0 != strcmp(refrec.typvar, therec.typvar))    return 0;
    if (0 != strcmp(refrec.etiket, therec.etiket))    return 0;
    if (0 != strcmp(refrec.grtyp, therec.grtyp))      return 0;
    if (refrec.ip1 != therec.ip1)    return 0;
    if (refrec.ip2 != therec.ip2)    return 0;
    if (refrec.ip3 != therec.ip3)    return 0;
    if (refrec.dateo != therec.dateo)    return 0;
    if (refrec.npas != therec.npas)    return 0;
    if (refrec.deet != therec.deet)    return 0;
    if (refrec.ig1 != therec.ig1)    return 0;
    if (refrec.ig2 != therec.ig2)    return 0;
    return 1;
}


int c_diese_AddGrid(int igrid, int ip1, int ip2)
{
    int32_t key_ax, key_ay;
    int32_t nix, njx, nkx, niy, njy, nky;
    int32_t i, iun;

    i = igrid;
    bemol_get_iun_in(&iun);
    /*fprintf(stderr, "iun = %d\n\n", iun);  */
    key_ax = c_fstinf(iun, &nix, &njx, &nkx, -1, "            ", ip1 , ip2 , -1, "  ", ">>  ");
    key_ay = c_fstinf(iun, &niy, &njy, &nky, -1, "            ", ip1 , ip2 , -1, "  ", "^^  ");

    grd[i].key_ax = key_ax;
    grd[i].key_ay = key_ay;
    strcpy(grd[i].nomvarx, "    ");
    strcpy(grd[i].typvarx, " ");
    strcpy(grd[i].etiketx, "            ");
    strcpy(grd[i].grref,  " ");
    strcpy(grd[i].nomvary, "    ");
    strcpy(grd[i].typvary, " ");
    strcpy(grd[i].etikety, "            ");


    c_fstprm(grd[i].key_ax, &grd[i].dateo, &grd[i].deet, &grd[i].npas, &grd[i].nix, &grd[i].njx, &grd[i].nkx, &grd[i].nbits,
        &grd[i].datyp, &grd[i].ip1, &grd[i].ip2, &grd[i].ip3, grd[i].typvarx, grd[i].nomvarx, grd[i].etiketx,
        grd[i].grref, &grd[i].ig1ref, &grd[i].ig2ref, &grd[i].ig3ref, &grd[i].ig4ref, &grd[i].swa, &grd[i].lng, &grd[i].dltf,
        &grd[i].ubc, &grd[i].extra1, &grd[i].extra2, &grd[i].extra3);

    c_fstprm(grd[i].key_ay, &grd[i].dateo, &grd[i].deet, &grd[i].npas, &grd[i].niy, &grd[i].njy, &grd[i].nky, &grd[i].nbits,
        &grd[i].datyp, &grd[i].ip1, &grd[i].ip2, &grd[i].ip3, grd[i].typvary, grd[i].nomvary, grd[i].etikety,
        grd[i].grref, &grd[i].ig1ref, &grd[i].ig2ref, &grd[i].ig3ref, &grd[i].ig4ref, &grd[i].swa, &grd[i].lng, &grd[i].dltf,
        &grd[i].ubc, &grd[i].extra1, &grd[i].extra2, &grd[i].extra3);

    grd[i].ax = (float *) malloc(nix*sizeof(float));
    grd[i].ay = (float *) malloc(njy*sizeof(float));

    c_fstluk(grd[i].ax, grd[i].key_ax, &nix, &njx, &nkx);
    c_fstluk(grd[i].ay, grd[i].key_ay, &niy, &njy, &nky);
    return 0;
}
