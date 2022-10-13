#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <rmn.h>

#include "diese.h"
#include "diese_funct.h"
#include "diese_var_e.h"

void c_diesFillMissingTiles(int igrd, float *fld, _Fldlst flist, int fill_mode, int *ni_start, int *nj_start, int *ni_end, int *nj_end, int *nig, int *njg);

int f77name(dies_process_flds)(int *keys, int *nkeys)
{
    int res, ind_tuile;
    _Fld fstrec;
    int swa, lng, dltf, ubc, extra1, extra2, extra3;
    int nig,njg,nit,njt,nkt,startx, starty, nistart, njstart, niend, njend;
    void *buffer, *tuile;
    char nomvar[8], typvar[4], etiket[16], user_grtyp[2];
    int fill_mode, n, duplicate;
    int ig3, ig4, ig3core, ig3coarse, avg, user_nbits, lcl_nbits;
    int usrc, udst, ucfs, ucoarse, ucore;
    int iun_outs[4];
    static int missing_tiles = 0;
    int un = 1;

    bemol_get_fill_mode(&fill_mode);
    bemol_get_iun_dst(&udst);
    bemol_get_iun_cfs(&ucfs);
    bemol_get_iun_core(&ucore);
    bemol_get_iun_coarse(&ucoarse);
    bemol_get_avgfactor(&avg);
    bemol_get_nbits(&user_nbits);
    bemol_get_ig3core(&ig3core);
    bemol_get_ig3coarse(&ig3coarse);

    iun_outs[0] = udst;
    iun_outs[1] = ucore;
    iun_outs[2] = ucoarse;
    iun_outs[3] = ucfs;

    flist = (_Fldlst *)malloc(*nkeys * sizeof(_Fldlst));
    memset (grd, 0 , 256 * sizeof(_Diese));
    for (int i = 0; i < *nkeys; i++) {
        flist[i].tuiles = (_Tuile *)malloc(sizeof(_Tuile) * 128);
        flist[i].nb_tuiles = 0;
    }

    int ier;
    lng_flist = 0;
    for (int i = 0; i < *nkeys; i++) {
        memset(&fstrec, (int)0, sizeof(_Fld));
        strcpy(fstrec.nomvar, "    ");
        strcpy(fstrec.typvar, "  ");
        strcpy(fstrec.etiket, "            ");
        strcpy(fstrec.grtyp, " ");
        ier = c_fstprm(keys[i], &fstrec.dateo, &fstrec.deet, &fstrec.npas, &fstrec.ni, &fstrec.nj, &fstrec.nk, &fstrec.nbits,
        &fstrec.datyp, &fstrec.ip1, &fstrec.ip2, &fstrec.ip3, fstrec.typvar, fstrec.nomvar, fstrec.etiket,
        fstrec.grtyp, &fstrec.ig1, &fstrec.ig2, &fstrec.ig3, &fstrec.ig4, &swa, &lng, &dltf,
        &ubc, &extra1, &extra2, &extra3);

        int found = 0;
        int j = 0;
        while (j < lng_flist && found == 0) {
            res = fstfldcmp(flist[j].fldinfo, fstrec);
            if (res == 0) {
                j++;
            } else {
                found = 1;
                if (fstrec.grtyp[0] == '#') {
                    duplicate = 0;
                    n = 0;
                    while (n < flist[j].nb_tuiles && !duplicate) {
                        if (fstrec.ig3 == flist[j].tuiles[n].ni_start &&
                            fstrec.ig4 == flist[j].tuiles[n].nj_start &&
                            fstrec.ni == flist[j].tuiles[n].ni &&
                            fstrec.nj == flist[j].tuiles[n].nj) {
                            duplicate = 1;
                        }
                        n++;
                    }
                    if (!duplicate) {
                        fstrec.key = keys[i];
                        ind_tuile = flist[j].nb_tuiles;
                        flist[j].tuiles[ind_tuile].key      = keys[i];
                        flist[j].tuiles[ind_tuile].ni       = fstrec.ni;
                        flist[j].tuiles[ind_tuile].nj       = fstrec.nj;
                        flist[j].tuiles[ind_tuile].nk       = fstrec.nk;
                        flist[j].tuiles[ind_tuile].ig1ref   = fstrec.ig1;
                        flist[j].tuiles[ind_tuile].ig2ref   = fstrec.ig2;
                        flist[j].tuiles[ind_tuile].ni_start = fstrec.ig3;
                        flist[j].tuiles[ind_tuile].nj_start = fstrec.ig4;
                        flist[j].tuiles[ind_tuile].ni_end   = flist[j].tuiles[ind_tuile].ni_start +  flist[j].tuiles[ind_tuile].ni - 1 ;
                        flist[j].tuiles[ind_tuile].nj_end   = flist[j].tuiles[ind_tuile].nj_start +  flist[j].tuiles[ind_tuile].nj - 1;
                        flist[j].nb_tuiles++;
                        if (0 == flist[j].nb_tuiles % 128) {
                            flist[j].tuiles = realloc(flist[j].tuiles, sizeof(_Tuile) * ((flist[j].nb_tuiles) + 128));
                        }
                    }
                }
            }
        }

        if (found == 0) {
            flist[j].tuiles = malloc(sizeof(_Tuile) * 128);
            flist[j].nb_tuiles = 1;
            memcpy(&(flist[lng_flist].fldinfo), &fstrec, sizeof(_Fld));
            flist[lng_flist].tuiles[0].key      = keys[i];
            flist[lng_flist].tuiles[0].ni = fstrec.ni;
            flist[lng_flist].tuiles[0].nj = fstrec.nj;
            flist[lng_flist].tuiles[0].nk = fstrec.nk;
            flist[j].tuiles[0].ig1ref     = fstrec.ig1;
            flist[j].tuiles[0].ig2ref     = fstrec.ig2;
            flist[j].tuiles[0].ni_start   = fstrec.ig3;
            flist[j].tuiles[0].nj_start   = fstrec.ig4;
            flist[j].tuiles[0].ni_end     = flist[j].tuiles[0].ni_start + flist[j].tuiles[0].ni - 1;
            flist[j].tuiles[0].nj_end     = flist[j].tuiles[0].nj_start + flist[j].tuiles[0].nj - 1;
            lng_flist++;
        }
    }

    // Inventaire de chacune des tuiles pour determiner si des tuiles sont manquantes.
    for (int i = 0; i < lng_flist; i++) {
        if (flist[i].fldinfo.grtyp[0] == '#') {
            igrd = c_diesFindGrid(flist[i].fldinfo.ig1, flist[i].fldinfo.ig2);
            missing_tiles = c_inventaire_tuiles(igrd, flist[i].tuiles, flist[i].nb_tuiles);
            if (fill_mode == ABORT && missing_tiles < 0) {
                return ier;
            }
        }
    }

    // Charger tous les enregistrements positionnels disponibles
    for (int i = 0; i < lng_flist; i++) {
        if (0 == strncmp(flist[i].fldinfo.nomvar, ">>", 2)) {
            igrd = c_diesFindGrid(flist[i].fldinfo.ip1, flist[i].fldinfo.ip2);
        }
    }

    // Ecrire les enregistrements sur disque
    for (int i = 0; i < ndiese; i++) {
        if (udst != -1) {
            igrd = i;
            f77name(bm_wrt_axay)(&udst, grd[igrd].ax, grd[igrd].ay, &(grd[igrd].nix), &(grd[igrd].njy), grd[igrd].typvarx, grd[igrd].etiketx,
            &(grd[igrd].ip1), &(grd[igrd].ip2), &(grd[igrd].ip3), &(grd[igrd].dateo), &(grd[igrd].deet), &(flist[i].fldinfo.npas), &(flist[i].fldinfo.nbits),
            grd[igrd].grref, &(grd[igrd].ig1ref), &(grd[igrd].ig2ref), &(grd[igrd].ig3ref), &(grd[igrd].ig4ref),
            (F2Cl) 4,(F2Cl) 16,(F2Cl) 2);
        }
        if (ucfs != -1) {
            igrd = i;
            f77name(bm_wrt_axay)(&ucfs, grd[igrd].ax, grd[igrd].ay, &(grd[igrd].nix), &(grd[igrd].njy), grd[igrd].typvarx, grd[igrd].etiketx,
            &(grd[igrd].ip1), &(grd[igrd].ip2), &(grd[igrd].ip3), &(grd[igrd].dateo), &(grd[igrd].deet), &(flist[i].fldinfo.npas), &(flist[i].fldinfo.nbits),
            grd[igrd].grref, &(grd[igrd].ig1ref), &(grd[igrd].ig2ref), &(grd[igrd].ig3ref), &(grd[igrd].ig4ref),
            (F2Cl) 4,(F2Cl) 16,(F2Cl) 2);
        }
    }

    // Traiter tous les enregistrements...

    // Assembler les morceaux ...
    for (int i = 0; i < lng_flist; i++) {
        if (0 != strncmp(flist[i].fldinfo.nomvar, ">>", 2) && 0 != strncmp(flist[i].fldinfo.nomvar, "^^", 2)) {
            // Determination de la taille des champs recomposes ...

            switch(flist[i].fldinfo.grtyp[0]) {
                case '#':
                    igrd = c_diesFindGrid(flist[i].fldinfo.ig1, flist[i].fldinfo.ig2);
                    nig =  grd[igrd].nix;
                    njg =  grd[igrd].njy;
                    nistart = grd[igrd].lim_x[0];
                    njstart = grd[igrd].lim_y[0];
                    niend   = grd[igrd].lim_x[grd[igrd].ntuiles_x]-1;
                    njend   = grd[igrd].lim_y[grd[igrd].ntuiles_y]-1;
                    if ((nig - niend) == 1) niend = niend + 1;
                    if (nig == (niend-nistart + 1) && njg == (njend-njstart + 1)) {
                        strcpy(user_grtyp, "Z");
                    } else {
                        strcpy(user_grtyp, "#");
                    }
                    break;

                case 'Y':
                case 'Z':
                    igrd = c_diesFindGrid(flist[i].fldinfo.ig1, flist[i].fldinfo.ig2);
                    nistart = 1;
                    njstart = 1;
                    niend   = grd[igrd].nix;
                    njend   = grd[igrd].njy;
                    strcpy(user_grtyp, "Z");
                    break;

                default:
                    nistart = 1;
                    njstart = 1;
                    niend   = flist[i].fldinfo.ni;
                    njend   = flist[i].fldinfo.nj;
                    break;
            }

            switch(flist[i].fldinfo.grtyp[0]) {
                case '#':
                    igrd = c_diesFindGrid(flist[i].fldinfo.ig1, flist[i].fldinfo.ig2);
                    if (flist[i].fldinfo.nbits > 32) {
                        buffer = (double *) malloc((niend-nistart + 1)*(njend-njstart + 1)*sizeof(double));
                    } else {
                        buffer = (float *) malloc((niend-nistart + 1)*(njend-njstart + 1)*sizeof(float));
                    }
                    for (int n = 0; n < flist[i].nb_tuiles; n++) {
                        nit = flist[i].tuiles[n].ni;
                        njt = flist[i].tuiles[n].nj;
                        nkt = 1;
                        if (flist[i].fldinfo.nbits > 32) {
                            tuile = (double *)  malloc(nit*njt*sizeof(double));
                        } else {
                            tuile = (float *) malloc(nit*njt*sizeof(float));
                        }

                        ier = c_fstluk(tuile, &(flist[i].tuiles[n].key), &nit, &njt, &nkt);

                        startx = flist[i].tuiles[n].ni_start;
                        starty = flist[i].tuiles[n].nj_start;
                        if (flist[i].fldinfo.nbits > 32) {
                            f77name(fillgrid8)(buffer,tuile,&nistart,&njstart,&niend,&njend,&nit,&njt,&startx,&starty);
                        } else {
                            f77name(fillgrid)(buffer,tuile,&nistart,&njstart,&niend,&njend,&nit,&njt,&startx,&starty);
                        }

                        if (startx == 1 && (359.99 <= (grd[igrd].ax[nig-1]-grd[igrd].ax[0]))) {
                            if (flist[i].fldinfo.nbits > 32) {
                                f77name(fill_lastcol8)(buffer,tuile,&nistart,&njstart,&niend,&njend,&nit,&njt,&startx,&starty);
                            } else {
                                f77name(fill_lastcol)(buffer,tuile,&nistart,&njstart,&niend,&njend,&nit,&njt,&startx,&starty);
                            }
                        }
                        free(tuile);
                    }
                    if (fill_mode != ABORT && missing_tiles < 0) {
                        c_diesFillMissingTiles(igrd, buffer, flist[i], fill_mode, &nistart, &njstart, &niend, &njend, &nig, &njg);
                    }
                    break;

                default:
                    if (flist[i].fldinfo.nbits > 32) {
                        buffer = calloc(flist[i].fldinfo.ni*flist[i].fldinfo.nj,sizeof(double));
                    } else {
                        buffer = calloc(flist[i].fldinfo.ni*flist[i].fldinfo.nj,sizeof(float));
                    }
                    ier = c_fstluk((void *)buffer, flist[i].tuiles[0].key, &nit, &njt, &nkt);
                    break;
            }

            if (user_nbits == -1) {
                lcl_nbits = flist[i].fldinfo.nbits;
            } else {
                lcl_nbits = user_nbits;
            }

            // Ecriture des champs...
            switch(flist[i].fldinfo.grtyp[0]) {
                case '#':
                case 'Z':
                    if (udst != -1) {
                        if (user_grtyp[0] == '#') {
                            ig3 = nistart;
                            ig4 = njstart;
                        } else {
                            ig3 = 0;
                            ig4 = 0;
                        }

                        f77name(bm_std_wrt)(&udst, buffer, &nistart, &njstart, &niend, &njend,
                            flist[i].fldinfo.nomvar, flist[i].fldinfo.typvar, flist[i].fldinfo.etiket,
                            &(flist[i].fldinfo.ip1), &(flist[i].fldinfo.ip2), &(flist[i].fldinfo.ip3), &(flist[i].fldinfo.dateo), &(flist[i].fldinfo.deet),
                            &(flist[i].fldinfo.npas), &(flist[i].fldinfo.datyp), &lcl_nbits,
                            user_grtyp, &(flist[i].fldinfo.ig1), &(flist[i].fldinfo.ig2), &ig3, &ig4,
                            (F2Cl) 8,(F2Cl) 4,(F2Cl) 16,(F2Cl) 2);
                    }

                    if (ucfs != -1) {
                        if (user_grtyp[0] == '#') {
                            ig3 = nistart;
                            ig4 = njstart;
                        } else {
                            ig3 = 0;
                            ig4 = 0;
                        }

                        f77name(bm_std_wrt)(&ucfs, buffer, &nistart, &njstart, &niend, &njend,
                            flist[i].fldinfo.nomvar, flist[i].fldinfo.typvar, flist[i].fldinfo.etiket,
                            &(flist[i].fldinfo.ip1), &(flist[i].fldinfo.ip2), &(flist[i].fldinfo.ip3), &(flist[i].fldinfo.dateo), &(flist[i].fldinfo.deet),
                            &(flist[i].fldinfo.npas), &(flist[i].fldinfo.datyp), &lcl_nbits,
                            user_grtyp, &(flist[i].fldinfo.ig1), &(flist[i].fldinfo.ig2), &ig3, &ig4,
                            (F2Cl) 8,(F2Cl) 4,(F2Cl) 16,(F2Cl) 2);
                    }

                    if (ucore != -1) {
                        igrd = c_diesFindGrid(flist[i].fldinfo.ig1, flist[i].fldinfo.ig2);
                        nig =  grd[igrd].nix;
                        njg =  grd[igrd].njy;
                        ig4 = 0;
                        strcpy(user_grtyp, "Z");
                        f77name(bm_core_wrt)(&ucore, buffer, grd[igrd].ax, grd[igrd].ay, &(grd[igrd].nix), &(grd[igrd].njy),
                                            flist[i].fldinfo.nomvar, flist[i].fldinfo.typvar, flist[i].fldinfo.etiket,
                                            &(flist[i].fldinfo.ip1), &(flist[i].fldinfo.ip2), &(flist[i].fldinfo.ip3),
                                            &(flist[i].fldinfo.dateo), &(flist[i].fldinfo.deet),
                                            &(flist[i].fldinfo.npas), &(flist[i].fldinfo.datyp), &lcl_nbits,
                                            user_grtyp, &(flist[i].fldinfo.ig1), &(flist[i].fldinfo.ig2), &ig3core, &ig4,
                                            grd[igrd].grref, &(grd[igrd].ig1ref), &(grd[igrd].ig2ref), &(grd[igrd].ig3ref),
                                            &(grd[igrd].ig4ref), (F2Cl) 8,(F2Cl) 4,(F2Cl) 16,(F2Cl) 2,(F2Cl) 2);
                    }

                    if (ucoarse != -1) {
                        igrd = c_diesFindGrid(flist[i].fldinfo.ig1, flist[i].fldinfo.ig2);
                        nig =  grd[igrd].nix;
                        njg =  grd[igrd].njy;
                        bemol_get_ig3coarse(&ig3coarse);
                        ig4 = 0;
                        strcpy(user_grtyp, "Z");
                        f77name(bm_coarse_wrt)(&ucoarse, buffer, grd[igrd].ax, grd[igrd].ay, &(grd[igrd].nix), &(grd[igrd].njy),
                                            flist[i].fldinfo.nomvar, flist[i].fldinfo.typvar, flist[i].fldinfo.etiket,
                                            &(flist[i].fldinfo.ip1), &(flist[i].fldinfo.ip2), &(flist[i].fldinfo.ip3), &(flist[i].fldinfo.dateo), &(flist[i].fldinfo.deet),
                                            &(flist[i].fldinfo.npas), &(flist[i].fldinfo.datyp), &lcl_nbits,
                                            user_grtyp, &(flist[i].fldinfo.ig1), &(flist[i].fldinfo.ig2), &ig3coarse, &ig4,
                                            grd[igrd].grref, &(grd[igrd].ig1ref), &(grd[igrd].ig2ref), &(grd[igrd].ig3ref), &(grd[igrd].ig4ref), &avg, (F2Cl) 8,(F2Cl) 4,(F2Cl) 16,(F2Cl) 2,(F2Cl) 2);
                    }
                    free(buffer);
                    break;

                default:
                    /* loop value check matches size for iun_outs */
                    for (int n = 0; n < 4; n++) {
                        if (iun_outs[n] != -1) {
                            if (flist[i].fldinfo.nbits > 32) {
                                f77name(bm_vanilla_wrt8)(&iun_outs[n], buffer, &(flist[i].fldinfo.ni), &(flist[i].fldinfo.nj),
                                    flist[i].fldinfo.nomvar, flist[i].fldinfo.typvar, flist[i].fldinfo.etiket,
                                    &(flist[i].fldinfo.ip1), &(flist[i].fldinfo.ip2), &(flist[i].fldinfo.ip3), &(flist[i].fldinfo.dateo), &(flist[i].fldinfo.deet),
                                    &(flist[i].fldinfo.npas), &(flist[i].fldinfo.datyp), &lcl_nbits,
                                    flist[i].fldinfo.grtyp, &(flist[i].fldinfo.ig1), &(flist[i].fldinfo.ig2), &(flist[i].fldinfo.ig3), &(flist[i].fldinfo.ig4),
                                    (F2Cl) 8,(F2Cl) 4,(F2Cl) 16,(F2Cl) 2);
                            } else {
                                f77name(bm_vanilla_wrt)(&iun_outs[n], buffer, &(flist[i].fldinfo.ni), &(flist[i].fldinfo.nj),
                                    flist[i].fldinfo.nomvar, flist[i].fldinfo.typvar, flist[i].fldinfo.etiket,
                                    &(flist[i].fldinfo.ip1), &(flist[i].fldinfo.ip2), &(flist[i].fldinfo.ip3), &(flist[i].fldinfo.dateo), &(flist[i].fldinfo.deet),
                                    &(flist[i].fldinfo.npas), &(flist[i].fldinfo.datyp), &lcl_nbits,
                                    flist[i].fldinfo.grtyp, &(flist[i].fldinfo.ig1), &(flist[i].fldinfo.ig2), &(flist[i].fldinfo.ig3), &(flist[i].fldinfo.ig4),
                                    (F2Cl) 8,(F2Cl) 4,(F2Cl) 16,(F2Cl) 2);
                            }
                        }
                    }
                    free(buffer);
                    break;
            }
        }
    }
    return 0;
}


void c_diesFillMissingTiles(int igrd, float *fld, _Fldlst flist, int fill_mode, int *ni_start, int *nj_start, int *ni_end, int *nj_end, int *gni, int *gnj)
{
    int *theTiles;
    int tx, ty, nt;
    int tileIndex;
    float lmin, lmax, gmin, gmax,fill_val;

    theTiles = malloc(grd[igrd].ntuiles_x * grd[igrd].ntuiles_y * sizeof(int));
    tx = grd[igrd].ntuiles_x;
    ty = grd[igrd].ntuiles_y;
    nt = tx * ty;

    flist.masque = malloc(sizeof(int)*nt);
    memset(flist.masque, 0, nt*sizeof(int));
    flist.tuilesPresentes = malloc(sizeof(int)*nt);
    flist.tuilesAbsentes = malloc(sizeof(int)*nt);

    for (int i = 0; i < nt; i++) {
        tileIndex = diesGetTileIndex(igrd, flist.tuiles[i].ni_start, flist.tuiles[i].nj_start, flist.tuiles[i].ni, flist.tuiles[i].nj);
        if (tileIndex >= 0) {
            flist.masque[tileIndex] = 1;
        }
    }

    flist.nbTuilesPresentes = 0;
    flist.nbTuilesAbsentes = 0;
    for (int i = 0; i < nt; i++) {
        switch (flist.masque[i]) {
            case 0:
                flist.tuilesAbsentes[flist.nbTuilesAbsentes] = i;
                flist.nbTuilesAbsentes++;
                break;

            case 1:
                flist.tuilesPresentes[flist.nbTuilesPresentes] = i;
                flist.nbTuilesPresentes++;
                break;
        }
    }

    tileIndex = flist.tuilesPresentes[0];
    int k = FTN2C(grd[igrd].tuiles[tileIndex].ni_start,grd[igrd].tuiles[tileIndex].nj_start,*gni);
    gmin = fld[k];
    gmax = fld[k];
    for (int i = 0; i < flist.nbTuilesPresentes; i++) {
        tileIndex = flist.tuilesPresentes[i];
        f77name(findmin)(&gmin,fld,ni_start,nj_start,ni_end,nj_end,
                &(grd[igrd].tuiles[tileIndex].ni),&(grd[igrd].tuiles[tileIndex].nj),
                &(grd[igrd].tuiles[tileIndex].ni_start),&(grd[igrd].tuiles[tileIndex].nj_start));
        f77name(findmax)(&gmax,fld,ni_start,nj_start,ni_end,nj_end,
            &(grd[igrd].tuiles[tileIndex].ni),&(grd[igrd].tuiles[tileIndex].nj),
            &(grd[igrd].tuiles[tileIndex].ni_start),&(grd[igrd].tuiles[tileIndex].nj_start));
    }

    if (fill_mode == MINIMUM) {
        fill_val = gmin - 0.1 * (gmax - gmin);
    } else {
        fill_val = gmax+ 0.1 * (gmax - gmin);
    }

    for (int i = 0; i < flist.nbTuilesAbsentes; i++) {
        tileIndex = flist.tuilesAbsentes[i];
        f77name(fillval)(&fill_val,fld,ni_start,nj_start,ni_end,nj_end,
            &(grd[igrd].tuiles[tileIndex].ni),&(grd[igrd].tuiles[tileIndex].nj),
            &(grd[igrd].tuiles[tileIndex].ni_start), &(grd[igrd].tuiles[tileIndex].nj_start));
    }

    if (grd[igrd].tuiles[tileIndex].ni_start == 1 && (*gni == (1 + grd[igrd].tuiles[tileIndex].ni))) {
        f77name(fill_lastcol2)(fld,ni_start,nj_start,ni_end,nj_end);
    }
}
