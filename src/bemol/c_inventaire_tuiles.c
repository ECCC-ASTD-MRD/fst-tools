#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include <rmn/rpnmacros.h>

#include "diese.h"
#include "diese_funct.h"
#include "diese_var_e.h"

extern int compare_ints(int *token1, int *token2);

int c_inventaire_tuiles(int igrd, _Tuile *tuiles, int nb_tuiles) {
    float *tuile;
    int32_t ier, ier_ax, ier_ay, key_ax, key_ay, keyt;
    int32_t dateo, datev, npas, deet, nit, njt, nkt, nbits, datyp;
    char nomvar[5], typvar[2], etiket[13];
    int32_t nix, njx, nkx, niy, njy, nky, nig, njg;
    char grtyp[2];
    int32_t ip1, ip2, ip3;
    int32_t liste[1024], infon;
    double delta_t;
    int32_t i, j, ii, jj, n, found;
    static int32_t nmax = 1024;
    int32_t startx, starty, tile_index;
    _Tuile *inputTiles;
    static int32_t status = 0;

    if (grd[igrd].initialized == 1) {
        return status;
    }

    grd[igrd].ntuiles_total = 0;
    grd[igrd].ntuiles_x = 0;
    grd[igrd].ntuiles_y = 0;
    grd[igrd].lim_x = (int32_t *) calloc(nb_tuiles+1, sizeof(int32_t));
    grd[igrd].lim_y = (int32_t *) calloc(nb_tuiles+1, sizeof(int32_t));
    inputTiles = (_Tuile *) calloc(nb_tuiles, sizeof(_Tuile));

    for (i=0; i < nb_tuiles; i++) {
        tile_index = grd[igrd].ntuiles_total;
        inputTiles[tile_index].presente = 1;
        inputTiles[tile_index].ni_start = tuiles[i].ni_start;
        inputTiles[tile_index].nj_start = tuiles[i].nj_start;
        inputTiles[tile_index].ni = tuiles[i].ni;
        inputTiles[tile_index].nj = tuiles[i].nj;

        grd[igrd].ntuiles_total++;
        ii = 0;
        while (ii < grd[igrd].ntuiles_x && tuiles[i].ni_start != grd[igrd].lim_x[ii]) {
            ii++;
        }
        if (ii == grd[igrd].ntuiles_x) {
            grd[igrd].ntuiles_x++;
            grd[igrd].lim_x[ii] = tuiles[i].ni_start;
        }

        ii = 0;
        while (ii < grd[igrd].ntuiles_x && (tuiles[i].ni_start+tuiles[i].ni) != grd[igrd].lim_x[ii]) {
            ii++;
        }
        if (ii == grd[igrd].ntuiles_x) {
            grd[igrd].ntuiles_x++;
            grd[igrd].lim_x[ii] = tuiles[i].ni_start+tuiles[i].ni;
        }

        jj = 0;
        while (jj < grd[igrd].ntuiles_y && tuiles[i].nj_start != grd[igrd].lim_y[jj]) {
            jj++;
        }
        if (jj == grd[igrd].ntuiles_y) {
            grd[igrd].ntuiles_y++;
            grd[igrd].lim_y[jj] = tuiles[i].nj_start;
        }

        jj = 0;
        while (jj < grd[igrd].ntuiles_y && (tuiles[i].nj_start+tuiles[i].nj) != grd[igrd].lim_y[jj]) {
            jj++;
        }
        if (jj == grd[igrd].ntuiles_y) {
            grd[igrd].ntuiles_y++;
            grd[igrd].lim_y[jj] = tuiles[i].nj_start+tuiles[i].nj;
        }
    }

    qsort(grd[igrd].lim_x, grd[igrd].ntuiles_x, sizeof(int), &compare_ints);
    qsort(grd[igrd].lim_y, grd[igrd].ntuiles_y, sizeof(int), &compare_ints);

    grd[igrd].ntuiles_x--;
    grd[igrd].ntuiles_y--;
    if (grd[igrd].ntuiles_total != ((grd[igrd].ntuiles_x)*(grd[igrd].ntuiles_y))) {
        fprintf(stderr, "\n***************************************************\n");
        fprintf(stderr, "************************************************   \n");
        fprintf(stderr, "*********************************************      \n");
        fprintf(stderr, "***\n*** WARNING : The number of tiles does not match the expected number... expected %d (%dx%d), found %d !\n***\n", grd[igrd].ntuiles_x*grd[igrd].ntuiles_y, grd[igrd].ntuiles_x, grd[igrd].ntuiles_y, grd[igrd].ntuiles_total );
        fprintf(stderr, "*********************************************      \n");
        fprintf(stderr, "************************************************   \n");
        fprintf(stderr, "***************************************************\n\n");
    }

    grd[igrd].tuiles = calloc(grd[igrd].ntuiles_x*grd[igrd].ntuiles_y, sizeof(_Tuile));
    for (j=0; j < grd[igrd].ntuiles_y; j++) {
        for (i=0; i < grd[igrd].ntuiles_x; i++) {
            n = i + grd[igrd].ntuiles_x * (j);
            grd[igrd].tuiles[n].presente = 0;
            grd[igrd].tuiles[n].ni_start = grd[igrd].lim_x[i];
            grd[igrd].tuiles[n].ni = grd[igrd].lim_x[i+1]-grd[igrd].lim_x[i];
            grd[igrd].tuiles[n].nj_start = grd[igrd].lim_y[j];
            grd[igrd].tuiles[n].nj = grd[igrd].lim_y[j+1]-grd[igrd].lim_y[j];
        }
    }

    for (j=0; j < grd[igrd].ntuiles_y; j++) {
        for (i=0; i < grd[igrd].ntuiles_x; i++) {
            n = i + grd[igrd].ntuiles_x * (j);
            ii = 0;
            found = 0;
            while (ii < grd[igrd].ntuiles_total && !found) {
                if (inputTiles[ii].presente == 1) {
                    if (grd[igrd].tuiles[n].ni_start == inputTiles[ii].ni_start && grd[igrd].tuiles[n].nj_start == inputTiles[ii].nj_start) {
                        grd[igrd].tuiles[n].presente = 1;
                        inputTiles[ii].presente = 0;
                        found = 1;
                    }
                }
                ii++;
            }
        }
    }

    for (j=0; j < grd[igrd].ntuiles_y; j++) {
        for (i=0; i < grd[igrd].ntuiles_x; i++) {
            n = i + grd[igrd].ntuiles_x * (j);
            if (grd[igrd].tuiles[n].presente == 0) {
                fprintf(stderr, "\n***************************************************\n");
                fprintf(stderr, "************************************************   \n");
                fprintf(stderr, "*********************************************      \n");
                fprintf(stderr, "***\n*** Ourgh! Missing tile # %d : (%d, %d) with ni_start : %d and nj_start : %d\n***\n", n+1, i, j, grd[igrd].tuiles[n].ni_start, grd[igrd].tuiles[n].nj_start);
                fprintf(stderr, "*********************************************      \n");
                fprintf(stderr, "************************************************   \n");
                fprintf(stderr, "***************************************************\n\n");
                status = -1;
            }
        }
    }
    grd[igrd].initialized = 1;
    return status;
}


int diesGetTileIndex(int igrd, int nistart, int njstart, int ni, int nj) {
    int i = 0;
    int found = 0;

    while (i < grd[igrd].ntuiles_x*grd[igrd].ntuiles_y && !found) {
        if (nistart == grd[igrd].tuiles[i].ni_start &&
            njstart == grd[igrd].tuiles[i].nj_start &&
            ni == grd[igrd].tuiles[i].ni &&
            nj == grd[igrd].tuiles[i].nj) {
            found = 1;
        } else {
            i++;
        }
    }
    if (found == 0) {
        return -1;
    } else {
        return i;
    }
}
