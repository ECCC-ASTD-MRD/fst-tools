#include <rpnmacros.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <diese.h>
#include <diese_funct.h>
#include <diese_var_e.h>


/*****************************************************************************************/
wordint c_dies_getgridparams(int *ni_start, int *nj_start, int *ni_end, int *nj_end);
wordint f77name(dies_setninj)(wordint *ni_start, wordint *ni_end, wordint *ni, wordint *nj_start, wordint *nj_end, wordint *nj) 
  {
  gni_start = *ni_start;
  gnj_start = *nj_start;
  gni_end   = *ni_end;
  gnj_end   = *nj_end;
  gni       = *ni;
  gnj       = *nj;
  
/*  fprintf(stderr, "Dies_setninj : %d-%d-%d --- %d-%d-%d\n", gni_start, gni_end, gni, gnj_start, gnj_end, gnj); */
  }
/*****************************************************************************************/
wordint f77name(dies_getgridparams)(int *ni_start, int *nj_start, int *ni_end, int *nj_end)
  {
  wordint ier;
  ier = c_dies_getgridparams(ni_start, nj_start, ni_end, nj_end);
  }

wordint f77name(diesinf)(wordint *key, wordint *iun, wordint *ni, wordint *nj, wordint *nk, wordint *datev, char etiket[], 
      wordint *ip1, wordint *ip2, wordint *ip3, wordint *ig1, wordint *ig2, char typvar[], char nomvar[], 
      wordint lenetiket, wordint lentypvar, wordint lennomvar)
  {
  wordint ier;

  ier = ftnstrclean(nomvar, lennomvar);
  ier = ftnstrclean(typvar, lentypvar);
  ier = ftnstrclean(etiket, lenetiket);

  ier = c_diesinf(*key, *iun, ni, nj, nk, *datev, etiket, *ip1, *ip2, *ip3, *ig1, *ig2, typvar, nomvar);
  return ier;
  }
/*****************************************************************************************/
wordint f77name(dieslir)(int *iun, wordint *key, float *buffer, float *ax, float *ay, 
      char *grref, wordint *ig1ref, wordint *ig2ref, wordint *ig3ref, 
      wordint *ig4ref)
  {
  wordint ier;

  ier = c_dieslir(*iun, *key, buffer, ax, ay, grref, ig1ref, ig2ref, ig3ref, ig4ref);
  return ier;

  }
/*****************************************************************************************/
wordint f77name(diesfillval)(float *value)
  {
  wordint ier;

  ier = c_diesfillval(*value);
  return ier;
  }

/* ------1---------2---------3---------4---------5---------6---------7---------8---------9------- */
wordint f77name(diesaxay)(int *key, float *ax, float *ay)
  {
  return c_diesaxay(*key, ax, ay);
  }
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9------- */
wordint f77name(diesaxayprm)(int *key, wordint *ni, wordint *nj,  wordint *ip1, wordint *ip2, wordint *ip3, 
          wordint *dateo, char *typvar, char *etiket, char *grref,
          wordint *ig1ref, wordint *ig2ref, wordint *ig3ref, wordint *ig4ref, 
          wordint lentypvar, wordint lenetiket, wordint lengrref)
  {
  wordint i, ier;

  ier = ftnstrclean(typvar, lentypvar);
  ier = ftnstrclean(etiket, lenetiket);
  ier = ftnstrclean(grref, lengrref);

  return c_diesaxayprm(*key, ni, nj, ip1, ip2, ip3, dateo, typvar, etiket, grref, ig1ref, ig2ref, ig3ref, ig4ref);
  }
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9------- */
wordint f77name(diesclrcache)()
{
  return c_diesclrcache();
}
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9------- */
wordint f77name(diesisincache)(int *key)
{
  return c_diesisincache(*key);
}


/*****************************************************************************************/
wordint c_diesinf(wordint key, wordint iun, wordint *ni, wordint *nj, wordint *nk, wordint datev, char etiket[], 
      wordint ip1, wordint ip2, wordint ip3, wordint ig1, wordint ig2, char typvar[], char nomvar[])
{
  wordint ier, ier_ax, ier_ay, key_ax, key_ay;
  wordint dateo, npas, deet, nit, njt, nkt, nbits, datyp;
  wordint nix, njx, nkx, niy, njy, nky;
  char grtyp, grref;
  wordint ig1t, ig2t, ig3t, ig4t, ig1ref, ig2ref, ig3ref, ig4ref;
  wordint swa, lng, dltf, ubc, extra1, extra2, extra3;

  char lnomvar[5],ltypvar[3],lgrtyp[2],letiket[13];
  wordint i, ndiese;

  strcpy(lnomvar, "    ");
  strcpy(letiket, "            ");
  strcpy(lgrtyp,  " ");
  strcpy(ltypvar, "  ");

  strncpy(lnomvar, nomvar, 4);
  strncpy(letiket, etiket, 12);
  strncpy(ltypvar, typvar, 2);

  ier = c_fstprm(key, &dateo, &deet, &npas, &nit, &njt, &nkt, &nbits,
    &datyp, &ip1, &ip2, &ip3, ltypvar, lnomvar, letiket,
    lgrtyp, &ig1t, &ig2t, &ig3t, &ig4t, &swa, &lng, &dltf,
    &ubc, &extra1, &extra2, &extra3);

  if (lgrtyp[0] != '#')
    {
    ier = -13;
    return ier;
    }

  key_ax = c_fstinf(iun, &nix, &njx, &nkx, -1, "            ", ig1t, ig2t, -1, "  ", ">>  ");
  key_ay = c_fstinf(iun, &niy, &njy, &nky, -1, "            ", ig1t, ig2t, -1, "  ", "^^  ");
  if (key_ax < 0 || key_ay < 0)
    {
    ier = -1313;
    return ier;
    }

  ndiese = currentdiese;
  i = c_diesFindGrid(ig1t, ig2t);
  if (i > ndiese)
    {
    grd[i].key_ax = key_ax;
    grd[i].key_ay = key_ay;
    strcpy(grd[i].nomvarx, "    ");
    strcpy(grd[i].typvarx, " ");
    strcpy(grd[i].etiketx, "            ");
    strcpy(grd[i].grref,  " ");
    strcpy(grd[i].nomvary, "    ");
    strcpy(grd[i].typvary, " ");
    strcpy(grd[i].etikety, "            ");


    ier = c_fstprm(grd[i].key_ax, &grd[i].dateo, &grd[i].deet, &grd[i].npas, &grd[i].nix, &grd[i].njx, &grd[i].nkx, &grd[i].nbits,
      &grd[i].datyp, &grd[i].ip1, &grd[i].ip2, &grd[i].ip3, grd[i].typvarx, grd[i].nomvarx, grd[i].etiketx,
      grd[i].grref, &grd[i].ig1ref, &grd[i].ig2ref, &grd[i].ig3ref, &grd[i].ig4ref, &grd[i].swa, &grd[i].lng, &grd[i].dltf,
      &grd[i].ubc, &grd[i].extra1, &grd[i].extra2, &grd[i].extra3);

    ier = c_fstprm(grd[i].key_ay, &grd[i].dateo, &grd[i].deet, &grd[i].npas, &grd[i].niy, &grd[i].njy, &grd[i].nky, &grd[i].nbits,
      &grd[i].datyp, &grd[i].ip1, &grd[i].ip2, &grd[i].ip3, grd[i].typvary, grd[i].nomvary, grd[i].etikety,
      grd[i].grref, &grd[i].ig1ref, &grd[i].ig2ref, &grd[i].ig3ref, &grd[i].ig4ref, &grd[i].swa, &grd[i].lng, &grd[i].dltf,
      &grd[i].ubc, &grd[i].extra1, &grd[i].extra2, &grd[i].extra3);

    grd[i].ax = (float *) malloc(nix*sizeof(float));
    grd[i].ay = (float *) malloc(njy*sizeof(float));

    ier = c_fstluk(grd[i].ax, grd[i].key_ax, &nix, &njx, &nkx);
    ier = c_fstluk(grd[i].ay, grd[i].key_ay, &niy, &njy, &nky);


    }

  *ni = grd[i].nix;
  *nj = grd[i].njy;
  *nk = grd[i].nky;

  return key;


}
/*****************************************************************************************/
wordint c_dies_getgridparams(int *ni_start, int *nj_start, int *ni_end, int *nj_end)
  {
  wordint igrd = 0;
  *ni_start = grd[igrd].lim_x[0];
  *nj_start = grd[igrd].lim_y[0];
  *ni_end   = grd[igrd].lim_x[grd[igrd].ntuiles_x]-1;
  *nj_end   = grd[igrd].lim_y[grd[igrd].ntuiles_y]-1;
  
/*  fprintf(stderr, "getgridparams: (%d, %d)), (%d, %d)\n", *ni_start, *nj_start, *ni_end, *nj_end); */
  }
/*****************************************************************************************/
wordint c_dieslir(int iun, wordint key, float *buffer, float *ax, float *ay, 
      char *grref, wordint *ig1ref, wordint *ig2ref, wordint *ig3ref, wordint *ig4ref)
{
  float *tuile;
  wordint ier, ier_ax, ier_ay, key_ax, key_ay, keyt;
  wordint dateo, datev, npas, deet, nit, njt, nkt, nbits, datyp;
  char nomvar[5], typvar[2], etiket[13];
  wordint nix, njx, nkx, niy, njy, nky, nig, njg;
  char grtyp[2];
  wordint ip1, ip2, ip3; 
  wordint ig1t, ig2t, ig3t, ig4t;
  wordint swa, lng, dltf, ubc, extra1, extra2, extra3;
  wordint liste[1024], infon;
  double delta_t;
  wordint i, j;
  static wordint nmax = 1024;
  wordint startx, starty;

  wordint dateox, datevx, npasx, deetx, datypx, nbitsx;
  char nomvarx[5], typvarx[2], etiketx[13];
  wordint ip1x, ip2x, ip3x;

  strcpy(nomvar, "    ");
  strcpy(typvar, " ");
  strcpy(etiket, "            ");
  strcpy(grtyp,  " ");

  ier = c_fstprm(key, &dateo, &deet, &npas, &nit, &njt, &nkt, &nbits,
    &datyp, &ip1, &ip2, &ip3, typvar, nomvar, etiket,
    grtyp, &ig1t, &ig2t, &ig3t, &ig4t, &swa, &lng, &dltf,
    &ubc, &extra1, &extra2, &extra3);
  igrd = c_diesFindGrid(ig1t, ig2t);

  if (grtyp[0] != '#')
    {
    ier = -13;
    return ier;
    }

  if (igrd == ndiese)
    {
    ier = -13;
    return ier;
    }

  strcpy(nomvarx, "    ");
  strcpy(typvarx, " ");
  strcpy(etiketx, "            ");
  strcpy(grref,  " ");

  delta_t = (double) (deet * npas)/3600.0;
  f77name(incdatr)(&datev, &dateo, &delta_t);
  ier = c_fstinl(iun, &nit, &njt, &nkt, datev, etiket, ip1, ip2, -1, typvar, nomvar, liste, &infon, nmax);
  ier = c_inventaire_tuiles(igrd, liste, infon, ig1t, ig2t);
  
  if (ier < 0) return ier;

  nig = grd[igrd].nix;
  njg = grd[igrd].njy;

  memset(buffer, NULL, sizeof(float)*nig*njg);
/*  for (i=0; i < nig*njg; i++)
    {
    buffer[i] = 0.0;
    }
*/

  for (i=0; i < nig; i++)
    {
    ax[i] = grd[igrd].ax[i];
    }

  for (j=0; j < njg; j++)
    {
    ay[j] = grd[igrd].ay[j];
    }

  for (i=0; i <  infon; i++)
    {
    strcpy(nomvar, "    ");
    strcpy(typvar, " ");
    strcpy(etiket, "            ");
    strcpy(grtyp,  " ");

    ier = c_fstprm(liste[i], &dateo, &deet, &npas, &nit, &njt, &nkt, &nbits,
      &datyp, &ip1, &ip2, &ip3, typvar, nomvar, etiket,
      grtyp, &ig1t, &ig2t, &ig3t, &ig4t, &swa, &lng, &dltf,
      &ubc, &extra1, &extra2, &extra3);
    if (ig1t == grd[igrd].ip1 && ig2t == grd[igrd].ip2)
      {
      tuile = (float *) malloc(nit*njt*sizeof(float));
      ier = c_fstluk(tuile, liste[i], &nit, &njt, &nkt);
      startx = ig3t;
      starty = ig4t;
      f77name(fillgrid)(buffer,tuile,&nig,&njg,&nit,&njt,&startx,&starty);
      if (startx == 1 && ((startx - 1) + nit) < nig)
        {
        f77name(fill_lastcol)(buffer,tuile,&nig,&njg,&nit,&njt,&startx,&starty);
        }
      c_diesaddkey(liste[i]);
      free(tuile);
      }
    }

  grref[0] = grd[igrd].grref[0];
  *ig1ref   = grd[igrd].ig1ref;
  *ig2ref   = grd[igrd].ig2ref;
  *ig3ref   = grd[igrd].ig3ref;
  *ig4ref   = grd[igrd].ig4ref;
  ier = 0;
  return ier;

}
/*****************************************************************************************/
wordint c_diesfillval(float value)
{
  wordint ier = 0;

  return ier;
}

/*****************************************************************************************/
wordint c_diesfillmode(int mode)
{
  wordint ier = 0;

  return ier;

}

/* ------1---------2---------3---------4---------5---------6---------7---------8---------9------- */
wordint c_diesaxay(int key, float *ax, float *ay)
{
  wordint ier = 0;

  return ier;
}

/* ------1---------2---------3---------4---------5---------6---------7---------8---------9------- */
wordint c_diesaxayprm(int key, wordint *ni, wordint *nj,  wordint *ip1, wordint *ip2, wordint *ip3, 
          wordint *dateo, char *typvar, char *etiket, char *grref,
          wordint *ig1ref, wordint *ig2ref, wordint *ig3ref, wordint *ig4ref)
  {
  wordint ier = 0;

  return ier; 
  }
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9------- */
wordint c_diesclrcache()
  {
  free(dieskeys);
  dieskeys = (wordint *) NULL;
  ndieskeys = 0;
  return 0;
  }
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9------- */
wordint c_diesisincache(key)
{
  wordint i;

  if (dieskeys == NULL) return -1;

  for (i=0; i < ndieskeys; i++)
    {
    if (key == dieskeys[i]) return key;
    }
  return -1;
}
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9------- */
wordint c_diesaddkey(int key)
{
  wordint  i, n;

  n = currentdiese;
if (dieskeys == NULL)
  {
  dieskeys = (wordint *) malloc(256*sizeof(int));
  }

for (i=0; i < ndieskeys; i++)
  {
  if (key == dieskeys[i]) return 0;
  }

dieskeys[i] = key;
ndieskeys++;

if (0 == ndieskeys%256)
  {
  dieskeys = (wordint *) realloc(dieskeys, (ndieskeys+256)*sizeof(int));
  }

return 0;

}

/* ------1---------2---------3---------4---------5---------6---------7---------8---------9------- */
wordint c_diesFindGrid(wordint ip1, wordint ip2)
{
  wordint i = 0;

  i = 0;
  while (i < ndiese && (grd[i].ip1 != ip1 || grd[i].ip2 != ip2))
    {
    i++;
    }

  if (i > currentdiese)
    {
    c_diese_AddGrid(i, ip1, ip2);
    currentdiese++;
    ndiese++;
    }
  else
    {
    currentdiese = i;
    }

  if (i > ndiesemax)
    {
    fprintf(stderr, "Trop de grilles diese (#############) !\n");
    exit(1313);
    }

  return currentdiese;
}
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9------- */

int compare_ints(int *token1, int *token2)
{
  if (*token1 < *token2)
    {
    return -1;
    }
  else
    {
    if (*token1 > *token2)
      {
      return 1;
      }
    else
      {
      return 0;
      }
    }
}


  /* -------------------------------------------------------------------*/     

  
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

/* ------1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-- */

int c_diese_AddGrid(int igrid, int ip1, int ip2)
  {
  int old_ndiese;
  float *tuile;
  wordint ier, ier_ax, ier_ay, key_ax, key_ay, keyt;
  wordint dateo, datev, npas, deet, nit, njt, nkt, nbits, datyp;
  char nomvar[5], typvar[2], etiket[13];
  wordint nix, njx, nkx, niy, njy, nky, nig, njg;
  char grtyp[2];
  wordint ig1t, ig2t, ig3t, ig4t;
  wordint swa, lng, dltf, ubc, extra1, extra2, extra3;
  wordint liste[1024], infon;
  double delta_t;
  wordint i, j,iun;
  static wordint nmax = 1024;
  wordint startx, starty;

  wordint dateox, datevx, npasx, deetx, datypx, nbitsx;
  char nomvarx[5], typvarx[2], etiketx[13];
  wordint ip1x, ip2x, ip3x;
  
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


  ier = c_fstprm(grd[i].key_ax, &grd[i].dateo, &grd[i].deet, &grd[i].npas, &grd[i].nix, &grd[i].njx, &grd[i].nkx, &grd[i].nbits,
    &grd[i].datyp, &grd[i].ip1, &grd[i].ip2, &grd[i].ip3, grd[i].typvarx, grd[i].nomvarx, grd[i].etiketx,
    grd[i].grref, &grd[i].ig1ref, &grd[i].ig2ref, &grd[i].ig3ref, &grd[i].ig4ref, &grd[i].swa, &grd[i].lng, &grd[i].dltf,
    &grd[i].ubc, &grd[i].extra1, &grd[i].extra2, &grd[i].extra3);

  ier = c_fstprm(grd[i].key_ay, &grd[i].dateo, &grd[i].deet, &grd[i].npas, &grd[i].niy, &grd[i].njy, &grd[i].nky, &grd[i].nbits,
    &grd[i].datyp, &grd[i].ip1, &grd[i].ip2, &grd[i].ip3, grd[i].typvary, grd[i].nomvary, grd[i].etikety,
    grd[i].grref, &grd[i].ig1ref, &grd[i].ig2ref, &grd[i].ig3ref, &grd[i].ig4ref, &grd[i].swa, &grd[i].lng, &grd[i].dltf,
    &grd[i].ubc, &grd[i].extra1, &grd[i].extra2, &grd[i].extra3);

  grd[i].ax = (float *) malloc(nix*sizeof(float));
  grd[i].ay = (float *) malloc(njy*sizeof(float));

  ier = c_fstluk(grd[i].ax, grd[i].key_ax, &nix, &njx, &nkx);
  ier = c_fstluk(grd[i].ay, grd[i].key_ay, &niy, &njy, &nky);
  return 0;
  }

  
