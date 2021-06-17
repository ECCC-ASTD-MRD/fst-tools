#define ABORT       13
#define MINIMUM      1
#define MAXIMUM      2
#define MISSING      3

#define FTN2C(i,j,ni)  (int)((ni) * (j-1) + i-1)


typedef struct
  {
  wordint presente;
  wordint key;
  wordint ni_start, ni_end, ni;
  wordint nj_start, nj_end, nj;
  wordint nk;
  wordint ig1ref, ig2ref;
  float rmin, rmax;
  } _Tuile;


typedef struct
  {
  wordint key_ax, key_ay,ip1, ip2, ip3, dateo;
  wordint nix, njx, nkx, niy, njy, nky;
  wordint npas, deet, nbits;
  wordint ig1ref, ig2ref, ig3ref, ig4ref;
  wordint datyp, swa, lng, dltf, ubc, extra1, extra2, extra3;

  char nomvarx[8];
  char nomvary[8];
  char typvarx[4];
  char typvary[4];
  char etiketx[16];
  char etikety[16];

  char grref[4];
  
  wordint ntuiles_total;
  wordint ntuiles_x, ntuiles_y;
  wordint initialized;
  wordint *lim_x;
  wordint *lim_y;
  _Tuile *tuiles;
  float *ax, *ay;
  } _Diese;
  
typedef struct
  {
  wordint key,ip1, ip2, ip3, dateo;
  wordint npas, deet, nbits, datyp;
  wordint ig1, ig2, ig3, ig4;
  wordint ni, nj, nk;

  char nomvar[8];
  char typvar[4];
  char etiket[16];
  char grtyp[4];
  } _Fld;
  
typedef struct
  {
  _Fld fldinfo;
  _Tuile *tuiles;
  int *masque;
  int *tuilesPresentes;
  int *tuilesAbsentes;
  int nb_tuiles, nbTuilesPresentes, nbTuilesAbsentes;
  } _Fldlst;

