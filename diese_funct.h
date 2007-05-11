/* ------1---------2---------3---------4---------5---------6---------7---------8---------9------- */
wordint ftnstrclean(char *string, wordint length);
wordint f77name(diesinf)(wordint *key, wordint *iun, wordint *ni, wordint *nj, wordint *nk, wordint *datev, char etiket[], 
      wordint *ip1, wordint *ip2, wordint *ip3, wordint *ig1, wordint *ig2, char typvar[], char nomvar[], 
      wordint lenetiket, wordint lentypvar, wordint lennomvar);
wordint f77name(dieslir)(int *iun, wordint *key, float *buffer, float *ax, float *ay, 
        char *grref, wordint *ig1ref, wordint *ig2ref, wordint *ig3ref, wordint *ig4ref);
wordint f77name(diesfillval)(float *value);
wordint f77name(diesfillmode)(int *mode);
wordint f77name(diesaxay)(int *key, float *ax, float *ay);
wordint f77name(diesaxayprm)(int *key, wordint *ni, wordint *nj,  wordint *ip1, wordint *ip2, wordint *ip3, 
          wordint *dateo, char *typvar, char *etiket, char *grref,
          wordint *ig1ref, wordint *ig2ref, wordint *ig3ref, wordint *ig4ref, 
          wordint lentypvar, wordint lenetiket, wordint lengrref);
wordint f77name(diesclrcache)();
wordint f77name(diesisincache)(int *key);
wordint f77name(dies_getgridparams)(int *ni_start, int *nj_start, int *ni_end, int *nj_end);
/* ------1---------2---------3---------4---------5---------6---------7---------8---------9------- */
wordint c_diesFindGrid(wordint ip1, wordint ip2);
wordint c_diesinf(wordint key, wordint iun, wordint *ni, wordint *nj, wordint *nk, wordint datev, char etiket[], 
      wordint ip1, wordint ip2, wordint ip3, wordint ig1, wordint ig2, char typvar[], char nomvar[]);
wordint c_dieslir(int iun, wordint key, float *buffer, float *ax, float *ay, 
      char *grref, wordint *ig1ref, wordint *ig2ref, wordint *ig3ref, wordint *ig4ref);
wordint c_diesfillval(float value);
wordint c_diesfillmode(int mode);
wordint c_diesaxay(int key, float *ax, float *ay);
wordint c_diesaxayprm(int key, wordint *ni, wordint *nj,  wordint *ip1, wordint *ip2, wordint *ip3, 
          wordint *dateo, char *typvar, char *etiket, char *grref,
          wordint *ig1ref, wordint *ig2ref, wordint *ig3ref, wordint *ig4ref);
wordint c_diesclrcache();
wordint c_diesisincache(int key);
wordint c_diesaddkey(int key);
