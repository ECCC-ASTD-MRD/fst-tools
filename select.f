*** S/R SELECT INTERPRETE DES DIRECTIVES DE L'USAGER
      SUBROUTINE SELECT
      IMPLICIT NONE 
*AUTEUR       YVON R. BOURASSA JUN 86
*REVISION 001   "  "      "    OCT 90 VERSION QLXINS
*         002   "  "      "    JUL 91 DIRECTIVE ZAP
*         003   "  "      "    DEC 91 QLXINX (NEC)
*         004   "  "      "    MAI 91 QLXINX (partout sauf CRAY)
*
*LANGUAGE FTN77
*
*MODULES
      EXTERNAL QLXINX
      EXTERNAL SAUTSEQ, STDCOPI, WEOFILE, SETPER, QLXINS, ZAP,
     X         EXCLURE, SEQCOPI, REWINDS, DESIRE, FSTABT,
     X         CRITSUP, SAUTSQI, SQICOPI, READLX
      INTEGER  MOIN1, SAUTSEQ, STDCOPI, WEOFILE, SETPER, SAUTSQI, KERR, 
     X         MOIN2, EXCLURE, SEQCOPI, REWINDS, DESIRE, SQICOPI, ZAP,
     X         MOIN3, MOIN4,   CRITSUP, DUMY
      DATA     MOIN1, MOIN2, MOIN3, MOIN4/ -1, -2, -3, -4/
      integer  M1000, M1001, M1002, M1003
      data     M1000, M1001, M1002, M1003 / -1000, -1001, -1002, -1003 /
*
*IMPLICITE
      INTEGER   NMR, NMS, NME, NMN, NMM, NMD
      PARAMETER(NMR=12, NMS=25, NME=20, NMN=40, NMM=10, NMD=20)
      COMMON/LOGIQ/  SCRI, XPRES, ESAIS, DM1, DEBUG, SELEC, BOX, DIAG,
     x               INTERAC, ZA
      LOGICAL        SCRI, XPRES, ESAIS, DM1, DEBUG, SELEC, BOX, DIAG,
     x               INTERAC, ZA
      COMMON/FICHES/ NP, FIXD, ECR, SSEQ, VS, OUVS, DSEQ, VD, OUVD
      INTEGER        NP
      LOGICAL            FIXD, ECR, SSEQ, VS, OUVS, DSEQ, VD, OUVD
      COMMON/DESRS/  JOURS(4), NREQ, SAUV, DESEXC(NMD), SATISF(NMD),
     X               NEXC, REQ(11,4,NMD), SUP(8,NMD), NIS, NJS, NKS,
     X               IG1S, IG2S, IG3S, IG4S, REQN(NMD), REQT(NMD),
     X               REQE(NMD), Z1, Z2, Z3, ZD
      INTEGER        NREQ, SAUV, DESEXC, SATISF, NEXC, REQ, SUP, NIS, 
     X               NJS, NKS, IG1S, IG2S, IG3S, IG4S, REQN, REQT, REQE,
     X               JOURS, Z1, Z2, Z3, ZD
      COMMON/TAPES / MEOF, COPIES, NDS, NDD, EOF, CEOF, LEOF, LIMITE, 
     X               NFS,  NFSO,   SOURCES(35), NRECMIN
      INTEGER        MEOF, COPIES, NDS, NDD, EOF, CEOF, LEOF, LIMITE, 
     X               NFS,  NFSO,   SOURCES, NRECMIN
**
*     PREPARE LE DICTIONAIRE DE READLX
      CALL QLXINS(DEBUG  , 'DEBUG'  , DUMY, 1, 1) 
      CALL QLXINS(DIAG   , 'DIAG'   , DUMY, 1, 1) 
      CALL QLXINS(ECR    , 'ECR'    , DUMY, 1, 1) 
      CALL QLXINS(EOF    , 'EOF'    , DUMY, 1, 1) 
      CALL QLXINS(MEOF   , 'FINSEQ' , DUMY, 1, 1) 
      CALL QLXINS(FIXD   , 'FIXDATE', DUMY, 1, 1) 
      CALL QLXINS(LIMITE , 'LIMITE' , DUMY, 1, 1) 
      CALL QLXINS(CEOF   , 'SAUVEOF', DUMY, 1, 1) 
      CALL QLXINS(SAUV   , 'SAUVDES', DUMY, 1, 1) 
      CALL QLXINS(VS     , 'VOIRS'  , DUMY, 1, 1) 
      CALL QLXINS(VD     , 'VOIRD'  , DUMY, 1, 1) 
      CALL QLXINS(VD     , 'VOIR'   , DUMY, 1, 1) 
*     APELLE UN SOUS-PROGRAMME
      CALL QLXINX(DESIRE , 'DESIRE',  NP, 107, 2) 
      CALL QLXINX(CRITSUP, 'CRITSUP', NP, 108, 2) 
      CALL QLXINX(EXCLURE, 'EXCLURE', NP, 107, 2) 
      CALL QLXINX(SEQCOPI, 'FTNCOPI', NP, 107, 2) 
      CALL QLXINX(SETPER,  'PERIODE', NP, 104, 2) 
      CALL QLXINX(REWINDS, 'REWINDS', NP, 102, 2) 
      CALL QLXINX(SAUTSEQ, 'SAUTFTN', NP, 103, 2) 
      CALL QLXINX(SAUTSEQ, 'SAUTSEQ', NP, 103, 2) 
      CALL QLXINX(SAUTSQI, 'SAUTSQI', NP, 103, 2) 
      CALL QLXINX(SEQCOPI, 'SEQCOPI', NP, 107, 2) 
      CALL QLXINX(SQICOPI, 'SQICOPI', NP, 107, 2) 
      CALL QLXINX(STDCOPI, 'STDCOPI', NP, 104, 2) 
      CALL QLXINX(WEOFILE, 'STDWEOF', NP, 103, 2) 
      CALL QLXINX(ZAP,     'ZAP',     NP, 107, 2)
*     CHANGE UNE CONSTANTE
      CALL QLXINS(MOIN1  , 'TOUS'   , DUMY, 1, 0) 
      CALL QLXINS(MOIN2  , '@'      , DUMY, 1, 0) 
      CALL QLXINS(MOIN3  , 'DELTA'  , DUMY, 1, 0) 
      CALL QLXINS(MOIN4  , 'COMMUNE', DUMY, 1, 0) 
      CALL QLXINS(M1000  , 'METERS' , DUMY, 1, 0) 
      CALL QLXINS(M1001  , 'SIGMA'  , DUMY, 1, 0) 
      CALL QLXINS(M1002  , 'MBAR'   , DUMY, 1, 0) 
      CALL QLXINS(M1003  , 'OTHER'  , DUMY, 1, 0) 
      CALL QLXINS(.TRUE. , 'OUI'    , DUMY, 1, 0) 
      CALL QLXINS(.FALSE., 'NON'    , DUMY, 1, 0) 
      CALL READLX(5, DUMY, KERR)
      IF(DUMY .LT. 0) THEN
         WRITE(6,*)'  **************************************'
         WRITE(6,*)' *                                      *'
         WRITE(6,*)'*               ATTENTION                *'
         WRITE(6,*)'*       ERREUR DANS LES DIRECTIVES       *'
         WRITE(6,*)' *                                      *'
         WRITE(6,*)'  **************************************'
         CALL FSTABT 
      ENDIF
      RETURN
      END 
