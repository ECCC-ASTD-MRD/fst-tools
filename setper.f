*** S/R SETPER ETABLIR UNE DATE (DEBUT/FIN DE PERIODE)
      SUBROUTINE SETPER(DN, ECART, DUREE, DELTA)
      IMPLICIT NONE 
      INTEGER    DN(*), ECART, DUREE, DELTA
*
*ARGUMENTS
*  ENTRE    - DN    - A) 'OPRUN'
*                     C) 'YYJJJZZ'
*                     D) -(CMCSTAMP)
*    "      - ECART - NOMBRE D'HEURES PAR LEQUEL IF FAUT MODIFIER
*                     LA DATE QUI VIENT DU FICHIER "DN" (SI PRESENT)
*    "      - DUREE - DUREE DE LA PERIODE (SI PRESENT)
*    "      - DELTA - INTERVALE EN HEURES ENTRE LES CAS (SI PRESENT)
*
*AUTEURS
*         Y BOURASSA NOV  90
*REV 001  "     "    JUIL 91 ACCEPTE LES DATESTAMP CMC *
*REV 002  "     "    FEV  92 APPEL A LOW2UP AVANT IOPDATM
*
*LANGUAGE   - FTN77 
*
      INTEGER   NMR, NMS, NME, NMN, NMM, NMD
      PARAMETER(NMR=12, NMS=25, NME=20, NMN=40, NMM=10, NMD=20)
      COMMON/LOGIQ/  SCRI, XPRES, ESAIS, DM1, DEBUG, SELEC, BOX, DIAG,
     x               INTERAC, ZA
      LOGICAL        SCRI, XPRES, ESAIS, DM1, DEBUG, SELEC, BOX, DIAG,
     x               INTERAC, ZA
      CHARACTER*8 LIN128
      PARAMETER   (LIN128='(32A4)')
      COMMON/DESRS/  JOURS(4), NREQ, SAUV, DESEXC(NMD), SATISF(NMD),
     X               NEXC, REQ(11,4,NMD), SUP(8,NMD), NIS, NJS, NKS,
     X               IG1S, IG2S, IG3S, IG4S, REQN(NMD), REQT(NMD),
     X               REQE(NMD), Z1, Z2, Z3, ZD
      INTEGER        NREQ, SAUV, DESEXC, SATISF, NEXC, REQ, SUP, NIS, 
     X               NJS, NKS, IG1S, IG2S, IG3S, IG4S, REQN, REQT, REQE,
     X               JOURS, Z1, Z2, Z3, ZD
      COMMON/FICHES/ NP, FIXD, ECR, SSEQ, VS, OUVS, DSEQ, VD, OUVD
      INTEGER        NP
      LOGICAL            FIXD, ECR, SSEQ, VS, OUVS, DSEQ, VD, OUVD
*
*MODULES  
      EXTERNAL      IOPDATM, ARGDIMS, INCDAT, DATMGP, FSTABT, JULHR,
     X              LOW2UP
*
**
      INTEGER       IOPDATM, ARGDIMS, DTG(14), I, K
      EQUIVALENCE   (K, DTG(14))
      CHARACTER*128 C
*     ETABLIR LE DATESTAMP DU CAS OU DU DEBUT DE LA PERIODE 
      IF(DN(1) .LT. 0) THEN
         K = -DN(1)
      ELSE
         WRITE(C,LIN128) (DN(I), I=1,ARGDIMS(1))
         CALL LOW2UP(C, C)
         K = IOPDATM( C )
         IF(K .EQ. 10101011) THEN
            PRINT*,'DATE DE IOPDATM INACCEPTABLE'
            CALL FSTABT 
         ENDIF
      ENDIF
*     SI ON DOIT MODIFIER LA DATE PASSEE
      IF(NP .GT. 1) CALL INCDAT(K, K, ECART)
      CALL JULHR(JOURS(1) , K)
      JOURS(2) = 0
      JOURS(3) = 0
      JOURS(4) = 1
*     SI ON A DONNE UNE DATA SIMPLE
      IF(NP .LT. 3) THEN
         IF( DEBUG ) THEN
            CALL DATMGP( DTG )
            WRITE(6,600) (DTG(I),I=7,13), JOURS(1)
  600       FORMAT(' * CAS DU '7A4,'  JULH = ',I8)
         ENDIF
      ELSE
         IF(DUREE .GE. 0) THEN
            JOURS(2) = JOURS(1) + DUREE 
         ELSE
            JOURS(2) = JOURS(1)
            JOURS(1) = JOURS(1) + DUREE 
         ENDIF
         IF(NP .GT. 3) THEN
            JOURS(3) = ABS( DELTA )
         ELSE
            JOURS(3) = 1
         ENDIF
         JOURS(4) = -1
         IF( DEBUG ) THEN
            CALL DATMGP( DTG )
            WRITE(6,601) (DTG(I),I=7,13)
  601       FORMAT(' * DEBUT *  ', 7A4) 
            CALL INCDAT(K, K, DUREE)
            CALL DATMGP( DTG )
            WRITE(6,602) (DTG(I),I=7,13)
  602       FORMAT(' *  FIN  *  ',7A4)
            WRITE(6,*) JOURS(1), ' @ ', JOURS(2), ' DELTA ', JOURS(3) 
         ENDIF
      ENDIF
      RETURN
      END 
