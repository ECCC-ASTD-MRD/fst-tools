*** S/R FERMES FERME  LES FICHIERS SOURCES
      SUBROUTINE FERMES
      IMPLICIT      NONE
*
*AUTEURs
*VERSION ORIGINALE  - Y. BOURASSA NOV 90
*REVISION 001         "      "    MAR 92 VARIABLE NFSO (NOMBRE DE SOURCE OUVERTS)
*                                        CHANGE ALLEL A FATVOI
*         002         "      "    MAI 92 FCLOS SUB.>FUNCTION.
*     LANGUAGE FTN77
*
      INTEGER   NMR, NMS, NME, NMN, NMM, NMD
      PARAMETER(NMR=12, NMS=25, NME=20, NMN=40, NMM=10, NMD=20)
      COMMON/DESRS/  JOURS(4), NREQ, SAUV, DESEXC(NMD), SATISF(NMD),
     X               NEXC, REQ(11,4,NMD), SUP(8,NMD), NIS, NJS, NKS,
     X               IG1S, IG2S, IG3S, IG4S, REQN(NMD), REQT(NMD),
     X               REQE(NMD), Z1, Z2, Z3, ZD
      INTEGER        NREQ, SAUV, DESEXC, SATISF, NEXC, REQ, SUP, NIS, 
     X               NJS, NKS, IG1S, IG2S, IG3S, IG4S, REQN, REQT, REQE,
     X               JOURS, Z1, Z2, Z3, ZD
      COMMON /KEY/   KLE(60), DEF1(60), DEF2(60), PRINTR
      CHARACTER*8    KLE
      CHARACTER*128           DEF1,     DEF2,     PRINTR
      COMMON /CHAR/  NS, ND, SNOM, DNOM, ZE, ETI, ETIS(10,NMD), 
     X               ZT, TYP, TYPS(10,NMD), GTY, GTYS(NMD), GTYPS,
     X               ZN, NOM, NOMS(10,NMD), ETAT
      CHARACTER *1   GTY, GTYS, GTYPS
      CHARACTER *2   ZT, TYP, TYPS
      CHARACTER *4   ZN, NOM, NOMS
      CHARACTER *6   ETAT
      CHARACTER *12  ZE, ETI, ETIS
      CHARACTER *128 NS, ND
      CHARACTER *15  SNOM, DNOM
      COMMON/TAPES / MEOF, COPIES, NDS, NDD, EOF, CEOF, LEOF, LIMITE, 
     X               NFS,  NFSO,   SOURCES(35), NRECMIN
      INTEGER        MEOF, COPIES, NDS, NDD, EOF, CEOF, LEOF, LIMITE, 
     X               NFS,  NFSO,   SOURCES, NRECMIN
      COMMON/FICHES/ NP, FIXD, ECR, SSEQ, VS, OUVS, DSEQ, VD, OUVD
      INTEGER        NP
      LOGICAL            FIXD, ECR, SSEQ, VS, OUVS, DSEQ, VD, OUVD
*
*MODULES
      EXTERNAL      FSTVOI, FSTFRM, FSTRWD, FSTUNL, FSTOPC, FCLOS
*
**
      INTEGER       FSTVOI, FSTFRM, FSTRWD, FSTUNL, FSTOPC, FCLOS, I, J
      CHARACTER*128 DN
*     TRAITEMENT DES FICHIERS SOURCES
      IF( OUVS ) THEN
         IF(NFSO .GT. 1) I = FSTUNL( )
         DO 10 J=1,NFSO
            I = FSTFRM( SOURCES(J) )
            I = FCLOS(  SOURCES(J) )
   10       CONTINUE
         OUVS = .FALSE.
         NFSO = 0
      ENDIF
      RETURN
      ENTRY FERMED
*     TRAITEMENT DU FICHIER DESTINATION 
      DN = ND
      IF( OUVD ) THEN
         IF( VD ) THEN 
            I = FSTOPC('MSGLVL', 'INFORM', .FALSE.)
            IF( DSEQ ) I = FSTRWD(3)
            IF( INDEX(DNOM,'FTN') .NE. 0) THEN
               I = FSTVOI(3, 'SEQ')
            ELSE
               I = FSTVOI(3, 'STD')
            ENDIF
            I = FSTOPC('MSGLVL', DEF1(13), .FALSE.)
         ENDIF
         I = FSTFRM( 3 )
         I = FCLOS(  3 )
         OUVD = .FALSE.
      ENDIF
      RETURN
      END 
