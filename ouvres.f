*** S/R OUVRES OUVRE UN FICHIER SOURCE
      SUBROUTINE OUVRES( DN ) 
      IMPLICIT      NONE
      CHARACTER*(*) DN(35)
*ARGUMENTS
* ENTREE DN    -  LISTE DES NOMS DE FICHIER SOURCE
*
*AUTEUR -      Y. BOURASSA SEP 90
*REVISION 001  "      "    OCT 90 VERSION QLXINS, STANDARD 90
*         002  "      "     "  91 LINK FICHIERS SOURCE
*         003  "      "    FEV 92 CHECK SI FICHIER INEXISTANT
*                                 FNOM-OUVRE-LINK VIA FSTNOL
*                                 CHANGE APPEL A FSTVOI
*         004  "      "    NOV 92 liimite a 15 nombre de fichiers
*LANGUAGE FTN77
*
      INTEGER   NMR, NMS, NME, NMN, NMM, NMD
      PARAMETER(NMR=12, NMS=25, NME=20, NMN=40, NMM=10, NMD=20)
      COMMON/LOGIQ/  SCRI, XPRES, ESAIS, DM1, DEBUG, SELEC, BOX, DIAG,
     x               INTERAC, ZA
      LOGICAL        SCRI, XPRES, ESAIS, DM1, DEBUG, SELEC, BOX, DIAG,
     x               INTERAC, ZA
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
      EXTERNAL FSTNOL, FSTVOI, FSTRWD, FERMES, FERMED
      INTEGER  FSTNOL, FSTVOI, FSTRWD, I
*     TRAITEMENT DU FICHIER SOURCE
*     si le fichier source etait ouvert comme fichier destination
      IF( DN(1).EQ.ND .AND. OUVD ) CALL FERMED
      IF( OUVS ) THEN
         IF( DN(1).EQ.NS .AND. NFS.EQ.1 ) RETURN
*        si on change de fichier source
         CALL FERMES
      ENDIF
      I    = FSTNOL(SOURCES, DN, NFS, SNOM)
      NFSO = NFS
      SSEQ = INDEX(SNOM,'SEQ') .GT. 0
*     REFERME LE FICHIER SI 'RND' ET VIDE
      IF(SSEQ .OR. (I .GE. 0)) THEN
         OUVS = .TRUE.
         NS   = DN(1)
         IF( VS ) THEN
            IF( SSEQ ) I = FSTRWD( SOURCES ) 
            IF( INDEX(SNOM,'FTN') .GT. 0) THEN
               I = FSTVOI(SOURCES, 'SEQ')
            ELSE
               I = FSTVOI(SOURCES, 'STD')
            ENDIF
            IF( SSEQ ) I = FSTRWD( SOURCES ) 
         ENDIF
      ELSE
         CALL FERMES
      ENDIF
      RETURN
      END 
