*** FUNCTION OUVRE UN FICHIER DESTINATION
      FUNCTION OUVRED( DN ) 
      IMPLICIT      NONE
      INTEGER       OUVRED
      CHARACTER*(*) DN
*ARGUMENTS
*SORTIE OUVRED - >=0 DIMENSION DU FICHIER OUVERT
*                <0  ERREUR DE FNOM PAS OUVERT
*ENTREE DN    -  NOM DU FICHIERUS DESTINATION
*
*AUTEUR -       Y. BOURASSA OCT 91 SEPARATION DE OUVRES
*REVISION  001  "     "     FEV 92 ENLEVE EXTERNAL INUTILE
*          002  "     "     MAR 92 CHANGE S/R EN FUNCTION
*                                  CHANGE APPEL A FNOM
*          003  "     "     MAR 92 DEPLACE LE RETURN QUAND DEJA OUVERT
*          004  "     "     MAI 92 SKIP ABORT EN INTERACTIF
*          005  "     "     DEC 94 SI FICHIER DESTINATION EST SEQUENTIEL
*                                  ON PLACE LE FICHIER A LA FIN AVANT COPIE
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
      EXTERNAL FSTOUV, FNOM, FSTNBR, FSTINF, FERMED, FSTABT
      INTEGER  FSTOUV, FNOM, FSTNBR, FSTINF, RENDUA, I, J, K
*     SI DEJA OUVERT COMME DESTINATION, RELE NOMBRE D'ENREGISTREMENTS
      IF(DN.EQ.ND .AND. OUVD) THEN
         IF(INDEX(DNOM,'SEQ') .GT. 0) THEN
            OUVRED = 0
            IF( DEBUG ) PRINT*,'FICHIER ',ND,' DEJA OUVERT SEQUENTIEL'
         ELSE
            OUVRED = FSTNBR( 3 )
            IF( DEBUG ) PRINT*,'FICHIER ',ND,' DEJA OUVERT RANDOM ',
     X                         'TALIIE =',OUVRED
         ENDIF
         RETURN
      ENDIF
*     SI DEJA OUVERT COMME SOURCE, RIEN A FAIRE DU TOUT
      IF(DN.EQ.NS .AND. OUVS) THEN
         PRINT*,'  **************************************'
         PRINT*,' *              ATTENTION               *'
         IF( INTERAC ) THEN
            PRINT*,'*        DESTINATION = SOURCE,           *'
         ELSE
            PRINT*,'* DESTINATION = SOURCE,  ERREUR FATALE   *'
            PRINT*,' *                ABORT                 *'
         ENDIF
         PRINT*,'  **************************************'
         IF( INTERAC ) THEN
            RETURN
         ELSE
            CALL FSTABT
         ENDIF
      ENDIF
*     FERME LE FICHIER DESTINATION D'OUVERT
      IF( OUVD ) CALL FERMED
*     RETOURNE OUVRED >= 0 SI OUVERT
      IF(FNOM(3, DN, DNOM, 0) .EQ. 0) THEN
         OUVRED = FSTOUV(3, DNOM)
         ND     = DN
         OUVD   = .TRUE.
         DSEQ   = INDEX(DNOM,'SEQ') .NE. 0
         IF( DSEQ ) THEN
            RENDUA = 0
 10         COPIES = FSTINF( 3, I, J, K, -1, ' ', -1, -1, -1, ' ', ' ')
            IF(COPIES .GE. 0) THEN
               RENDUA = RENDUA+1
               GO TO 10
            ENDIF
            IF( DEBUG ) PRINT*,'ENREGISTREMENTS AVANT COPIE =',RENDUA
         ENDIF
         COPIES = 0
      ELSE
         PRINT*,'  **************************************'
         PRINT*,' *              ATTENTION               *'
         PRINT*,'* ERREUR DANS FNOM FICHIER INUTILISABLE  *'
         IF( INTERAC ) THEN
            PRINT*,'  **************************************'
         ELSE
            PRINT*,' *                ABORT                 *'
            PRINT*,'  **************************************'
            CALL FSTABT
         ENDIF
      ENDIF
      RETURN
      END 
