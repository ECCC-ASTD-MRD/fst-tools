!** S/R SQICOPI COPIE UN FICHIER SEQUENTIEL SQI DANS UN FICHIER STANDARD
      SUBROUTINE SQICOPI(INPT, OUPT, TD, PR, TO, PS, NB)
      use configuration
      use app
      use rmn_fst24
      IMPLICIT   NONE
      INTEGER    INPT(*), OUPT(*), TD(*), PR, TO, PS, NB
!
!ARGUMENTS
!  ENTRE    - INPT  - DN  FICHIER SOURCE
!    "      - OUPT  -  "     "    DESTINATION
!    "      - TD    - TYPE   "         "      (SEQ,RND,SEQ+FTN)
!    "      - PR    - EOF LIGIQUE PRECEDANT ENREGISTREMENT RECHERCHE [0]
!    "      - TO    - EOF LIGIQUE TERMINANT ENREGISTREMENT RECHERCHE [0]
!    "      - PS    - EOF LIGIQUE QUI TERMINE LE CYCLE [0]
!    "      - NB    - NOMBRE DE BOUCLE A EXECUTER [MIN = 1] 
!
!AUTEURS
!VERSION ORIGINALE  Y. BOURASSA AVRL 86
!REVISION 001       "      "    NOV  90 ACCEPTE FICHIER SQI COMME FICHIER SOURCE
!         002       "      "    JUIL 91 ACCEPTE -1 POUR LES ARGUMENTS INPT, OUPT, TD
!         003       "      "    MARS 92 FICHIER SOURCE DOIT EXISTER CHANGE TEST FSTINF
!                                       CHANGE ALLEL A OUVRED
!         004       "      "    MAI 92  SKIP ABORT SI EN INTERACTIF
!                                       OUVRE FICHIER SOURCE AVANT DESTINATION
!         005       M. Lepine   Nov 05  rempalcement de fstabt par qqexit
!         006       M. Valin    Mai 14 Remplacement des comdecks par un module
!
!LANGUAGE   - FTN77 
!
!MODULES  
      EXTERNAL      SAUVDEZ, qqexit, COPYSTX, OUVRES, OUVRED, DMPDES
      EXTERNAL      ARGDIMS, FERMES, LOW2UP
      INTEGER       ARGDIMS, OUVRED, NI, NJ, NK
      INTEGER       N, M, I, J, success
      INTEGER       PRE, CSD,  POS
      CHARACTER(len=128) :: DD
      type(fst_file)     :: fstfile
 
!     INITIALISATION
      SNOM = 'STD+SEQ+OLD'
    1 PRE  = 0
      CSD  = 0
      POS  = 0
      N    = -1
      fstfile = SOURCES(1)
!     UTILISATION DES PARAMETRES PASSES PAR L'APPELEUR
!     DIRECTION A PRENDRE SELON LE NOMBRE D'ARGUMENTS PASSES
      GO TO(50, 50, 50, 40, 30, 20, 10) NP
   10 N    = NB
   20 POS  = PS
   30 CSD  = TO
   40 PRE  = PR
      IF(PRE .GT. MEOF) THEN
         WRITE(app_msg,*) 'sqicopi: Cannot copy passed EOF (PRE=',PRE,', MAXEOF=',MEOF,')'
         call app_log(APP_ERROR,app_msg)
         IF( INTERAC ) THEN
            RETURN
         ELSE
            CALL qqexit(70)
         ENDIF
      ENDIF


!     FICHIER DESTINATION
   50 IF(NP.GE.3 .AND. (TD(1).NE.-1 .AND. OUPT(1).NE.-1)) THEN
         WRITE(DNOM, LIN128) (TD(I), I=1,ARGDIMS(3)) 
         CALL LOW2UP(DNOM, DNOM)
         IF(INDEX(DNOM,'FTN') .GT. 0) THEN
            DNOM = 'STD+SEQ+FTN' 
         ELSEIF(INDEX(DNOM,'SEQ').GT.0 .OR. INDEX(DNOM,'SQI').GT.0) THEN
            DNOM = 'STD+SEQ'
         ELSE
            DNOM = 'STD+RND'
         ENDIF
      ENDIF

!     FICHIER SOURCE
      IF(INPT(1) .NE. -1) THEN
         WRITE(DD,LIN128) (INPT(I),I=1,ARGDIMS(1))
         NFS = 1
         CALL OUVRES( DD )
      ENDIF
      IF( .NOT. OUVS ) THEN
         call app_log(APP_ERROR,'sqicopi: Source file unknown')
         IF( INTERAC ) THEN
            RETURN
         ELSE
            CALL qqexit(71)
         ENDIF
      ENDIF

      IF(NP.GE.2 .AND. (OUPT(1) .NE. -1)) THEN
         WRITE(DD,LIN128) (OUPT(I),I=1,ARGDIMS(2))
         I = OUVRED( DD )
      ENDIF
      IF( .NOT. OUVD) THEN
         call app_log(APP_ERROR,'sqicopi: Destination file unknown')
         IF( INTERAC ) THEN
            RETURN
         ELSE
            CALL qqexit(72)
         ENDIF
      ENDIF

!     DEVONS=NOUS METTRE COPYSTX,EN MODE XPRES?
      CALL DMPDES
      M    = MEOF
      MEOF = CSD
  
!     BOUCLE DES N REPETITIONS (N=-1 ON BOUCLE JUSQUA LA FIN)
!     SAUTE DES MARQUES DE FIN DE FICHIER LOGIQUES AVANT COPIE
   80 LEOF = 0
  
      IF(PRE .GT. 0) THEN
!        SAUTE AU PROCHAIN EOF NIVEAU PRE
         success = fstfile%set_search_criteria(ni=ni,nj=nj,nk=nk,datev=0_int64,etiket='0           ',ip1=0,ip2=0,ip3=0,nomvar='0   ',typvar='0 ')

   90    IF (fstfile%find_next()) GOTO 90
         LEOF = fstfile%eof()
         WRITE(app_msg,*) 'sqicopi: Encountered EOF #',LEOF
         call app_log(APP_DEBUG,app_msg)
         IF(LEOF.GT.15 .OR. LEOF.LT.1) THEN
            WRITE(app_msg,*) 'sqicopi: ',LEOF,' is not an acceptable logical EOF'
            call app_log(APP_ERROR,app_msg)
            CALL qqexit(73)
         ENDIF
         IF(LEOF .LT. PRE) GO TO 90
         IF(LEOF.GE.POS .AND. POS.NE.0) GO TO 110 
      ENDIF
  
!     SI POSSIBLE COPIE JUAQU'AU PROCHAIN EOF DESIGNE
      IF(LEOF.LT.M .AND. (LEOF.LT.CSD .OR. CSD.EQ.0)) THEN
         IF(LIMITE .NE. 0) THEN
            CALL COPYSTX
         ELSE
            call app_log(APP_WARNING,'sqicopi: Transfer limit already reached')
            GO TO 120
         ENDIF
      ENDIF
  
!     SAUTE DES MARQUES DE FIN DE FICHIER LOGIQUES APRES COPIE
      IF(LEOF .LT. POS) THEN
!        SAUTE AU PROCHAIN EOF NIVEAU POS
         success = fstfile%set_search_criteria(ni=ni,nj=nj,nk=nk,datev=0_int64,etiket='0           ',ip1=0,ip2=0,ip3=0,nomvar='0   ',typvar='0 ')

  100    IF (fstfile%find_next()) GOTO 100
         LEOF = fstfile%eof()
         WRITE(app_msg,*) 'sqicopi: Encountered EOF #',LEOF
         call app_log(APP_DEBUG,app_msg)
         IF(LEOF.GT.15 .OR. LEOF.LT.1) THEN
            WRITE(app_msg,*) 'sqicopi: ',LEOF,' is not an acceptable logical EOF'
            call app_log(APP_ERROR,app_msg)
            CALL qqexit(74)
         ENDIF
         IF(LEOF .LT. POS) GO TO 100
      ENDIF
  
  110 IF(LEOF .LT. M) THEN
         N = N-1
         IF(N .NE. 0) GO TO 80
      ENDIF
  
!     CONTROLE DE LA PORTEE DES DIRECTIVES
  120 CALL SAUVDEZ
      MEOF = M
      RETURN
  
!**   COPIE UN FICHIER SEQUENTIEL FTN DANS UN FICHIER STANDARD
      ENTRY SEQCOPI(INPT, OUPT, TD, PR, TO, PS, NB)
      SNOM = 'STD+SEQ+FTN+OLD'
      GO TO 1
  
      END 
