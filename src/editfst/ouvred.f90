!**   FUNCTION OUVRE UN FICHIER DESTINATION
      FUNCTION OUVRED( DN ) 
      use app
      use rmn_fst24
      use configuration
      IMPLICIT         NONE
      INTEGER          OUVRED
      CHARACTER(len=*) DN
  
!ARGUMENTS
!SORTIE OUVRED - >=0 DIMENSION DU FICHIER OUVERT
!                <0  ERREUR DE FNOM PAS OUVERT
!ENTREE DN    -  NOM DU FICHIERUS DESTINATION
!
!AUTEUR -       Y. BOURASSA OCT 91 SEPARATION DE OUVRES
!REVISION  001  "     "     FEV 92 ENLEVE EXTERNAL INUTILE
!          002  "     "     MAR 92 CHANGE S/R EN FUNCTION
!                                  CHANGE APPEL A FNOM
!          003  "     "     MAR 92 DEPLACE LE RETURN QUAND DEJA OUVERT
!          004  "     "     MAI 92 SKIP ABORT EN INTERACTIF
!          005  "     "     DEC 94 SI FICHIER DESTINATION EST SEQUENTIEL
!                                  ON PLACE LE FICHIER A LA FIN AVANT COPIE
!          006  M. Lepine   Fev 05 Utilisation optionnelle des fichiers remotes
!          007  M. Lepine   Nov 05 Remplacement de tous les fstabt par qqexit
!          008  M. Valin    Fev-Avr 14 mode DRYRUN - simplification de la logique d'erreur
!
!LANGUAGE FTN77
!
!
!MODULES
      type(fst_record) :: record

      EXTERNAL FERMED, qqexit
      INTEGER  RENDUA, I, J, K
      integer ier
      logical :: success

      if(dryrun) then  ! mode dryrun, on fait comme si on avait ouvert le fichier de sortie
        call app_log(APP_INFO,'OUVRED: This is a dryrun, the output file will not be opened')
        ouvred = 0
        ouvd = .true.
        return
      endif
!     SI DEJA OUVERT COMME DESTINATION, TROUVER LE NOMBRE D'ENREGISTREMENTS
      IF(DN.EQ.ND .AND. OUVD) THEN
         IF(INDEX(DNOM,'SEQ') .GT. 0) THEN
            OUVRED = 0
            write(app_msg,*) 'ouvred: Sequentila file ',ND,' already opened'
            call app_log(APP_DEBUG,app_msg)
         ELSE
            OUVRED = destination%get_num_records()
            write(app_msg,*) 'ouvred: File',ND,' already opened ','Size =',OUVRED
            call app_log(APP_DEBUG,app_msg)
         ENDIF
         RETURN
      ENDIF
  
!     SI DEJA OUVERT COMME SOURCE, ERREUR FATALE
      IF(DN.EQ.NS .AND. OUVS) THEN
         call app_log(APP_ERROR,'ouvred: Destination file is the same as the source file')
         CALL qqexit(54)
      ENDIF

!     FERMER LE FICHIER DESTINATION SI OUVERT
      IF( OUVD ) CALL FERMED

!     RETOURNE True SI OUVERT
      success = destination%open(dn,DNOM//'R/W+REMOTE')

      if (.not. success) then
         call app_log(APP_ERROR,'ouvred: Cannot open file')
         CALL qqexit(55)
      endif

      ND     = DN
      ouvred = 1
      OUVD   = .TRUE.
      DSEQ   = INDEX(DNOM,'SEQ') .NE. 0
      IF( DSEQ ) THEN  ! fichier sequentiel, aller se placer a la fin
         RENDUA = 0

         success = destination%set_search_criteria()
10       COPIES = destination%find_next(record)
         IF(COPIES .GE. 0) THEN
            RENDUA = RENDUA+1
            GO TO 10
         ENDIF
         write(app_msg,*) 'ouvred: Records before copy =',RENDUA
         call app_log(APP_DEBUG,app_msg)
      ENDIF
      COPIES = 0

      RETURN
      END 
