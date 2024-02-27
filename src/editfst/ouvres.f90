!** S/R OUVRES OUVRE UN FICHIER SOURCE
      SUBROUTINE OUVRES( DN ) 
      use configuration
      IMPLICIT      NONE
      CHARACTER(len=*), dimension(*) :: DN(120)
  
!ARGUMENTS
! ENTREE DN    -  LISTE DES NOMS DE FICHIER SOURCE
!
!AUTEUR -      Y. BOURASSA SEP 90
!REVISION 001  "      "    OCT 90 VERSION QLXINS, STANDARD 90
!         002  "      "     "  91 LINK FICHIERS SOURCE
!         003  "      "    FEV 92 CHECK SI FICHIER INEXISTANT
!                                 FNOM-OUVRE-LINK VIA FSTNOL
!                                 CHANGE APPEL A FSTVOI
!         004  "      "    NOV 92 liimite a 15 nombre de fichiers
!         005  M. Lepine   Juil 2005, limite du nombre de fichiers a 120
!         006  M. Valin    Mai  2014, remplacement des comecks par un module
!LANGUAGE FTN77
!
!
!MODULES
      EXTERNAL FERMES, FERMED
      INTEGER  i,j,k
      logical :: success

!     TRAITEMENT DU FICHIER SOURCE
!     si le fichier source etait ouvert comme fichier destination
      IF( DN(1).EQ.ND .AND. OUVD ) CALL FERMED
      IF( OUVS ) THEN
         IF( DN(1).EQ.NS .AND. NFS.EQ.1 ) RETURN
!        si on change de fichier source
         CALL FERMES
      ENDIF

     if (nfs.gt.1 .and. ((index(SNOM,'SEQ') + index(SNOM,'SQI') + index(SNOM,'FTN')) .ne. 0)) then
         call app_log(APP_ERROR,'ouvres: Cannot chain sequential files')
         nfs = 0
      else
        do 20 i=1,nfs
            if(.not. sources(i)%open(dn(i),SNOM//'+REMOTE')) then
               do 10 j=1,i
                  success = sources(i)%close()
   10          continue
               nfs = 0
               goto 30
            endif
   20    continue

   30    if (nfs .gt. 1) then
            if (.not. fst24_link(sources)) then
               call app_log(APP_ERROR, 'Unable to link source files')
               return
            endif
         endif
      endif

      NFSO = NFS
      SSEQ = INDEX(SNOM,'SEQ') .GT. 0

!     REFERME LE FICHIER SI 'RND' ET VIDE
      IF(SSEQ .OR. (nfs .GE. 0)) THEN
         OUVS = .TRUE.
         NS   = DN(1)
         IF( VS ) THEN
            IF( SSEQ )  i = sources(1)%rewind()
            call sources(1)%print_summary()
            IF( SSEQ )  i = sources(1)%rewind() 
         ENDIF
      ELSE
         CALL FERMES
      ENDIF
      
      RETURN
      END 





