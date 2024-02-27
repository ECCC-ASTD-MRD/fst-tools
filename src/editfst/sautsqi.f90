!** S/R SAUTSQI
!        POSITIONNE LE FICHIER SEQUENTIEL DN 
!        A UN CERTAIN NIVEAU D'EOF LOGIQUE
      SUBROUTINE SAUTSQI(DN, LEV, NL)
      use configuration
      use app
      use rmn_fst24
      IMPLICIT   NONE
        
      INTEGER    DN(*), LEV, NL
  
!ARGUMENTS
!  ENTRE    - DN    - DATASET NAME DU FICHIER DESTINATION A POSITIONNER
!    "      - LEVEL - NIVEAU DE EOF LIGIQUE RECHERCHE [1]
!    "      - NL    - NOMBRE DE DE CES EOF RECHERCHES 1]
!
!AUTEURS
!VERSION ORIGINALE  C. THIBEAULT, C. CHOUINARD ET M.VALIN (STDSAUT)
!REVISION 001       Y. BOURASSA MARS 86
!         002       "      "    NOV 90 VERSION FTN/SQI
!         003       "      "    FEV 92 LE FICHIER DOIT EXISTER
!         004       "      "    MAR 92 CORRIGE BUG DANS TEST FSTINF
!         005       "      "    MAI 92 SKIP ABORT SI EN INTERACTIF
!         006       M. Lepine   Nov 05 Remplacement de fstabt par qqexit
!         007       M. Valin    Mai 14 Remplacement des comdecks par un module
!
!LANGUAGE   - FTN77 
!
!MODULES  
      EXTERNAL      ARGDIMS, OUVRED, qqexit 

      INTEGER       ARGDIMS, OUVRED, LEVEL
      INTEGER       I, K, L, M, success
      CHARACTER(len=128) :: CLE
      CHARACTER(len=15)  :: SORTE, TAPE
      CHARACTER(len=3)   :: DND
      type(fst_file)     :: fstfile

      DND   = 'SEQ'
      SORTE = 'STD+SEQ+OLD'
!     ASSIGNATION DES PARAMETRES PAR DEFAUT ET OUVRE FICHIER
    1 IF(NP .GT. 1) THEN
         LEVEL = LEV
      ELSE
         LEVEL = 1
      ENDIF
      IF(NP .EQ. 3) THEN
         I = NL
      ELSE
         I = 1
      ENDIF
  
      WRITE(CLE, LIN128) (DN(M), M=1,ARGDIMS(1))
      IF(CLE.EQ.ND .AND. OUVD) THEN
         IF(INDEX(DNOM, DND) .EQ. 0) THEN
            WRITE(app_msg,*) 'sautsqi: Problem opening file ',ND,', with type=',SORTE,' already opened with type=',DNOM
            call app_log(APP_ERROR,app_msg)
            IF( INTERAC ) THEN
               RETURN
            ELSE
               CALL qqexit(61)
            ENDIF
         ENDIF
         fstfile = destination
      ELSEIF(CLE.EQ.NS .AND. OUVS) THEN 
         IF(INDEX(SNOM, DND) .EQ. 0) THEN
            WRITE(app_msg,*) 'sautsqi: Problem opening file ',NS,', with type=',SORTE,' already opened with type=',SNOM
            call app_log(APP_ERROR,app_msg)
            IF( INTERAC ) THEN
               RETURN
            ELSE
               CALL qqexit(62)
            ENDIF
         ENDIF
         fstfile = sources(1)
      ELSE
         DNOM = SORTE
         K    = OUVRED( CLE )
         IF(K .EQ. 0) THEN
            fstfile = destination
         ELSE
            call app_log(APP_ERROR,'sautsqi: file does not exist')
            IF( INTERAC ) THEN
               RETURN
            ELSE
               CALL qqexit(63)
            ENDIF
         ENDIF
      ENDIF

      TAPE = CLE
      WRITE(app_msg,600) I, LEVEL, fstfile%get_unit(), TAPE
      call app_log(APP_DEBUG,app_msg)
      600    FORMAT('sautsqi: SAUTE',I3,' EOF 'I2,' FICHIER',I3,'=',A15,'...')

!     SAUTE AU N..IEME EOF DE NIVEAU LEVEL
      success = fstfile%set_search_criteria(ni=k,nj=l,nk=m,datev=0_int64,etiket='0',ip1=0,ip2=0,ip3=0,nomvar='0',typvar='0')

   10 IF (fstfile%find_next()) GOTO 10
      M = fstfile%eof() 
      IF(M.LT.1 .OR. M.GT.15) THEN
         WRITE(app_msg,*) 'sautsqi: Wrong EOF marker=',M,' within tape=',fstfile%get_unit()
         call app_log(APP_ERROR,app_msg)
         CALL qqexit(64)
      ENDIF
      WRITE(app_msg,*) 'sautsqi: Hit an EOF #',M
      call app_log(APP_DEBUG,app_msg)
      IF(M .LT. LEVEL) GO TO 10
      IF(M .EQ. LEVEL) THEN
         I = I-1
         IF(I .NE. 0) GO TO 10
      ENDIF
      IF (fstfile%get_unit() .EQ. sources(1)%get_unit()) LEOF = M
      RETURN
  
!**   SAUTSEQ OUVRE LE FICHIER 2 (SEQUENTIEL 'SEQ+FTN')
      ENTRY SAUTSEQ(DN, LEV, NL)
      DND   = 'FTN'
      SORTE = 'STD+SEQ+FTN+OLD'
      GO TO 1
  
      END 
