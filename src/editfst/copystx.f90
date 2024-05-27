!** S/P COPYSTX COPIE UN FICHIER STANDARD EN TOUT OU EN PARTIE.
      SUBROUTINE COPYSTX
      use ISO_C_BINDING
!      use convert_ip123
!      use format_ip123_kind
      use configuration
      use app
      use rmn_fst24
      use rmn_meta
      IMPLICIT NONE 
      include 'rmn/convert_ip123.inc'
      include 'rmn/excdes.inc'
  
!AUTEURS
!         - C. THIBEAULT  FEV 83
!         - Y. BOURASSA   FEV 86
!           "     "       NOV 89 INGORER LISTE OU GANGE DE RECORDS 
!           "     "              CORRIGE BUG QUAND EXPRESS = .TRUE.
!           "     "       OCT 90 ACCEPTE FICHIERS STD89. 
!           "     "       NOV 90 SI MIX DE DESIRE ET EXCLURE, EXCLURE
!                                LES ENREGISTREMENTS DESIRES SEULEMENT.
!           "     "       JUL 91 VERSION ZAPPER
!           "     "       NOV 91 TESTING DES PARAMETRES DE SELECTION DANS 
!                                UN ORDRE DIFFERENT
!           "     "       JAN 92 BUG PREMIERE ALLOCATION MEMOIRE
!Revision 009   M. Lepine - mars 98 - extensions pour fstd98
!Revision 010   M. Lepine - Oct  98 - Ajout du test de nrecmin
!Revision 010   M. Lepine - Dec  01 - Termine dans le cas d'une erreur d'ecriture
!Revision 011   M. Lepine - Mai  02 - code de traitement des IP1 en valeurs reelle
!Revision 012   M. Lepine - Oct  02 - save sur buftemp
!Revision 013   M. Lepine - Nov  05 - remplacement de fstabt par qqexit
!Revision 014   M. Valin  - Fev  14 - mode dryrun, nouveau traitement IP1/2/3
!                         - Mai  14 - remplacement de la logique de selection
!Revision 015   M. Lepine - Sep  16 - elimination des espaces blancs a l'impression
!Revision 016   M. Lepine - Sep  16 - dump_request_table en mode debug seulement
!
!LANGAGE  - FTN77

      EXTERNAL     QQEXIT
      INTEGER      I, J, K, IREC
      integer      nrecord
      character(len=4) :: nomvar
      logical :: can_translate, success
      real :: p1, p2, p3
      integer :: kind1, kind2, kind3, matches
      character (len=2) :: strkind1, strkind2, strkind3
      integer :: date_1, date_2
  
      type(fst_record) :: record
      type(fst_query)  :: query
      type(meta)       :: metaf
      type(C_PTR)      :: obj

      LOGICAL      FIRSTP, BONNE, OK
      interface ! can IP1/2/3 for variable 'name' be translated to value/kind ?
        function fstcantranslate(name) result (yesno) BIND(C,name='FstCanTranslateName')
        use ISO_C_BINDING
        integer(C_INT) :: yesno
        character(C_CHAR), dimension(*), intent(IN) :: name
        end function
      end interface
      integer, save :: NM=0
!      DATA         NM, IST / 0, 0/

      OK     = .NOT.FIXD .AND. .NOT.DM1 
!     OK     = .TRUE. UNE DATE DANS LES DESIRES ET ENREGISTREMENTS
!                     PAS NECESSAIREMENT VALIDES EN MEME TEMP
!     DM1    = .TRUE. PAS DE DATES DANS LES DESIRES
!     FIXD   = .TRUE. LES ENREGISTREMENTS DU FICHIER SOURCE SONT TOUS 
!                     VALIDES EN MEME TEMPS.
!     BONNE  = .TRUE. SI LA DATE DU PREMIER ENREGISTREMENT ACCEPTABLE 
!     DONC IF(FIXD .AND. .NOT.BONNE) INUTILE DE CHERCHER PLUS LOIN

      IF( DEBUG ) call Dump_Request_table()

   10 BONNE  = .FALSE.
      FIRSTP = .TRUE.
      WRITE(app_msg,*) 'copystx: FIRSTP=',FIRSTP,'  OK=',OK,' BONNE=',BONNE
      call app_log(APP_DEBUG,app_msg)

!     OBTENIR LA CLE DU PROCHAIN ENREGISTREMENT QUI NOUS INTERESSE
      
      query= sources(1)%new_query()

      nrecord=0
      do while(query%find_next(record))

         if (record%data_bits.GT.48 .AND. record%data_type.EQ.1) then
            write(app_msg,*) 'copystx: Unable to copy record no ',nrecord,' NBITS=',record%data_bits 
            call app_log(APP_ERROR,app_msg)
            goto 140
         endif

         write(nomvar,'(A4)')record%nomvar
         can_translate = (0 /= fstcantranslate(nomvar))   ! est-ce qu'on peut traduire ip1/2/3 pour cette variable ?

         IF(FIRSTP .OR. OK) THEN 
            WRITE(app_msg,*) 'copystx: ENRG. #1 DATE ORIG = ',record%dateo,' VALID =',record%datev 
            IF(FIRSTP) call app_log(APP_DEBUG,app_msg)
            FIRSTP = .FALSE.
            WRITE(app_msg,*) 'copystx: FIRSTP=',FIRSTP,' OK=',OK,  ' BONNE=',BONNE 
            call app_log(APP_DEBUG,app_msg)
         ENDIF

   !     SI ON DEMANDE TOUT LE FICHIER.
         IF( XPRES ) THEN
            BONNE  = .TRUE.
         ENDIF
   
   !     si on se rend ici, c'est que l'enregistrement est a copier
   
         if (dryrun) then
            i = 0    ! fake status of succesful read if dry run
         else
            success = record%read()
            metaf = record%metadata
         endif
   !
   !     ==================   logique pour directive ZAP  ==================
   !

         IF( ZA ) THEN    ! on "zappe ?"
            IF(Z1 .NE. -1)  record%ip1   = Z1  ! zap ip1
            IF(Z2 .NE. -1)  record%ip2   = Z2  ! zap ip2
            IF(Z3 .NE. -1)  record%ip3   = Z3  ! zap ip3
            IF(ZD .NE. -1)  then ! zap origin date
               record%dateo = ZD  
               if (metaf%is()) then
                  obj = metaf%DefForecastTime(int(ZD, int64))
               endif
            endif
            IF(ZT .NE. '??') THEN   ! zap type
               IF(ZT(1:1) .NE. '?') record%typvar(1:1) = ZT(1:1)
               IF(ZT(2:2) .NE. '?') record%typvar(2:2) = ZT(2:2)

               if (metaf%is()) then
                  obj=metaf%DefFromTypVar(ZT)
               endif
            ENDIF
            IF(ZN .NE. '??') THEN   ! zap name
               IF(ZN(1:1) .NE. '?') record%nomvar(1:1) = ZN(1:1)
               IF(ZN(2:2) .NE. '?') record%nomvar(2:2) = ZN(2:2)
               IF(ZN(3:3) .NE. '?') record%nomvar(3:3) = ZN(3:3)
               IF(ZN(4:4) .NE. '?') record%nomvar(4:4) = ZN(4:4)

               if (metaf%is()) then
                  obj=metaf%DefVarFromDict(ZN)
               endif
            ENDIF
            IF(ZE .NE. '????????????') THEN   ! zap ETIKET
               DO 130 I=1,12
                  IF(ZE(I:I) .NE. '?') record%etiket(I:I) = ZE(I:I)
      130      CONTINUE
               if (metaf%is()) then
                  obj=metaf%DefFromEtiket(ZE)
               endif
            ENDIF
         ENDIF
   
         if(dryrun) then  ! dry run debug mode, tell what we would be copying
   
         i = decode_ip(p1,kind1,p2,kind2,p3,kind3,record%ip1,record%ip2,record%ip3)
         strkind1=kind_to_string(kind1)
         strkind2=kind_to_string(kind2)
         strkind3=kind_to_string(kind3)
         call newdate(record%dateo,date_1,date_2,-3)   ! translate date time stamp to printable format
         if(can_translate)then
            write(app_msg,667)'copystx: DRYRUN-select: ',date_1,date_2,record%typvar,record%nomvar,record%etiket,P1,strkind1,P2,strkind2,P3,strkind3,record%dateo,record%grtyp,record%ig1,record%ig2,record%ig3,record%ig4
         else
            write(app_msg,666)'copystx: DRYRUN-select: ',date_1,date_2,record%typvar,record%nomvar,record%etiket,record%ip1,record%ip2,record%ip3,record%dateo,record%grtyp,record%ig1,record%ig2,record%ig3,record%ig4
         endif
         call app_log(APP_INFO,app_msg)
   666     format(A,2(I8.8,1X),A3,A5,A13,3I14       ,I12,A2,4I10)
   667     format(A,2(I8.8,1X),A3,A5,A13,3(G12.5,A2),I12,A2,4I10)
   
         else   !  real mode, write into the output file
   
            success = destination%write(record,rewrite=ECR)

            if (i .lt. 0) then
               call app_log(APP_ERROR,'copystx: write error')
               call qqexit(55)
            endif
         endif
   
         COPIES = COPIES + 1
         LIMITE = LIMITE - 1
   140   IF(LIMITE .EQ. 0) GO TO 180            ! nombre maximum d'enregistrements a copier atteint

      END DO

      call query % free()

  160 IF(SSEQ) THEN                          ! le fichier source est sequentiel
         leof=sources(1)%eof()
         WRITE(app_msg,*)'copystx: Encountered EOF ',LEOF,' within file ...'
         call app_log(APP_DEBUG,app_msg)
         IF(LEOF.GT.15 .OR. LEOF.LT.1) THEN
            WRITE(app_msg,*) 'copystx: ',LEOF,' is not an acceptable logical EOF'
            call app_log(APP_ERROR,app_msg)
            call qqexit(30)
         ENDIF
         IF(DSEQ .AND. CEOF.NE.0) THEN
            K = CEOF
            IF(CEOF .LT. 0) K = LEOF
            IF(K .LT. 15) THEN
               I = destination%weo(K)
               IF(I.EQ.0) THEN
                  WRITE(app_msg,*) 'copystx: EOF LOGIQUE ',K,' AJOUTEE AU FICHIER',TRIM(ND) 
                  call app_log(APP_DEBUG,app_msg)
               ELSE
                  WRITE(app_msg,*) 'copystx: Unable to write mark of level ',K,' in ',TRIM(ND) 
                  call app_log(APP_ERROR,app_msg)
                  call qqexit(31)
               ENDIF
            ENDIF
         ENDIF
!        DEVONS-NOUS CONTINUER PASSE LE EOF RENCONTRE DANS SOURCE?
         IF(LEOF .LT. MEOF) GO TO 10
      ENDIF
  
!     DOIT-ON ECRIRE UN EOF AVANT DE FERMER?
      IF(DSEQ .AND. EOF.GT.0) THEN           ! le fichier destination est sequentiel
         I = destination%weo(EOF)
         IF(I.EQ.0) THEN
            WRITE(app_msg,*) 'copystx: Mark of level ',K,' written in ',TRIM(ND) 
            call app_log(APP_ERROR,app_msg)
         ELSE
            WRITE(app_msg,*) 'copystx: Unable to write mark of level ',K,' in ',TRIM(ND) 
            call app_log(APP_ERROR,app_msg)
            call qqexit(32)
         ENDIF
      ENDIF
  
  180 IF (COPIES .LT. NRECMIN) THEN
         WRITE(app_msg,*) 'copystx: Minimal number of records not satisfied, NRECMIN=',NRECMIN,' NREC found=',COPIES
         call app_log(APP_ERROR,app_msg)
         CALL QQEXIT(12)
      ELSE
         WRITE(app_msg,*) 'copystx: ',COPIES,' record(s) copied in ',TRIM(ND) 
         IF (COPIES .EQ. 0) THEN
            call app_log(APP_WARNING,app_msg)
         ELSE
            call app_log(APP_INFO,app_msg)
         ENDIF
      ENDIF

      RETURN
  
      END 
