!** S/P COPYSTX COPIE UN FICHIER STANDARD EN TOUT OU EN PARTIE.
      SUBROUTINE COPYSTX
      use ISO_C_BINDING
      use convert_ip123
      use format_ip123_kind
      use configuration
      IMPLICIT NONE 
  
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
!Revision 014   M. Valin  - Fev  14 - mode dryrun, noveau traitement IP1/2/3
!
!LANGAGE  - FTN77
!#include "maxprms.cdk"
!#include "logiq.cdk"
!#include "desrs.cdk"
!#include "tapes.cdk"
!#include "key.cdk"
!#include "char.cdk"
!#include "fiches.cdk"
      integer, dimension ( : ), pointer, save :: buftemp => NULL()
!     - buftemp    CHAMP DONT LA LONGUEUR VARIE SELON LES RESSOURCES
!                  NECESSAIRES, LA ROUTINE MEMOIR RESERVE L'ESPACE
!                  (LONGUEUR INITIALE = 1)
      logical, external :: ip_equiv
      EXTERNAL     FSTPRM, FSTSUI, FSTECR, FSTINF, FSTWEO
      EXTERNAL     CRITSUP, DESIRE, FSTLUK, FSTFRM, FSTEOF
      EXTERNAL      JULSEC, QQEXIT, convip, ip1equiv
      INTEGER      FSTINF, FSTEOF, IG1, XTRA1, NI, I, IREC, DATE, SWA
      INTEGER      FSTPRM, FSTFRM, IG2, XTRA2, NJ, J, DEET,       LNG
      INTEGER      FSTECR, FSTSUI, IG3, XTRA3, NK, K, DLFT, DTYP, UBC
      INTEGER      FSTLUK, FSTWEO, IG4, NBITS, NPAS
      integer *8   IP(4), p_int8, kind_8, ior
!      integer *8   IP(4), p_int8, kind_8, ior
!      intrinsic    ior
      integer      IP1,IP2,IP3,kind,p_int
      real         p
      equivalence (p_int,p)
      character *128 string
      character(len=4) :: nomvar
      logical :: can_translate
      real :: p1, p2, p3
      integer :: kind1, kind2, kind3
      character (len=2) :: strkind1, strkind2, strkind3
  
      LOGICAL      FIRSTP, BONNE, OK, EXCL
      interface
        function fstcantranslate(name) result (yesno) BIND(C,name='FstCanTranslateName')
        use ISO_C_BINDING
        integer(C_INT) :: yesno
        character(C_CHAR), dimension(*), intent(IN) :: name
        end function
      end interface
      integer, save :: NM=0, IST=0
!      DATA         NM, IST / 0, 0/

      EXCL   = (NEXC.GT.0) .AND. (NREQ.EQ.NEXC)
      OK     = .NOT.FIXD .AND. .NOT.DM1 
!     EXCL   = .TRUE. TROUVE DES DIRECTIVES EXCLURE SEULEMENT
!     OK     = .TRUE. UNE DATE DANS LES DESIRES ET ENREGISTREMENTS
!                     PAS NECESSAIREMENT VALIDES EN MEME TEMP
!     DM1    = .TRUE. PAS DE DATES DANS LES DESIRES
!     FIXD   = .TRUE. LES ENREGISTREMENTS DU FICHIER SOURCE SONT TOUS 
!                     VALIDES EN MEME TEMPS.
!     BONNE  = .TRUE. SI LA DATE DU PREMIER ENREGISTREMENT ACCEPTABLE 
!     DONC IF(FIXD .AND. .NOT.BONNE) INUTILE DE CHERCHER PLUS LOIN

   10 BONNE  = .FALSE.
      FIRSTP = .TRUE.
!     TROUVE LA CLE DU PROCHAIN ENREGISTREMENT.
      IREC   = FSTINF(SOURCES(1), NI, NJ, NK, -1, ' ', -1, -1, -1,' ', ' ')
      IF(IREC .LT. 0) GO TO 160
      IF( DEBUG ) WRITE(6,*)' FIRSTP=',FIRSTP,'  OK=',OK,' BONNE=',BONNE
   20 I = FSTPRM(IREC, DATE, DEET, NPAS, NI, NJ, NK, NBITS, DTYP,    &
                 IP1, IP2, IP3, TYP, NOM, ETI, GTY, IG1, IG2,        &
                 IG3, IG4, SWA, LNG, DLFT, UBC, XTRA1, XTRA2, XTRA3)
      write(nomvar,'(A4)')NOM
      can_translate = 0/=fstcantranslate(nomvar)
!     on ne convertit plus ip1, la logique de selection va devoir aller plus bas
!     on regarde si NOM permet la conversion des IP1/2/3
!      CALL convip(IP1,p,kind,-1,string,.true.)
!      p_int8 = Z'80000000'
!      if (p .lt. 0) then
!         p = abs(p)
!         p_int8 = p_int8 - p_int
!      else
!         p_int8 = p_int8 + P_int
!      endif
!      kind_8 = kind
!      IP(1)= IOR(p_int8, ishft(kind_8,32))
      IP(1)=IP1
!      write(*,777) ip1,kind,p,ip(1)
 777  format('Debug+ cle(i)=',i10,' kind=',i2,' p =',e11.4,' IP(i) =',z16.16)
      IP(2)=IP2
      IP(3)=IP3
      IF(NBITS.GT.48 .AND. DTYP.EQ.1) THEN
         WRITE(6,*)'IMPOSSIBLE DE COPIER ENREGISTREMENT NO.',IREC,' NBITS =',NBITS 
         GO TO 140
      ENDIF

      IF(FIRSTP .OR. OK) THEN 
!        CALCUL DE La "SECONDE JULIENNE" (AU MOIN DU PREMIER ENREGISTREMENT).
         CALL JULSEC(IP(4), DATE)
         IP(4) = IP(4) +  DEET*(NPAS*1_8) ! possiblement +1800/3600 (en option $$)
         IF(DEBUG .AND. FIRSTP) WRITE(6,*)'DATE ENRG. #1= ',DATE,' JUL. SECONDES=',IP(4)
         FIRSTP = .FALSE.
         IF( DEBUG ) WRITE(6,*)' FIRSTP=',FIRSTP,' OK=',OK,  ' BONNE=',BONNE
      ENDIF

!     SI ON DEMANDE TOUT LE FICHIER.
      IF( XPRES ) THEN
         BONNE  = .TRUE.
         GO TO 120
      ENDIF

!     VERIFIE SI L'ENREGISTREMENT SATISFAIT UN DESIRE OU EXCLURE
      DO 110 K=1,NREQ
!        TEST DU NOMVAR
         IF(REQN(K) .NE. 0) THEN
            DO 30 J=1,REQN(K)
   30          IF(NOM .EQ. NOMS(J,K)) GO TO 40
            GO TO 110
         ENDIF

!        TEST DES IP1-2-3 ET DE LA DATE
   40    DO 61 J=4,1,-1 
!           SI LE PARAMETRE EST UNIVERSEL
            IF(REQ(11,J,K) .EQ. 0) THEN
               IF(J.EQ.4 .AND. (FIXD.OR.DM1)) BONNE = .TRUE.   ! J==4, c'est une date
            ELSEIF(REQ(11,J,K) .GT. 0) THEN 
!              REQUETE CONTIENT UNE LISTE DE PARAMETRES
               DO 50 I=1,REQ(11,J,K)
                  IF (J .LT. 4 .and. can_translate) THEN       ! verification d'equivalence si on peut traduire les IP
                     IF(IP_EQUIV(IP(J),REQ(I,J,K))) GO TO 60   ! pour cette variable et qu'on test IP1(1), IP2(2), ou IP3(3)
                  ELSE
                     IF(IP(J) .EQ. REQ(I,J,K)) GO TO 60
                  ENDIF
!                  IF (J .EQ. 1) THEN
!                     IF(IP1EQUIV(IP(J),REQ(I,J,K))) GO TO 60
!                  ELSE
!                     IF(IP(J) .EQ. REQ(I,J,K)) GO TO 60
!                  ENDIF
   50             CONTINUE
               GO TO 110
            ELSE
!              REQUETE CONTIENT UN INTERVALLE AVEC SAUT
               if (j .eq. 1) then
!                  write(*,888) REQ(1,J,K),REQ(2,J,K),
!     %                 IP(J),IP(J)-REQ(1,J,K),REQ(3,J,K)
!                  write(*,889) MOD((IP(J)-REQ(1,J,K)),REQ(3,J,K))
 888              format('Debug+ min=',z16.16,' max=',z16.16,' IP(1)=',z16.16,   &
                       ' ip1-min=',z16.16,' delta=',z16.16) 
 889              format('Debug+ MOD((IP(J)-REQ(1,J,K)),REQ(3,J,K))=',z16.16)
               endif
               IF(IP(J).GE.REQ(1,J,K) .AND.  IP(J).LE.REQ(2,J,K) .AND.  MOD((IP(J)-REQ(1,J,K)),REQ(3,J,K)).EQ.0) GOTO 60
               GO TO 110
            ENDIF
   60       CONTINUE
   61       CONTINUE

!        TEST DU TYVAR
         IF(REQT(K) .NE. 0)  THEN
            DO 70 J=1,REQT(K)
   70          IF(TYP .EQ. TYPS(J,K)) GO TO 80
            GO TO 110
         ENDIF

!        TEST DE L'ETIKET
   80    IF(REQE(K) .NE. 0) THEN
            DO 90 J=1,REQE(K)
               IF(ETI .EQ. ETIS(J,K)) GO TO 100
   90          CONTINUE
            GO TO 110
         ENDIF

!        SI DES CRITERES SUPLEMENTAIRES S'APPLIQUENT
  100    IF(SUP(8,K).NE.0  .AND.                             &
          ((SUP(1,K).NE.-1 .AND. SUP(1,K).NE.NI)  .OR.       &
           (SUP(2,K).NE.-1 .AND. SUP(2,K).NE.NJ)  .OR.       &
           (SUP(3,K).NE.-1 .AND. SUP(3,K).NE.NK)  .OR.       &
           (GTYS(K).NE.' ' .AND. GTYS(K).NE.GTY)  .OR.       &
           (SUP(4,K).NE.-1 .AND. SUP(4,K).NE.IG1) .OR.       &
           (SUP(5,K).NE.-1 .AND. SUP(5,K).NE.IG2) .OR.       &
           (SUP(6,K).NE.-1 .AND. SUP(6,K).NE.IG3) .OR.       &
           (SUP(7,K).NE.-1 .AND. SUP(7,K).NE.IG4))) GO TO 110

!        TROUVE UN MATCH POUR LA REQUETE(K).
         SATISF(K) = SATISF(K) + 1

!        SI LA DIRECTIVE SATISFAITE EST UN DESIRE
         IF( DESEXC(K) .EQ. -1 ) GO TO 120

!        SI LA DIRECTIVE SATISFAITE EST UN EXCLURE            
         IF( DIAG ) WRITE(6,*)'CLE',IREC, 'TYPRVAR=',TYP, ' NOMVAR=',NOM,' ETIKET=',ETI,  &
                              ' IP1,2,3,DATE=',IP1,ip2,ip3,date,' EXCLU'
         GO TO 140
            
  110    CONTINUE

!     AUCUNE DES REQUETES SATISFAITE
      IF( .NOT. EXCL ) GO TO 140
  
!     CONTROLE DE LA MEMOIRE TEMPON AVANT LECTURE
  120 IF(LNG .GT. NM) THEN  ! make sure buffer is large enough to receive data
         IF (associated(buftemp)) deallocate(buftemp) 
         NM = LNG
         allocate(BUFtemp(NM))
         ist = 1
      ENDIF
      if(dryrun)then
        I = 0
      else
        I = FSTLUK(BUFtemp, IREC, NI, NJ, NK)
      endif
      IP(1) = IP1      ! remettre la valeur de ip1 provenant du fstprm
      IF( ZA ) THEN    ! we are "zapping"
         IF(Z1 .NE. -1)  IP(1) = Z1  ! zap ip1
         IF(Z2 .NE. -1)  IP(2) = Z2  ! zap ip2
         IF(Z3 .NE. -1)  IP(3) = Z3  ! zap ip3
         IF(ZD .NE. -1)  DATE  = ZD  ! zap date
         IF(ZT .NE. '??') THEN   ! zap type
            IF(ZT(1:1) .NE. '?') TYP(1:1) = ZT(1:1)
            IF(ZT(2:2) .NE. '?') TYP(2:2) = ZT(2:2)
         ENDIF
         IF(ZN .NE. '??') THEN   ! zap name
            IF(ZN(1:1) .NE. '?') NOM(1:1) = ZN(1:1)
            IF(ZN(2:2) .NE. '?') NOM(2:2) = ZN(2:2)
            IF(ZN(3:3) .NE. '?') NOM(3:3) = ZN(3:3)
            IF(ZN(4:4) .NE. '?') NOM(4:4) = ZN(4:4)
         ENDIF
         IF(ZE .NE. '????????????') THEN   ! zap ETIKET
            DO 130 I=1,12
               IF(ZE(I:I) .NE. '?') ETI(I:I) = ZE(I:I)
  130          CONTINUE
         ENDIF
      ENDIF
      ip1 = IP(1)
      ip2 = IP(2)
      ip3 = IP(3)
      if(dryrun) then  ! dry run debug mode, say what we would be copying
        i = decode_ip(p1,kind1,p2,kind2,p3,kind3,ip1,ip2,ip3)
        strkind1=kind_to_string(kind1)
        strkind2=kind_to_string(kind2)
        strkind3=kind_to_string(kind3)
        if(can_translate)then
          write(6,667)'DRYRUN-select: ',TYP,NOM,ETI,P1,strkind1,P2,strkind2,P3,strkind3,DATE,GTY,IG1,IG2,IG3,IG4
        else
          write(6,666)'DRYRUN-select: ',TYP,NOM,ETI,IP1,IP2,IP3,DATE,GTY,IG1,IG2,IG3,IG4
        endif
666     format(A,A3,A5,A13,3I14,I12,A2,4I10)
667     format(A,A3,A5,A13,3(G12.5,A2),I12,A2,4I10)
        i = 0  ! cannot fail
      else   !  real mode, we write into the output file
        I = FSTECR(BUFtemp, BUFtemp, -NBITS, 3, DATE, DEET, NPAS, NI, NJ, NK,  &
                   IP1, IP2, IP3, TYP, NOM, ETI, GTY, IG1, IG2, IG3, IG4, DTYP, ECR)
      endif
      if (i .lt. 0) then
         write(6,*) 'EDITFST: write error, ABORTING'
         call qqexit(55)
      endif
      COPIES = COPIES + 1
      LIMITE = LIMITE - 1
      IF(LIMITE .EQ. 0) GO TO 180
  140 IF(.NOT.BONNE .AND. FIXD) THEN
         IF( DIAG .OR. DEBUG) WRITE(6,*)'*** DATE DE VALIDATION DU PREMIER ENREGITREMENT INACCEPTABLE ***'
!        PREMIERE DATE MAUVAISE ET LES AUTRES SUPPAUSEES PAREILLES.
!        SI LE SOURCE EST RANDOM LA COPIE EST TERMINEE.
!         "  "    "    "  SEQUENTIEL SKIP AU PROCHAIN EOF.
  150    IF(SSEQ .AND. FSTSUI(SOURCES(1), NI, NJ, NK) .NE. 0) GO TO 150
      ELSE
!        PREMIERE DATE BONNE OU DATES SONT PAS TOUTES PAREILLES
         IREC = FSTSUI(SOURCES(1), NI, NJ, NK)
         IF(IREC .GE. 0) GO TO 20
      ENDIF
  160 IF(SSEQ) THEN 
         LEOF = FSTEOF(SOURCES(1))
         print*,'apres fsteof leof=',leof
         IF(DIAG .OR. DEBUG) WRITE(6,*)'RENCONTRE UN EOF',LEOF, ' DANS ', SOURCES(1),'...'
         IF(LEOF.GT.15 .OR. LEOF.LT.1) THEN
            WRITE(6,*) LEOF," N'EST PAS ACCEPTABLE COMME EOF LOGIQUE"
            call qqexit(30)
         ENDIF
         IF(DSEQ .AND. CEOF.NE.0) THEN
            K = CEOF
            IF(CEOF .LT. 0) K = LEOF
            IF(K .LT. 15) THEN
               I = FSTWEO(3, K)
               IF(I.EQ.0 .AND. (DIAG .OR. DEBUG)) THEN
                  WRITE(6,*)'EOF LOGIQUE ',K,' AJOUTEE AU FICHIER',ND
               ELSEIF(I .NE. 0) THEN
                  WRITE(6,*)'IMPOSSIBLE D''ECRIRE UNE MARQUE DE ', 'NIVEAU ',K,' DANS ', ND
                  call qqexit(31)
               ENDIF
            ENDIF
         ENDIF
!        DEVONS-NOUS CONTINUER PASSE LE EOF RENCONTRE DANS SOURCE?
         IF(LEOF .LT. MEOF) GO TO 10
      ENDIF
  
!     DOIT-ON ECRIRE UN EOF AVANT DE FERMER?
      IF(DSEQ .AND. EOF.GT.0) THEN
         I = FSTWEO(3, EOF)
         IF(I.EQ.0 .AND. (DIAG .OR. DEBUG)) THEN
            WRITE(6,*)' MARQUE DE NIVEAU',K,' ECRITE DANS ', ND
         ELSEIF(I .NE. 0) THEN
            WRITE(6,*)' IMPOSSIBLE D''ECRIRE UNE MARQUE DE NIVEAU', K,' DANS ', ND
            call qqexit(32)
         ENDIF
      ENDIF
  
      IF(.NOT.XPRES .AND. (DIAG .OR. DEBUG)) THEN 
         DO 170 I=1,NREQ
!           PRENDRE NOTE DES REQUETES NON SATISFAITES
            IF(SATISF(I) .EQ. 0) WRITE(6,*)'*** ATTENTION REQUETE',I,' INSATISFAITE ***'
  170       CONTINUE
      ENDIF
  180 WRITE(6,*) COPIES,' ENREGISTREMENT(S) COPIES DANS ', ND

      IF (COPIES .LT. NRECMIN) THEN
         WRITE(6,*) ' NOMBRE MINIMAL D ENREGISTREMENT INSATISFAIT'
         WRITE(6,*) ' NRECMIN=',NRECMIN,' NOMBRE TROUVE = ',COPIES
         CALL QQEXIT(12)
      ENDIF

      RETURN
  
      END 
