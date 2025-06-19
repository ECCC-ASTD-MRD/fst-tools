!   PROGRAM FSTCOMP - COMPARAISON DE 2 FICHIERS STANDARDS

      PROGRAM  FSTCOMP
      use app
      use rmn_fst24
      use rmn_libc, only: c_exit

      IMPLICIT NONE

#include "fst-tools_build_info.h"


!AUTEURS
!     V1.0 ORIGINALE (COMPSTD)   M. LEPINE   (MARS 87)
!REVISIONS
! 001 V1.? AJOUT DU MARQUAGE EN CAS D'ERREUR PLUS GRANDE QUE LA LIMITE
!          SPECIFIEE
! 002 V4.0 VERSION UNIX/FST). Y BOURASSA (JAN 91)
! 003 V4.1 POSSIBILITE D'IGNORER LES TYPVAR,NOMVAT,ETIKET,IP1,IP2 ET IP3
!          LORS DE LA RECHERCHE DANS B. Y BOURASSA (JAN 91)
! 004 V4.2 MODIFICATIONS AUX SORTIES. Y BOURASSA (FEV 91)
! 005 V4.3 ACCEPTE LES FICHIERS BINAIRES. Y BOURASSA (AVR 91)
! 006 V4.4 APPEL A CCARD (AUG 91)
! 007 V5.0 APPEL A LOW2UP AORES CCARD, BUG DANS 2IE PARAMS
! 008 V5.1 (JUIL 92)
! 009 V6.0 (M. Lepine, Jan. 93) tenir compte du format IEEE
! 010 V6.1 (V. Lee, Dec. 1997) infinite loop when NI/NJ/NK does not match.
! 011 V6.2 (M. Lepine, avr. 1998) reload avec librmnbeta
! 012 V6.3 (M. Lepine, avr. 1998) reload avec fstd98
! 013 V6.4 (M. Lepine, avr. 1998) reload avec fstd98 et librmn32stack_LR.a
! 014 V6.5 (M. Valin,  avr. 1999) certains calculs faits en real!8
! 015 V6.6 (M. Lepine, Mai  2000) appel a incdatr au lieu de incdat
! 016 V6.7 (M. Lepine, Oct  2000) reload pour datev (fstd89)
! 017 V6.8 (M. Lepine, Jan  2002) mods pour tenir compte des extensions fstd2000
! 018 V6.9 (M. Lepine, Fev  2003) appel a convip pour afficher les niveaux
! 019 V7.0 (M. Lepine, Jan  2004) option reduction32 pour type E64, correction de format
! 019 V7.1 (M. Lepine, Oct  2004) Ajout datatype compresse (>128) et datatype 6
! 020 V7.2 (M. Lepine, Fev  2005) Reload avec librmn_x
! 021 V7.3 (M. Lepine, Mars 2005) Ajout de la fonctionnalite des fichiers remote
! 022 V7.4 (M. Lepine, Fev  2006) Appel a ip1_all
! 023 V7.5 (M. Lepine, Mai  2006) Reload, bug fix float_packer
! 024 V7.6 (Y. Chartier, Oct. 2006) Reload pour compresseur point flottant
! 025 V7.7 (M. Lepine, Fev. 2007) Comparaison avec l'erreur du a l'algorithme de compaction
! 026 V7.8 (M. Lepine, Juin 2007) Reload avec librmn_009
! 027 V7.9 (M. Lepine, Avr. 2010) Correction pour affichage de facteur de correlation negatif
! 028 V8.0 (M. Lepine, Juin 2011) Variables d'exception au convip du IP1
! 026 V8.1 (M. Lepine, Mai  2012) Reload avec librmn_013
! 027 V8.2 (M. Lepine, Nov  2012) En cas d'erreur, retourner un code d'erreur avec qqexit
! 028 V8.3 (M. Lepine, Mars 2014) Utilisation du fichier $ARMNLIB/data/exception_vars
! 029 V8.4 (M. Lepine, Juil 2014) Remettre ARMNLIB a la place de ARMNLIB_DATA
! 030 V8.5 (M. Lepine, Dec  2014) Reload avec librmn_015.1
! 031 V8.6 (M. Lepine, Fev  2015) Reload avec librmn_015.2
! 031 V8.7a (M. Lepine, Mars  2016) Reload avec librmn_Alpha_016
! 032 V8.7a (M. Lepine, Avril  2016) Ajout de l'option pour ignorer la verification de grille
! 033 V8.8 (M. Lepine, Dec  2016) Ajout de statistiques additionnelles
! 034 V8.9 (M. Lepine, Jan  2017) Eviter le traitement des enregistrements '!!' qui contiennent
!                                 un melange d'entiers, reels et caracteres

!OBJET(FSTCOMP)
!     ETABLIT DES STATISTIQUES DE COMPARAISON ENTRE DEUX FICHIERS
!     STANDARDS SEQUENTIEL OU ACCES DIRECT, PRODUIT UN RAPPORT.

    EXTERNAL CCARD, fstopc, fstopl, LOW2UP, convip_plus, fnom, fclos, ip1_all, qqexit

    integer RCMP1D,ICMP1D

    integer, parameter :: nkbkeys = 24

    CHARACTER(len=2)    :: TYPVAB
    CHARACTER(len=4)    :: NOMVAB
    CHARACTER(len=8)    :: CLE(nkbkeys)
    CHARACTER(len=12)   :: ETIKB, NOMA, NOMB, NOMC
    CHARACTER(len=40)   :: NA, NB
    CHARACTER(len=1024) :: DEF1(nkbkeys), DEF2(nkbkeys), NOMD

    LOGICAL TD, TE, TT, AS, AF, BS, BF, VA, VB, DI, LN, &
            P1, P2, P3, TN ,T, TG, EXCEPTION, ECODE
    INTEGER KA, KB, N1, N2, LIMITE, LIMVAL, L, N, I,   &
            IDATE, IP1B, IP2B, IP3B,            &
            FSTOPC, fstopl, fnom, fclos,        &
            TABLO(0:6,0:6)
    integer ier, kind, ip1_all, PACK_ERR, PACK_ERR2, ind
    integer lvar, iunexpv
    real Level
    character(len=30)  :: string
    character(len=128) :: exception_vars
    character(len=512) :: ARMNLIB_var

    type(fst_file)   :: filea, fileb
    type(fst_record) :: recorda, recordb
    type(fst_query)  :: querya, queryb
    logical          :: success

    DATA exception_vars /'^^  >>  !!   ^>  META'/

    DATA CLE  /'A:', 'B:', 'L.',    'AS',  'BS ',  'AF', 'BF',  'LI', &
                'ND',  'NE',  'D',      'N',   'VA',  'VB',  'NT',    &
                'N1',  'N2',  'N3',  'NN',  'X', 'PACKERR', 'NG', 'ECODE', 'LD' /

    DATA DEF1 /'A', 'B', '$OUT', 'NON', 'NON', 'NON', 'NON', '-7',  &
                'NON', 'NON', 'WARNIN', 'NON', 'NON', 'NON', 'NON',   &
                'NON', 'NON', 'NON', 'NON', 'X', '0', 'NON', 'NON', '-32'/

    DATA DEF2 /'A', 'B', '$OUT', 'SQI', 'SQI', 'FTN', 'FTN', '-7',  &
                'OUI', 'OUI', 'INFORM', 'OUI', 'VA',  'VB',  'OUI',   &
                'OUI', 'OUI', 'OUI', 'OUI', 'X', '1', 'OUI', 'OUI', '-32'/

    DATA       N, NOMVAB, TYPVAB, ETIKB, IDATE, IP1B, IP2B, IP3B     &
            / 0, ' ',    ' ',    ' ',   4*-1/

    !     VALIDE QUAND LA CLE 'X'.NE.'R'
    DATA TABLO/ 2, 1, 2, 3, 2, 1, 1,  &
                1, 1, 3, 3, 3, 1, 1,  &
                2, 3, 2, 3, 2, 3, 3,  &
                3, 3, 3, 3, 3, 3, 3,  &
                2, 3, 2, 3, 2, 3, 3,  &
                1, 1, 3, 3, 3, 1, 1,  &
                1, 1, 3, 3, 3, 1, 1 /
    !     0=BINAIRE 1=REEL 2=ENTIER 3=CARACTERE 4=ENTIER SANS SIGNE 5=IEEE
    !     NOTE:QUAND LA CLE('X'.EQ.'R') DATA TABLO(0,0)=1

    !     EXTRACTION DES CLES DE LA SEQUENCE D'APPEL
    I = -1
    CALL CCARD(CLE, DEF2, DEF1, nkbkeys, I)
    DO I = 4,11
        CALL LOW2UP(DEF1(I), DEF1(I))
    END DO
    READ(DEF1(8), '(I8)') LIMITE
    READ(DEF1(21),'(I8)') PACK_ERR
    READ(DEF1(24), '(I8)') LIMVAL

    VA = DEF1(13) .NE. 'NON'
    IF(VA .AND. (DEF1(13).NE.'VA')) DEF1(1) = DEF1(13)
    VB = DEF1(14) .NE. 'NON'
    IF(VB .AND. (DEF1(14).NE.'VB')) DEF1(2) = DEF1(14)

    AF = INDEX(DEF1(6),'FTN') .NE. 0
    IF( AF ) THEN
        NOMA = 'STD+SEQ+FTN'
        AS   = .FALSE.
    ELSE
        AS = INDEX(DEF1(4),'SQI') .NE. 0
        IF( AS ) THEN
            NOMA = 'STD+SEQ'
        ELSE
            NOMA = 'STD+RND'
        ENDIF
    ENDIF

    BF = INDEX(DEF1(7),'FTN') .NE. 0
    IF( BF ) THEN
        NOMB = 'STD+SEQ+FTN'
        BS   = .FALSE.
    ELSE
        BS = INDEX(DEF1(5),'SQI') .NE. 0
        IF( BS ) THEN
            NOMB = 'STD+SEQ'
        ELSE
            NOMB = 'STD+RND'
        ENDIF
    ENDIF
    I  = FNOM(6, DEF1(3), 'SEQ+FTN', 0)
    TD = DEF1( 9) .EQ. 'NON'
    TE = DEF1(10) .EQ. 'NON'
    TT = DEF1(15) .EQ. 'NON'
    P1 = DEF1(16) .EQ. 'NON'
    P2 = DEF1(17) .EQ. 'NON'
    P3 = DEF1(18) .EQ. 'NON'
    TN = DEF1(19) .EQ. 'NON'
    TG = DEF1(22) .EQ. 'NON'
    DI = DEF1(11) .EQ. 'INFORM'
    LN = DEF1(12) .EQ. 'OUI'
    ECODE = DEF1(23) .EQ. 'OUI'
    IF(DEF1(20) .EQ. 'R') TABLO(0,0) = 1

    app_ptr = app_init(0,'fstcomp',FSTCOMP_VERSION,'',BUILD_TIMESTAMP)
    call app_logstream(DEF1(3))

    IF( LN ) THEN
        call app_log(APP_VERBATIM,'* * * '//FSTCOMP_VERSION//' * * *')
    ELSE
        call app_start()
    ENDIF

    app_status = 0

    ier = app_loglevel(DEF1(11))
    ier = fstopl('REDUCTION32',.true.,.false.)

    CALL GETENV('ARMNLIB',ARMNLIB_var)
    lvar = len_trim(ARMNLIB_var)
    IF (lvar .gt. 0) THEN
        iunexpv = 0
        ier = fnom(iunexpv,ARMNLIB_var(1:lvar)//'/data/exception_vars_ok','SEQ+FTN+FMT+OLD+R/O',0)
        IF (ier .lt. 0) THEN
            WRITE(app_msg,*) '$ARMNLIB_DATA/exception_vars file not found; using internal exception list'
            call app_log(APP_INFO,app_msg)
        ELSE
            READ(iunexpv,'(a)') exception_vars
            ier = fclos(iunexpv)
        ENDIF
    ENDIF

    WRITE(app_msg,*) 'exception_vars=',exception_vars
    call app_log(APP_DEBUG,app_msg)

    !     SI A=RND & B=SEQ CHANGE [A POUR B] & [B POUR A]
    IF((BF.OR.BS) .AND. .NOT.(AF.OR.AS)) THEN
        NOMD    = DEF1(1)
        DEF1(1) = DEF1(2)
        DEF1(2) = NOMD
        NOMC    = NOMA
        NOMA    = NOMB
        NOMB    = NOMC
        T  = BS
        BS = AS
        AS = T
        T  = AF
        AF = BF
        BF = T
    ENDIF

    IF((DEF1(1).EQ.DEF1(2))) then
        WRITE(app_msg,*) 'Comparing file "A" with itself'
        call app_log(APP_WARNING,app_msg)
    endif

    NA = DEF1(1)
    success = filea%open(DEF1(1),NOMA//'+OLD+R/O+REMOTE')
    if (.not. success) then
        write(app_msg,*) 'Invalid input file'
        call app_log(APP_ERROR,app_msg)
        app_status=app_end(-1)
        call qqexit(-1)
    endif
    N1 = filea % get_num_records()

    NB = DEF1(2)
    success = fileb%open(DEF1(2),NOMB//'+OLD+R/O+REMOTE')
    if (.not. success) then
        write(app_msg,*) 'Invalid output file'
        call app_log(APP_ERROR,app_msg)
        app_status=app_end(-1)
        call qqexit(-1)
    endif
    N2 = fileb % get_num_records()

    IF(N1 .Le. 0) THEN
        WRITE(app_msg,*) 'Sequential file ', NA
        call app_log(APP_INFO,app_msg)
        IF( .NOT.(AS.OR.AF) ) THEN
            WRITE(app_msg,*) 'File declared random ', NA
            call app_log(APP_INFO,app_msg)
            GOTO 60
        ENDIF
    ELSE
        WRITE(app_msg,*) ' ',N1,' records in ', NA
        call app_log(APP_INFO,app_msg)
        IF(N1 .EQ. 0) GOTO 90
    ENDIF

    IF(N2 .Le. 0) THEN
        WRITE(app_msg,*) 'Sequential file ', NB
        call app_log(APP_INFO,app_msg)
        IF( .NOT.BS ) THEN
            WRITE(app_msg,*) 'File declared random  ', NB
            call app_log(APP_INFO,app_msg)
            GOTO 80
        ENDIF
    ELSE
        WRITE(app_msg,*) ' ',N2,' records in ', NB
        call app_log(APP_INFO,app_msg)
        IF(N2 .EQ. 0) GOTO 80
    ENDIF

    !     ECRIRE L'ENTETE DE PAGE
    IF (PACK_ERR .eq. 0) THEN
        WRITE(6,600)
    ELSE
        WRITE(6,700)
    ENDIF

    IF(AS .OR. AF) l = filea%rewind()

    querya = filea%new_query()
    do while(querya%find_next(recorda))

        if (recorda%NOMVAR == '!!') then
            WRITE(app_msg,*) 'Skipping record "!!", can''t compare'
            call app_log(APP_INFO,app_msg)
            cycle
        endif

        ! SI LA DATE EST A CONSIDERER
        IF( TD ) then
            if (recorda%dateo .eq. 0) then
                idate = 0
            else
                idate = recorda%datev
            endif
        endif

        ! SI ETIKET EST A CONSIDERER
        IF( TE ) etikb = recorda%etiket

        ! SI TYPVAR EST A CONSIDERER
        IF( TT ) typvab = recorda%typvar

        ! SI NOMVAR EST A CONSIDERER
        IF( TN ) nomvab = recorda%nomvar

        ! SI IP1 EST A CONSIDERER
        IF( P1 ) then
            IF (INDEX(exception_vars,recorda%nomvar) .ne. 0) then
                ip1b = recorda%ip1
                EXCEPTION = .TRUE.
            ELSE
                call convip_plus(recorda%ip1,level,kind,-1,string,.false.)
                !TODO: fix               ip1b = IP1_ALL(level,kind)
                ip1b = recorda%ip1
                EXCEPTION = .FALSE.
            ENDIF
        ENDIF

        ! SI IP2 EST A CONSIDERER
        IF( P2 ) ip2b = recorda%ip2

        ! SI IP3 EST A CONSIDERER
        IF( P3 ) ip3b = recorda%ip3

        if (BS .OR. BF) l = fileb%rewind()
        queryb = fileb%new_query(datev=idate, etiket=etikb, ip1=ip1b, ip2=ip2b, ip3=ip3b, typvar=typvab, nomvar=nomvab)
        success = queryb%find_next(recordb)

        IF(.not. success) THEN
            write(app_msg,601) nomvab, typvab, ip1b, ip2b, ip3b, idate, NB
            call app_log(APP_WARNING,app_msg)
            app_status = 2
            cycle
        ENDIF

        ! VERIFICATION DES DIMENSIONS DE LA GRILLE SI PRESENT
        IF((recordb%ni.NE.recorda%ni) .OR. (recordb%nj.NE.recorda%nj) .OR. (recordb%nk.NE.recorda%nk)) THEN
            write(app_msg,603) NOMVAB,recorda%ni,recorda%nj,recorda%nk,recordb%ni,recordb%nj,recordb%nk
            call app_log(APP_WARNING,app_msg)
            app_status = 1
            cycle
        ENDIF

        ! VERIFICATION DES PARAMETRES DE LA GRILLE
        IF (TG) THEN
            IF((recorda%grtyp .NE. recordb%grtyp) .OR. (recorda%ig1 .NE.recordb%ig1) .OR. (recorda%ig2.NE.recordb%ig2) .OR. &
                (recorda%ig3.NE.recordb%ig3) .OR. (recorda%ig4.NE.recordb%ig4)) THEN
                write(app_msg,602) NA, recorda%grtyp, recorda%ig1, recorda%ig2, recorda%ig3, recorda%ig4, &
                            NB, recordb%grtyp, recordb%ig1, recordb%ig2, recordb%ig3, recordb%ig4
                call app_log(APP_WARNING,app_msg)
                app_status=1
                cycle
            ENDIF
        ENDIF

        IF(recorda%pack_bits .NE. recordb%pack_bits) then
            write(app_msg, *) 'NBITSA=',recorda%pack_bits,' NBITSB=',recordb%pack_bits
            app_status = 1
            call app_log(APP_INFO, app_msg)
        endif

        WRITE(app_msg, *) 'COMPARE DATA_TYPE_A=',recorda%data_type,'  @  DATA_TYPE_B=',recordb%data_type
        call app_log(APP_INFO, app_msg)

        ! TOUT EST OK LIT ET COMPARE
        success = recorda%read()
        success = recordb%read()
        recordb%etiket = etikb

        IF ((mod(recorda%data_type,128) .ne. 1) .and. (mod(recorda%data_type,128) .ne. 6)) THEN
            PACK_ERR2 = 0
        ELSE
            PACK_ERR2 = PACK_ERR
        ENDIF
        IF ((mod(recorda%data_type,128) .gt. 6).or.(mod(recordb%data_type,128) .gt. 6)) goto 30
        GO TO (40, 50, 30) TABLO(mod(recorda%data_type, 128), mod(recordb%data_type, 128))

    30  WRITE(app_msg,*)' No comparison possible: DATA_TYPE_A=',recorda%data_type,' DATA_TYPE_B=',recordb%data_type
        app_status=1
        call app_log(APP_WARNING,app_msg)
        GO TO 60

    40  app_status = RCMP1D(recorda, recordb, LIMITE, LIMVAL, PACK_ERR2, EXCEPTION)
        GO TO 60

    50  app_status = ICMP1D(recorda, recordb, EXCEPTION)
        call queryb % free()
    60  continue
    enddo

    IF(VA .AND. .NOT.AF .OR. VB .AND. .NOT.BF) THEN
        IF(.NOT.DI) L = FSTOPC('MSGLVL', 'INFORM', .FALSE.)
        IF( VA ) THEN
            IF( AS ) L = filea%rewind()
            WRITE(6,*)' DN=', NA
            call filea%print_summary()
        ENDIF
        IF( VB ) THEN
            IF( BS ) L = fileb%rewind()
            WRITE(6,*)' DN=', NB
            call fileb%print_summary()
        ENDIF
    ENDIF

   80 success = fileb%close()
   90 success = filea%close()

    IF( LN ) THEN
        call app_log(APP_VERBATIM,'* * *  fstcomp end  * * *')
    ELSE
        app_status = app_end(app_status)
    ENDIF

  600 FORMAT('  NOM    ETIKET           IP1',  &
             '            IP2       IP3   E-REL-MAX', &
             '   E-REL-MOY   VAR-A        C-COR        MOY-A', &
             '        BIAIS       E-MAX       E-MOY')


  601 FORMAT(' Not found ',A4,' ',A2,' IP123=', 3I10, I10,' in ',A40)
  602 FORMAT(' ',A40,' GRTYP IG1@4=', A1,1X, 4I6,/ &
            ' ',A40,' GRTYP IG1@4=', A1,1X, 4I6)
  603 FORMAT(' ',A4,' -Dimensions found',3I5,' looking for',3I5)

  700 FORMAT('  NOM    ETIKET           IP1', &
             '            IP2       IP3  E-REL-MAX', &
             '  E-REL-MOY    VAR-A      C-COR        MOY-A', &
             '        BIAIS      E-MAX      E-MOY     TOLERANCE')

    if (.NOT. ECODE) then
        if (app_status == 2) then
            app_status = 0
        end if
    end if
    call c_exit(app_status)
END


!   S/P RCMP1D  COMPARAISON DE DEUX CHAMPS REELS DE UNE DIMENSION

!AUTEURS  VERSION ORIGINALE (REALCMP)  M.VALIN DRPN 1987
!         VERSION (RCMP1D)  Y.BOURASSA DRPN JAN 1990
!         Ajout de l'argument exception - M. Lepine Mars 2014

!ARGUMENTS
! ENTRE  A,B     CHAMPS REELS A COMPARER
!   "    LIMITE  ERREUR MAXIMUM TOLOREE
!   "    PACK_ERR NOMBRE D'UNITE D'ERREUR DU A L'ALGORITHME DE PACKING
!                 A UTILISER POUR DETERMINER SI "A" COMPARE A "B"
      integer function RCMP1D(A, B, LIMITE, LIMVAL, PACK_ERR, EXCEPTION)
      use app
      use rmn_fst24

      IMPLICIT NONE

      INTEGER  N,NR, LIMITE, LIMVAL
      INTEGER  PACK_ERR
      REAL     MAXABS, SUMABS, ERRABS

      type(fst_record) :: a, b
      logical :: exception
      real(kind = real32), dimension(:), pointer :: dataa, datab

      INTEGER   I, kind, irange
      CHARACTER(len=15) :: Level
      REAL      rlevel
      REAL(kind=real64) :: SA, SB, SA2, SB2, ERR, DERR, ERRMAX, ABAR, BBAR, &
                             AA, BB, FN, ERRLIM, ERRVAL, VARA, VARB, SAB
      REAL MIN_A, MAX_A, MIN_B, MAX_B, RANGE_A, RANGE_B, DEUX_EXP_NB
      REAL ratio_max, ratio
      REAL ERR_UNIT
      integer nbdiff
      EXTERNAL statfldx

      RCMP1D=0
      call a % get_data_array(dataa)
      call b % get_data_array(datab)

      nbdiff = 0
      NR=0
      n=a%ni*a%nj*a%nk

      ERRLIM = 10.**LIMITE
      ERRVAL = 10.**LIMVAL
      DEUX_EXP_NB = 2.0 ** MIN(a%pack_bits,b%pack_bits)
      SA     = 0.
      SB     = 0.
      SAB    = 0.
      SA2    = 0.
      SB2    = 0.
      ERRMAX = 0.
      ERR    = 0.
      SUMABS = 0.
      MAXABS = 0.
      ratio_max = 0.
      MIN_A = dataa(1)
      MAX_A = dataa(1)
      MIN_B = datab(1)
      MAX_B = datab(1)
      DO 10 I=1,N
        AA     = dataa(I)
        BB     = datab(I)
        MIN_A = MIN(MIN_A,dataa(I))
        MAX_A = MAX(MAX_A,dataa(I))
        MIN_B = MIN(MIN_B,datab(I))
        MAX_B = MAX(MAX_B,datab(I))
        IF(ABS(AA-BB) .LT. ERRVAL) THEN
           AA=BB
        ENDIF
        SA     = SA+AA
        SB     = SB+BB
        IF(AA .NE. BB) THEN
            if (aa .ne. 0.) ratio = (max(aa,bb) - min(aa,bb)) / aa * 100
            if (ratio > ratio_max) ratio_max = ratio
!            write(6,888) 'Debug difference A vs B au point I=',i,'AA, AA-BB=',aa,aa-bb
            nbdiff = nbdiff +1
888        format(a,i8,2x,a,e14.7,2x,e14.7)
            ERRABS = ABS(AA-BB)
            SUMABS = SUMABS+ERRABS
            MAXABS = MAX(ERRABS,MAXABS)
            derr=0.0
            IF(AA .NE. 0.) THEN
                DERR = ABS(1.-BB/AA)
            ELSEIF(BB .NE. 0.)THEN
                DERR = ABS(1.-AA/BB)
            ENDIF
            ERRMAX = MAX(ERRMAX,DERR)
            ERR    = ERR+DERR
        ENDIF
   10 CONTINUE

      WRITE(app_msg,*) 'nbdiff = ',nbdiff,' sur un total de ',NR
      call app_log(APP_INFO,app_msg)

      RANGE_A = MAX_A - MIN_A
      RANGE_B = MAX_B - MIN_B
      irange = TRANSFER(RANGE_A,1)
      irange = ISHFT(ISHFT(irange,-23) +1,23)
      RANGE_A = TRANSFER(irange,1.0)
      FN   = FLOAT(N)

      ERR  = ERR/FN
      ABAR = SA/FN
      BBAR = SB/FN
      DO 20 I=1,N
            AA  = dataa(I)-ABAR
            BB  = datab(I)-BBAR
            IF(ABS(dataa(I)-datab(I)) .LT. ERRVAL) THEN
               BB=AA
            ENDIF
            SAB = SAB+AA*BB
            SA2 = SA2+AA*AA
            SB2 = SB2+BB*BB
!         ENDIF
   20 continue
      SUMABS = SUMABS/FN
      VARA   = SA2/FN
      VARB   = SB2/FN
      WRITE(app_msg,*) 'sab avant=',sab
      call app_log(APP_DEBUG,app_msg)
      IF(SA2*SB2 .NE. 0.) THEN
         SAB    = SAB/SQRT(SA2*SB2)

      WRITE(app_msg,*) 'sa2,sb2,sqtr,sab=',sa2,sb2,SQRT(SA2*SB2),sab
      call app_log(APP_DEBUG,app_msg)

      ELSEIF(SA2.EQ.0. .AND. SB2.EQ.0.) THEN
         SAB = 1.0
      ELSEIF(SA2 .EQ. 0.) THEN
         SAB = SQRT(VARB)
      ELSE
         SAB = SQRT(VARA)
      ENDIF

      WRITE(app_msg,*) 'sa2,sb2,vara,varb,sab=',sa2,sb2,vara,varb,sab
      call app_log(APP_DEBUG,app_msg)

      IF (EXCEPTION) THEN
        WRITE(level,'(i5)') a%ip1
      ELSE
        CALL convip_plus(a%ip1,rlevel,kind,-1,level,.true.)
      ENDIF
      ERR_UNIT = RANGE_A / DEUX_EXP_NB

      IF ((ERRMAX .LE. ERRLIM) .and. (PACK_ERR .eq.0)) THEN
         WRITE(6,600) a%nomvar, b%etiket, level, a%ip2, a%ip3, &
                        ERRMAX, ERR, VARA, SAB, ABAR, BBAR-ABAR, &
                        MAXABS, SUMABS
      ELSE IF (PACK_ERR .gt. 0) THEN
         IF (MAXABS .le. (PACK_ERR*ERR_UNIT*1.001)) THEN
           WRITE(6,602) a%nomvar, b%etiket, level, a%ip2, a%ip3, &
                        ERRMAX, ERR, VARA, SAB, ABAR, BBAR-ABAR, &
                        MAXABS, SUMABS, PACK_ERR*ERR_UNIT
         ELSE
            WRITE(6,603) a%nomvar, b%etiket, level, a%ip2, a%ip3, &
                        ERRMAX, ERR, VARA, SAB, ABAR, BBAR-ABAR, &
                        MAXABS, SUMABS, PACK_ERR*ERR_UNIT
            RCMP1D=2
         ENDIF
      ELSE
         WRITE(6,601) a%nomvar, b%etiket, level, a%ip2, a%ip3, &
                        ERRMAX, ERR, VARA, SAB, ABAR, BBAR-ABAR, &
                        MAXABS, SUMABS
            RCMP1D=2
      ENDIF

      if ((nbdiff .ne. 0) .and. (PACK_ERR .gt. 0)) then
        write(6,900) '  <Difference> Number of elements differing is', &
          nbdiff, ' out of ',n, &
          ' elements, Ratio = ',(float(nbdiff) / float(n)) * 100.0,'%'
 900    format(a,i7,a,i8,a,f10.4,a)
        call statfldx(a)
        call statfldx(b)
        write(6,*) ' '
!        write(6,901) 'Err_Max=',ratio_max
! 901    format(a,e8.2,'%')
!        write(6,889) 'Debug MIN_A, MAX_A, MIN_B - MIN_A = ',MIN_A, MAX_A, MIN_B - MIN_A, MAX_B - MAX_A
! 889    format(a,2x,e14.7,2x,e14.7,2x,e14.7,2x,e14.7)
      endif

!  600 FORMAT('  CLEA CLEB NOM  ETIKET    IP1 IP2 IP3  E-REL-MAX', &
!             '  E-REL-MOY   VAR-A       C-COR         MOY-A', &
!             '         BIAIS      E-MAX      E-MOY')
  600 FORMAT(' ', '  ', A4, '  ', A12, a15, 2I9, 4(1X,1PE11.4), &
             2(1X,1PE12.4), 2(1X,1PE11.4) )
  601 FORMAT(' ', ' <', A4, '> ', A12, a15, 2I9, 3(1X,1PE11.4), &
             3(1X,1PE12.4), 2(1X,1PE11.4) )
  602 FORMAT(' ', '  ', A4, '  ', A12, a15, 2I9, 4(1X,1PE11.4), &
             2(1X,1PE12.4), 2(1X,1PE11.4), 2X, 1PE11.4 )
  603 FORMAT(/' ', ' <', A4, '> ', A12, a15, 2I9, 3(1X,1PE11.4), &
             3(1X,1PE12.4), 2(1X,1PE11.4), 2X, 1PE11.4 )

      end function RCMP1D

!    S/P ICMP1D DIFFERENCES ENTRE DEUX CHAMPS ENTIERS DE UNE DIMENSION

!AUTEURS  VERSION ORIGINALE (INTEMP)  M.VALIN DRPN 1987
!         VERSION (ICMP1D)  Y.BOURASSA DRPN JAN 1990

!ARGUMENTS
! ENTRE  A,B     CHAMPS ENTIERS A COMPARER
!   "    EXCEPTION

      integer function ICMP1D(A, B, EXCEPTION)
      use rmn_fst24
      IMPLICIT    NONE
      type(fst_record) :: a, b
      logical :: exception

      INTEGER I, J, K, MD, NC, kind, size
      CHARACTER(len=15) :: Level
      REAL      rlevel
      logical :: success
      integer(kind = int32), dimension(:), pointer :: dataa, datab

      ICMP1D = 0
      MD  = 0
      NC  = 0

      size=a%ni*a%nj*a%nk
      call a % get_data_array(dataa)
      call b % get_data_array(datab)

      DO 10 I=1,size
         if(dataa(I) .NE. datab(I)) THEN
            NC = NC+1
            K  = ABS(dataa(I)-datab(I))
            IF(K .GT. MD) THEN
               J  = size
               MD = K
            ENDIF
         ENDIF
10    CONTINUE

      IF (EXCEPTION) THEN
        WRITE(level,'(i5)') a%ip1
      ELSE
        CALL convip_plus(a%ip1,rlevel,kind,-1,level,.true.)
      ENDIF
      IF (MD .EQ. 0) THEN
         WRITE(6,600) a%nomvar, b%etiket, level, a%ip2, a%ip3
      ELSE
         WRITE(6,601) a%nomvar, b%etiket, level, a%ip2, a%ip3, NC, MD, J
         ICMP1D = 2
      ENDIF

 600  FORMAT(' ',  '  ', A4, '  ', A12, a15, 2I5,' SONT EGAUX')
 601  FORMAT(' ',  ' <', A4, '> ', A12, a15, 2I5,' ONT',I6,' POINTS ', &
            'INEGAUX, L''ERREUR MAX.=',I10,'  AU POINT',I6)

      end function ICMP1D
