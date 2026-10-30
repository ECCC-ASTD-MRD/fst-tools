!> Compare 2 fst files
program fstcomp
    use, intrinsic :: iso_fortran_env, only: int64
    use app
    use rmn_fst24
    use rmn_libc, only: c_exit

    implicit none

#include "fst-tools_build_info.h"


    !AUTEURS
    !     V1.0 ORIGINALE (COMPSTD)   M. LEPINE   (MARS 87)
    !REVISIONS
    ! 001 V1.? AJOUT DU MARQUAGE EN CAS D'ERREUR PLUS GRANDE QUE LA LIMITE
    !          SPECIFIEE
    ! 002 V4.0 VERSION UNIX/FST). Y BOURASSA (JAN 91)
    ! 003 V4.1 POSSIBILITE D'IGNORER LES TYPVAR, NOMVAT, ETIKET, IP1, IP2 ET IP3
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
    ! 027 V8.2 (M. Lepine, Nov  2012) En cas d'erreur, retourner un code d'erreur avec c_exit
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

    external ccard, low2up, convip_plus
    integer, external :: fstopc, fstopl, fnom, fclos

    integer, external :: rcmp1d, icmp1d

    integer, parameter :: nbkeys = 24
    integer, parameter :: key_val_sz = 1024

    character(*), parameter :: header_nor = &
        '  NOM    ETIKET           IP1            IP2       IP3   E-REL-MAX &
        &   E-REL-MOY   VAR-A        C-COR        MOY-A        BIAIS       E-MAX       E-MOY'
    character(*), parameter :: header_tol = &
        '  NOM    ETIKET           IP1            IP2       IP3  E-REL-MAX &
        &  E-REL-MOY    VAR-A      C-COR        MOY-A        BIAIS      E-MAX      E-MOY     TOLERANCE'
    character(*), parameter :: not_found_fmt = "(' Not found ', A4, ' ', A2, ' IP123=', 3I10, I10, ' in ', A40)"
    character(*), parameter :: grid_params_fmt = "(' ', A40, ' GRTYP IG1@4=', A1, 1X, 4I6, /, ' ', A40, ' GRTYP IG1@4=', A1, 1X, 4I6)"
    character(*), parameter :: grid_size_fmt = "(' ', A4, ' -Dimensions found', 3I5, ' looking for', 3I5)"

    integer, parameter :: a_path_idx = 1
    integer, parameter :: b_path_idx = 2
    integer, parameter :: out_idx = 3
    integer, parameter :: as_idx = 4
    integer, parameter :: bs_idx = 5
    integer, parameter :: af_idx = 6
    integer, parameter :: bf_idx = 7
    integer, parameter :: limite_idx = 8
    integer, parameter :: match_date_idx = 9
    integer, parameter :: match_etiket_idx = 10
    integer, parameter :: log_level_idx = 11
    integer, parameter :: omit_box_idx = 12
    integer, parameter :: va_idx = 13
    integer, parameter :: vb_idx = 14
    integer, parameter :: match_typvar_idx = 15
    integer, parameter :: match_ip1_idx = 16
    integer, parameter :: match_ip2_idx = 17
    integer, parameter :: match_ip3_idx = 18
    integer, parameter :: match_nomvar_idx = 19
    integer, parameter :: raw_bin_idx = 20
    integer, parameter :: pack_err_idx = 21
    integer, parameter :: match_grid_idx = 22
    integer, parameter :: exit_code_idx = 23
    integer, parameter :: limval_idx = 24

    character(len = 8), parameter :: CLE(nbkeys) = [character(len = 8) :: &
         'A:',  'B:',   'L.',  'AS',      'BS',  'AF',    'BF',  'LI', &
         'ND',  'NE',    'D',   'N',      'VA',  'VB',    'NT',  'N1', &
         'N2',  'N3',   'NN',   'X', 'PACKERR',  'NG', 'ECODE',  'LD' ]

    character(len = key_val_sz) :: DEF1(nbkeys) = [character(len = key_val_sz) :: &
          'A',   'B', '$OUT', 'NON',     'NON', 'NON',   'NON',  '-7', &
        'NON', 'NON', 'WARN', 'NON',     'NON', 'NON',   'NON', 'NON', &
        'NON', 'NON',  'NON',   'X',       '0', 'NON',   'NON', '-32' ]

    character(len = key_val_sz) :: DEF2(nbkeys) = [character(len = key_val_sz) :: &
          'A',   'B', '$OUT', 'SQI',     'SQI', 'FTN',   'FTN',  '-7', &
        'OUI', 'OUI', 'INFO', 'OUI',      'VA',  'VB',   'OUI', 'OUI', &
        'OUI', 'OUI',  'OUI',   'X',       '1', 'OUI',   'OUI', '-32' ]

    !> Compare reals
    integer, parameter :: ct_real = 1
    !> Compare integers
    integer, parameter :: ct_inte = 2
    !> Comparison impossible
    integer, parameter :: ct_impo = 3

    !> Comparison type matrix based on compared datyp 
    !> \note If key 'X' is set to 'R', comparison_types(0, 0) = ct_real
    integer :: comparison_types(0:6, 0:6) = reshape( [ &
        ct_inte, ct_real, ct_inte, ct_impo, ct_inte, ct_real, ct_real, &
        ct_real, ct_real, ct_impo, ct_impo, ct_impo, ct_real, ct_real, &
        ct_inte, ct_impo, ct_inte, ct_impo, ct_inte, ct_impo, ct_impo, &
        ct_impo, ct_impo, ct_impo, ct_impo, ct_impo, ct_impo, ct_impo, &
        ct_inte, ct_impo, ct_inte, ct_impo, ct_inte, ct_impo, ct_impo, &
        ct_real, ct_real, ct_impo, ct_impo, ct_impo, ct_real, ct_real, &
        ct_real, ct_real, ct_impo, ct_impo, ct_impo, ct_real, ct_real], [7, 7])
    integer :: comparison_type

    CHARACTER(len = 2) :: typvab = '  '
    CHARACTER(len = 4) :: nomvab = '    '
    CHARACTER(len = 12) :: etikb = '            '
    CHARACTER(len = 12) :: a_opt, b_opt, tmp_opt
    CHARACTER(len = key_val_sz) :: tmp_path

    LOGICAL :: a_is_seq, a_is_ftn, b_is_seq, b_is_ftn, a_print, b_print, tmp_bool
    LOGICAL :: match_date, match_etiket, match_typvar, log_info, omit_box, match_ip1, match_ip2, match_ip3, match_nomvar, match_grid
    LOGICAL :: EXCEPTION, return_err_code
    INTEGER(kind = int64) :: a_nbrecs, b_nbrecs
    INTEGER :: LIMITE, LIMVAL, dummy, I
    integer :: IDATE = -1
    integer :: IP1B = -1
    integer :: IP2B = -1
    integer :: IP3B = -1
    integer ier, kind, PACK_ERR, PACK_ERR2
    integer lvar, iunexpv
    real Level
    character(len = 30)  :: ip1_str
    character(len = 128) :: exception_vars = '^^  >>  !!   ^>  META'
    character(len = 512) :: ARMNLIB_var

    type(fst_file)   :: filea, fileb
    type(fst_record) :: recorda, recordb
    type(fst_query)  :: querya, queryb
    logical          :: success

    ! EXTRACTION DES CLES DE LA SEQUENCE D'APPEL
    I = -1
    CALL CCARD(CLE, DEF2, DEF1, nbkeys, I)
    !> \todo Figure out why this is done: The content of DEF1 is already in upper case. Also why operate only from indexes 4 to 11?
    DO I = 4, 11
        CALL LOW2UP(DEF1(I), DEF1(I))
    END DO
    READ(DEF1(limite_idx), '(I8)') LIMITE
    READ(DEF1(pack_err_idx), '(I8)') PACK_ERR
    READ(DEF1(limval_idx), '(I8)') LIMVAL

    a_print = DEF1(va_idx) /= 'NON'
    IF (a_print .AND. (DEF1(va_idx) /= 'VA')) DEF1(a_path_idx) = DEF1(va_idx)
    b_print = DEF1(vb_idx) /= 'NON'
    IF (b_print .AND. (DEF1(vb_idx) /= 'VB')) DEF1(b_path_idx) = DEF1(vb_idx)

    a_is_ftn = INDEX(DEF1(af_idx), 'FTN') /= 0
    IF (a_is_ftn) THEN
        a_opt = 'STD+SEQ+FTN'
        a_is_seq = .FALSE.
    ELSE
        a_is_seq = INDEX(DEF1(as_idx), 'SQI') /= 0
        IF (a_is_seq) THEN
            a_opt = 'STD+SEQ'
        ELSE
            a_opt = 'STD+RND'
        ENDIF
    ENDIF

    b_is_ftn = INDEX(DEF1(bf_idx), 'FTN') /= 0
    IF (b_is_ftn) THEN
        b_opt = 'STD+SEQ+FTN'
        b_is_seq   = .FALSE.
    ELSE
        b_is_seq = INDEX(DEF1(bs_idx), 'SQI') /= 0
        IF (b_is_seq) THEN
            b_opt = 'STD+SEQ'
        ELSE
            b_opt = 'STD+RND'
        ENDIF
    ENDIF
    I  = FNOM(6, DEF1(out_idx), 'SEQ+FTN', 0)
    match_date = DEF1(match_date_idx) == 'NON'
    match_etiket = DEF1(match_etiket_idx) == 'NON'
    match_typvar = DEF1(match_typvar_idx) == 'NON'
    match_ip1 = DEF1(match_ip1_idx) == 'NON'
    match_ip2 = DEF1(match_ip2_idx) == 'NON'
    match_ip3 = DEF1(match_ip3_idx) == 'NON'
    match_nomvar = DEF1(match_nomvar_idx) == 'NON'
    match_grid = DEF1(match_grid_idx) == 'NON'
    log_info = DEF1(log_level_idx) == 'INFORM'
    omit_box = DEF1(omit_box_idx) == 'OUI'
    return_err_code = DEF1(exit_code_idx) == 'OUI'
    IF (DEF1(raw_bin_idx) == 'R') comparison_types(0, 0) = 1

    app_ptr = app_init(0, 'fstcomp', FSTCOMP_VERSION, '', BUILD_TIMESTAMP)
    call app_logstream(DEF1(out_idx))

    IF (omit_box) THEN
        call app_log(APP_VERBATIM, '* * * ' // FSTCOMP_VERSION // ' * * *')
    ELSE
        call app_start()
    ENDIF

    app_status = 0

    ier = app_loglevel(DEF1(log_level_idx))
    ier = fstopl('REDUCTION32', .true., .false.)

    CALL GETENV('ARMNLIB', ARMNLIB_var)
    lvar = len_trim(ARMNLIB_var)
    IF (lvar > 0) THEN
        iunexpv = 0
        ier = fnom(iunexpv, ARMNLIB_var(1:lvar) // '/data/exception_vars_ok', 'SEQ+FTN+FMT+OLD+R/O', 0)
        IF (ier < 0) THEN
            WRITE(app_msg, *) '$ARMNLIB_DATA/exception_vars file not found; using internal exception list'
            call app_log(APP_INFO, app_msg)
        ELSE
            READ(iunexpv, '(a)') exception_vars
            ier = fclos(iunexpv)
        ENDIF
    ENDIF

    WRITE(app_msg, *) 'exception_vars=', exception_vars
    call app_log(APP_DEBUG, app_msg)

    ! SI A=RND & B=SEQ CHANGE [A POUR B] & [B POUR A]
    IF ((b_is_ftn .OR. b_is_seq) .AND. .NOT.(a_is_ftn .OR. a_is_seq)) THEN
        tmp_path = DEF1(a_path_idx)
        DEF1(a_path_idx) = DEF1(b_path_idx)
        DEF1(b_path_idx) = tmp_path
        tmp_opt = a_opt
        a_opt = b_opt
        b_opt = tmp_opt
        tmp_bool = b_is_seq
        b_is_seq = a_is_seq
        a_is_seq = tmp_bool
        tmp_bool = a_is_ftn
        a_is_ftn = b_is_ftn
        b_is_ftn = tmp_bool
    ENDIF

    IF ((DEF1(a_path_idx) == DEF1(b_path_idx))) then
        WRITE(app_msg, *) 'Comparing file "A" with itself'
        call app_log(APP_WARNING, app_msg)
    endif

    success = filea%open(DEF1(a_path_idx), a_opt // '+OLD+R/O+REMOTE')
    if (.not. success) then
        write(app_msg, *) 'Invalid input file'
        call app_log(APP_ERROR, app_msg)
        app_status = app_end(-1)
        call c_exit(-1)
    endif
    a_nbrecs = filea%get_num_records()

    success = fileb%open(DEF1(b_path_idx), b_opt // '+OLD+R/O+REMOTE')
    if (.not. success) then
        write(app_msg, *) 'Invalid output file'
        call app_log(APP_ERROR, app_msg)
        app_status = app_end(-1)
        call c_exit(-1)
    endif
    b_nbrecs = fileb%get_num_records()

    IF (a_nbrecs <= 0) THEN
        WRITE(app_msg, *) 'Sequential file ', DEF1(a_path_idx)
        call app_log(APP_INFO, app_msg)
        IF (.NOT. (a_is_seq .OR. a_is_ftn) ) THEN
            WRITE(app_msg, *) 'File declared random ', DEF1(a_path_idx)
            call app_log(APP_INFO, app_msg)
            GOTO 60
        ENDIF
    ELSE
        WRITE(app_msg, *) ' ', a_nbrecs, ' records in ', DEF1(a_path_idx)
        call app_log(APP_INFO, app_msg)
        IF (a_nbrecs == 0) GOTO 90
    ENDIF

    IF (b_nbrecs <= 0) THEN
        WRITE(app_msg, *) 'Sequential file ', DEF1(b_path_idx)
        call app_log(APP_INFO, app_msg)
        IF (.NOT. b_is_seq) THEN
            WRITE(app_msg, *) 'File declared random  ', DEF1(b_path_idx)
            call app_log(APP_INFO, app_msg)
            GOTO 80
        ENDIF
    ELSE
        WRITE(app_msg, *) ' ', b_nbrecs, ' records in ', DEF1(b_path_idx)
        call app_log(APP_INFO, app_msg)
        IF (b_nbrecs == 0) GOTO 80
    ENDIF

    ! Ecrire l'entete de page
    IF (PACK_ERR == 0) THEN
        call app_log(APP_VERBATIM, header_nor)
    ELSE
        call app_log(APP_VERBATIM, header_tol)
    ENDIF

    IF (a_is_seq .OR. a_is_ftn) dummy = filea%rewind()

    querya = filea%new_query()
    do while(querya%find_next(recorda))
        if (recorda%NOMVAR == '!!') then
            WRITE(app_msg, *) 'Skipping record "!!", can''t compare'
            call app_log(APP_INFO, app_msg)
            cycle
        endif

        ! SI LA DATE EST A CONSIDERER
        IF (match_date) then
            IF (recorda%dateo == 0) then
                idate = 0
            else
                idate = recorda%datev
            endif
        endif

        ! SI ETIKET EST A CONSIDERER
        IF (match_etiket) etikb = recorda%etiket

        ! SI TYPVAR EST A CONSIDERER
        IF (match_typvar) typvab = recorda%typvar

        ! SI NOMVAR EST A CONSIDERER
        IF (match_nomvar) nomvab = recorda%nomvar

        ! SI IP1 EST A CONSIDERER
        IF (match_ip1) then
            IF (INDEX(exception_vars, recorda%nomvar) /= 0) then
                ip1b = recorda%ip1
                EXCEPTION = .TRUE.
            ELSE
                call convip_plus(recorda%ip1, level, kind, -1, ip1_str, .false.)
                ip1b = recorda%ip1
                EXCEPTION = .FALSE.
            ENDIF
        ENDIF

        ! SI IP2 EST A CONSIDERER
        IF (match_ip2) ip2b = recorda%ip2

        ! SI IP3 EST A CONSIDERER
        IF (match_ip3) ip3b = recorda%ip3

        if (b_is_seq .OR. b_is_ftn) dummy = fileb%rewind()
        queryb = fileb%new_query(datev=idate, etiket=etikb, ip1=ip1b, ip2=ip2b, ip3=ip3b, typvar=typvab, nomvar=nomvab)
        success = queryb%find_next(recordb)

        IF (.not. success) THEN
            write(app_msg, not_found_fmt) nomvab, typvab, ip1b, ip2b, ip3b, idate, DEF1(b_path_idx)
            call app_log(APP_WARNING, app_msg)
            app_status = 2
            cycle
        ENDIF

        ! VERIFICATION DES DIMENSIONS DE LA GRILLE SI PRESENT
        IF ((recordb%ni /= recorda%ni) .OR. (recordb%nj /= recorda%nj) .OR. (recordb%nk /= recorda%nk)) THEN
            write(app_msg, grid_size_fmt) NOMVAB, recorda%ni, recorda%nj, recorda%nk, recordb%ni, recordb%nj, recordb%nk
            call app_log(APP_WARNING, app_msg)
            app_status = 1
            cycle
        ENDIF

        ! VERIFICATION DES PARAMETRES DE LA GRILLE
        IF (match_grid) THEN
            IF ((recorda%grtyp /= recordb%grtyp) .OR. (recorda%ig1 /= recordb%ig1) .OR. (recorda%ig2 /= recordb%ig2) .OR. &
                (recorda%ig3 /= recordb%ig3) .OR. (recorda%ig4 /= recordb%ig4)) THEN
                write(app_msg, grid_params_fmt) DEF1(a_path_idx), recorda%grtyp, recorda%ig1, recorda%ig2, recorda%ig3, recorda%ig4, &
                            DEF1(b_path_idx), recordb%grtyp, recordb%ig1, recordb%ig2, recordb%ig3, recordb%ig4
                call app_log(APP_WARNING, app_msg)
                app_status=1
                cycle
            ENDIF
        ENDIF

        IF (recorda%pack_bits /= recordb%pack_bits) then
            write(app_msg, *) 'NBITSA=', recorda%pack_bits, ' NBITSB=', recordb%pack_bits
            app_status = 1
            call app_log(APP_INFO, app_msg)
        endif

        WRITE(app_msg, *) 'COMPARE DATA_TYPE_A=', recorda%data_type, '  @  DATA_TYPE_B=', recordb%data_type
        call app_log(APP_INFO, app_msg)

        ! TOUT EST OK LIT ET COMPARE
        success = recorda%read()
        success = recordb%read()
        recordb%etiket = etikb

        IF ((mod(recorda%data_type, 128) /= 1) .and. (mod(recorda%data_type, 128) /= 6)) THEN
            PACK_ERR2 = 0
        ELSE
            PACK_ERR2 = PACK_ERR
        ENDIF

        if ((mod(recorda%data_type, 128) > 6) .or. (mod(recordb%data_type, 128) > 6)) then
            ! Out of bounds!
            comparison_type = ct_impo
        else
            comparison_type = comparison_types(mod(recorda%data_type, 128), mod(recordb%data_type, 128))
        end if
        select case (comparison_type)
            case (ct_real)
                app_status = rcmp1d(recorda, recordb, limite, limval, pack_err2, exception)
            case (ct_inte)
                app_status = icmp1d(recorda, recordb, exception)
                call queryb%free()
            case default
                WRITE(app_msg, *)' No comparison possible: DATA_TYPE_A=', recorda%data_type, ' DATA_TYPE_B=', recordb%data_type
                app_status = 1
                call app_log(APP_WARNING, app_msg)
        end select
    enddo

    ! Skip field loop anchor point
    60  continue

    IF (a_print .AND. .NOT. a_is_ftn .OR. b_print .AND. .NOT. b_is_ftn) THEN
        IF (.NOT. log_info) dummy = FSTOPC('MSGLVL', 'INFORM', .FALSE.)
        IF (a_print) THEN
            IF (a_is_seq) dummy = filea%rewind()
            WRITE(app_msg, *)' DN=', DEF1(a_path_idx)
            call app_log(APP_VERBATIM, app_msg)
            call filea%print_summary()
        ENDIF
        IF (b_print) THEN
            IF (b_is_seq) dummy = fileb%rewind()
            WRITE(app_msg, *)' DN=', DEF1(b_path_idx)
            call app_log(APP_VERBATIM, app_msg)
            call fileb%print_summary()
        ENDIF
    ENDIF

   80 success = fileb%close()
   90 success = filea%close()

    IF (omit_box) THEN
        call app_log(APP_VERBATIM, '* * *  fstcomp end  * * *')
    ELSE
        app_status = app_end(app_status)
    ENDIF

    if (.NOT. return_err_code) then
        if (app_status == 2) then
            app_status = 0
        end if
    end if
    call c_exit(app_status)
END


!> Compare two 32 bits real fields
integer function rcmp1d(a, b, limite, limval, pack_err, rawIp1)
    use app
    use rmn_fst24
    implicit none

    !< \return 0 when the fields are equivalent, 2 otherwise

    !> First 32bit real field to compare
    type(fst_record), intent(in) :: a
    !> Second 32bit real field to compare
    type(fst_record), intent(in) :: b
    !> Maximum power of 10 exponent value for the relative error to consider the fields equivalent
    integer, intent(in) :: limite
    !> Minimum power of 10 exponent value under which the absolute value of the difference will cause the value to be considered equivalent
    integer, intent(in) :: limval
    !> Maximum number of unit errors due to the packing algorithm to consider the fields equivalent
    integer, intent(in) :: pack_err
    !> Print raw numeric IP1 values if true
    logical, intent(in) :: rawIp1

    character(*), parameter :: equiv_fmt = &
        "(' ', '  ', A4, '  ', A12, a15, 2I9, 4(1X, 1PE11.4), 2(1X, 1PE12.4), 2(1X, 1PE11.4))"
    character(*), parameter :: equiv_pack_err_fmt = &
        "(' ', '  ', A4, '  ', A12, a15, 2I9, 4(1X, 1PE11.4), 2(1X, 1PE12.4), 2(1X, 1PE11.4), 2X, 1PE11.4)"
    character(*), parameter :: diff_pack_err_fmt = &
        "(/' ', ' <', A4, '> ', A12, a15, 2I9, 3(1X, 1PE11.4), 3(1X, 1PE12.4), 2(1X, 1PE11.4), 2X, 1PE11.4)"
    character(*), parameter :: diff_fmt = &
        "(' ', ' <', A4, '> ', A12, a15, 2I9, 3(1X, 1PE11.4), 3(1X, 1PE12.4), 2(1X, 1PE11.4))"
    character(*), parameter :: diff_summary_fmt = &
        "(a, i7, a, i8, a, f10.4, a)"

    real(kind = real32), dimension(:), pointer :: data_a, data_b
    integer :: size
    real :: max_err_abs, sum_err_abs, err_abs
    integer :: i, kind, irange
    character(len = 15) :: level
    real :: rlevel
    real(kind = real64) :: sum_a, sum_b, sa2, sb2, err, rel_err, max_rel_err, avg_a, avg_b, &
                           val_a, val_b, fsize, errlim, errval, vara, varb, sab
    real :: min_a, max_a, min_b, max_b, range_a, range_b, deux_exp_nb
    real :: ratio_max, ratio
    real :: err_unit
    integer :: nbDiff

    external statfldx

    rcmp1d = 0
    call a%get_data_array(data_a)
    call b%get_data_array(data_b)

    nbDiff = 0
    size = a%ni * a%nj * a%nk

    errlim = 10.0 ** limite
    errval = 10.0 ** limval

    deux_exp_nb = 2.0 ** min(a%pack_bits, b%pack_bits)
    sum_a = 0.0
    sum_b = 0.0
    sab = 0.0
    sa2 = 0.0
    sb2 = 0.0
    max_rel_err = 0.0
    err = 0.0
    sum_err_abs = 0.0
    max_err_abs = 0.0
    ratio_max = 0.0
    min_a = data_a(1)
    max_a = data_a(1)
    min_b = data_b(1)
    max_b = data_b(1)
    do i = 1, size
        val_a = data_a(i)
        val_b = data_b(i)
        min_a = min(min_a, data_a(i))
        max_a = max(max_a, data_a(i))
        min_b = min(min_b, data_b(i))
        max_b = max(max_b, data_b(i))
        if (abs(val_a - val_b) < errval) then
            val_a = val_b
        endif
        sum_a = sum_a + val_a
        sum_b = sum_b + val_b
        if (val_a /= val_b) then
            if (val_a /= 0.0) ratio = real( (max(val_a, val_b) - min(val_a, val_b)) / val_a * 100 )
            if (ratio > ratio_max) ratio_max = ratio
            nbDiff = nbDiff + 1
            err_abs = real( abs(val_a - val_b) )
            sum_err_abs = sum_err_abs + err_abs
            max_err_abs = max(err_abs, max_err_abs)
            rel_err = 0.0
            if (val_a /= 0.0) then
                rel_err = abs(1.0 - val_b / val_a)
            elseif (val_b /= 0.0) then
                rel_err = abs(1.0 - val_a / val_b)
            endif
            max_rel_err = max(max_rel_err, rel_err)
            err = err + rel_err
        endif
    end do

    write(app_msg, *) 'nbDiff = ', nbDiff, ' sur un total de ', size
    call app_log(app_info, app_msg)

    range_a = max_a - min_a
    range_b = max_b - min_b
    irange = transfer(range_a, 1)
    irange = ishft(ishft(irange, -23) +1, 23)
    range_a = transfer(irange, 1.0)
    fsize = float(size)

    err = err / fsize
    avg_a = sum_a / fsize
    avg_b = sum_b / fsize
    do i = 1, size
        val_a = data_a(i) - avg_a
        val_b = data_b(i) - avg_b
        if (abs(data_a(i) - data_b(i)) < errval) then
            val_b = val_a
        endif
        sab = sab + val_a * val_b
        sa2 = sa2 + val_a * val_a
        sb2 = sb2 + val_b * val_b
    end do
    sum_err_abs = real(sum_err_abs / fsize)
    vara = sa2 / fsize
    varb = sb2 / fsize
    write(app_msg, *) 'sab avant=', sab
    call app_log(app_debug, app_msg)
    if (sa2 * sb2 /= 0.0) then
        sab = sab / sqrt(sa2 * sb2)
        write(app_msg, *) 'sa2,sb2,sqtr,sab=', sa2, sb2, sqrt(sa2*sb2), sab
        call app_log(app_debug, app_msg)
    elseif (sa2 == 0.0 .and. sb2 == 0.0) then
        sab = 1.0
    elseif (sa2 == 0.) then
        sab = sqrt(varb)
    else
        sab = sqrt(vara)
    endif

    write(app_msg, *) 'sa2,sb2,vara,varb,sab=', sa2, sb2, vara, varb, sab
    call app_log(app_debug, app_msg)

    if (rawIp1) then
        write(level, '(i5)') a%ip1
    else
        call convip_plus(a%ip1, rlevel, kind, -1, level, .true.)
    endif
    err_unit = range_a / deux_exp_nb

    if ((max_rel_err <= errlim) .and. (pack_err == 0)) then
        write(app_msg, equiv_fmt) a%nomvar, b%etiket, level, a%ip2, a%ip3, &
            max_rel_err, err, vara, sab, avg_a, avg_b - avg_a, max_err_abs, sum_err_abs
        call app_log(APP_VERBATIM, app_msg)
    else if (pack_err > 0) then
        if (max_err_abs <= (pack_err * err_unit * 1.001)) then
            write(app_msg, equiv_pack_err_fmt) a%nomvar, b%etiket, level, a%ip2, a%ip3, &
                max_rel_err, err, vara, sab, avg_a, avg_b - avg_a, max_err_abs, sum_err_abs, pack_err * err_unit
            call app_log(APP_VERBATIM, app_msg)
        else
            write(app_msg, diff_pack_err_fmt) a%nomvar, b%etiket, level, a%ip2, a%ip3, &
                max_rel_err, err, vara, sab, avg_a, avg_b - avg_a, max_err_abs, sum_err_abs, pack_err * err_unit
            call app_log(APP_VERBATIM, app_msg)
            rcmp1d = 2
        endif
    else
        write(app_msg, diff_fmt) a%nomvar, b%etiket, level, a%ip2, a%ip3, &
            max_rel_err, err, vara, sab, avg_a, avg_b - avg_a, max_err_abs, sum_err_abs
        call app_log(APP_VERBATIM, app_msg)
        rcmp1d = 2
    endif

    if ((nbDiff /= 0) .and. (pack_err > 0)) then
        write(app_msg, diff_summary_fmt) '  <Difference> Number of elements differing is', &
            nbDiff, ' out of ', size, &
            ' elements, Ratio = ', (float(nbDiff) / float(size)) * 100.0, '%'
        call app_log(APP_VERBATIM, app_msg)
        call statfldx(a)
        call statfldx(b)
        call app_log(APP_VERBATIM, '')
    endif
end function rcmp1d


!> Compare two 32 bits integer scalar fields
integer function icmp1d(a, b, rawIp1)
    use rmn_fst24
    implicit none

    !< \return 0 when the fields are identical, 2 when they are different

    !> First field to compare
    type(fst_record), intent(in) :: a
    !> Second field to compare
    type(fst_record), intent(in) :: b
    !> Print raw numeric IP1 values if true
    logical, intent(in) :: rawIp1

    character(*), parameter :: equal_fmt = &
        "(' ',  '  ', a4, '  ', a12, a15, 2i5, ' SONT EGAUX')"
    character(*), parameter :: diff_fmt = &
        "(' ',  ' <', a4, '> ', a12, a15, 2i5, ' ONT', i6, ' POINTS ', 'INEGAUX, L''ERREUR MAX.=', i10, '  AU POINT', i6)"

    integer i, maxDiffPt, absDiff, maxAbsDiff, nbDiff, kind, size
    character(len = 15) :: level
    real :: rlevel
    integer(kind = int32), dimension(:), pointer :: data_a, data_b

    icmp1d = 0
    maxAbsDiff = 0
    nbDiff = 0

    size = a%ni * a%nj * a%nk
    call a%get_data_array(data_a)
    call b%get_data_array(data_b)

    do i = 1, size
        if (data_a(i) /= data_b(i)) then
            nbDiff = nbDiff + 1
            absDiff = abs(data_a(i) - data_b(i))
            if (absDiff > maxAbsDiff) then
                maxDiffPt = i
                maxAbsDiff = absDiff
            endif
        endif
    end do

    if (rawIp1) then
        write(level, '(i5)') a%ip1
    else
        call convip_plus(a%ip1, rlevel, kind, -1, level, .true.)
    endif
    if (maxAbsDiff == 0) then
        write(app_msg, equal_fmt) a%nomvar, b%etiket, level, a%ip2, a%ip3
        call app_log(APP_VERBATIM, app_msg)
    else
        write(app_msg, diff_fmt) a%nomvar, b%etiket, level, a%ip2, a%ip3, nbDiff, maxAbsDiff, maxDiffPt
        call app_log(APP_VERBATIM, app_msg)
        icmp1d = 2
    endif
end function icmp1d
