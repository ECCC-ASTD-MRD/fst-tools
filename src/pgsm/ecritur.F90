!> Ecrire sur fichier standard, ms, sequentiel
subroutine ecritur(fld, npac, idat, deet, npas, ni, nj, nk, ip1, ip2, ip3, ctypvar, cnomvar, cetiket, cgtyp, llg1, llg2, llg3, llg4)
    use ISO_FORTRAN_ENV, only : real64
    use app
    use files, only : outputFile, outputFileMode
    use packing, only : npack_orig
    use pgsm_mod, only: validate_date
    use pgsm_mod, only: iwrit, nsort, message
    use pgsm_mod, only: dateform, tmplat, tmplon
    use ecrires, only : etiksrt, typesrt, ip2srt, printsr, compression_level
    implicit none

    !> First dimension of field
    integer, intent(in) :: ni
    !> Second dimension of field
    integer, intent(in) :: nj
    !> Third dimension of field
    integer, intent(in) :: nk
    !> Field to write
    real, intent(in) :: fld(ni, nj, nk)
    !> Convention of the data in the field to write
    integer, intent(in) :: npac
    !> Origin date (CMC stamp)
    integer, intent(in) :: idat
    !> Duration of each timestep in seconds
    integer, intent(in) :: deet
    !> Timestep number
    integer, intent(in) :: npas
    !> Level
    integer, intent(in) :: ip1
    !> Forecast hour
    integer, intent(in) :: ip2
    !> User defined identifier
    integer, intent(in) :: ip3
    !> Variable type
    character(len = 2), intent(in) :: ctypvar
    !> Variable name
    character(len = 4), intent(in) :: cnomvar
    !> Label
    character(len = 12), intent(in) :: cetiket
    !> Grid type
    character(len = 1), intent(in) :: cgtyp
    !> First grid descriptor
    integer, intent(in) :: llg1
    !> Second grid descriptor
    integer, intent(in) :: llg2
    !> Third grid descriptor
    integer, intent(in) :: llg3
    !> Forth grid descriptor
    integer, intent(in) :: llg4

    ! DESCRIPTEUR DE GRILLE
    ! PS - LLG1 POSITION J DU POLE
    !      LLG2 POSITION I DU POLE
    !      LLG3 DGRW*100
    !      LLG4 D60 HETOMETRE(0-36000)
    ! LAT-LON  LLG1- DLAT*100
    !          LLG2- DLON*100
    !          LLG3- (90-LAT)*100 0=POLE SUD
    !          LLG4- (LON*100) (0-36000) LONGITUDE COIN
    ! GAUSSIEN  LLG1= 1 HEMISPHERE NORD
    !           LLG1= 2 HEMISPHERE SUD
    !           LLG1= 3 GLOBALE

    external :: conver, imprims
    integer, external :: fstopc

#include "enrege.cdk90"
#include "idents.cdk90"
#include "qqqfilt.cdk90"

    character(len = 24) :: chaine
    character(len = 12) :: cetksrt

    character(len = 12) :: letiksrt
    character(len = 4) :: lnomvar
    character(len = 2) :: ltypsrt

    integer :: i, idatv
    integer :: iun, ip3o, ip2o
    integer :: cdatyp, iopc, ier, gdout, datev
    logical :: rewrit

    integer, external :: gdll, ezgetgdout

    integer :: local_npac

    real(kind = real64) :: delta_t

    type(fst_record) :: record

    if (etiksrt(1) == -1) then
        cetksrt = cetiket
    else
        write(cetksrt, '(3A4)') (etiksrt(i), i=1, nwetiks)
    endif

    if (typesrt == -1) then
        ltypsrt = ctypvar
    else
        write(ltypsrt, '(A2)') typesrt
    endif

    if (ip3srt /= -1) then
        ip3o = ip3srt
    else
        ip3o = ip3
    endif

    if (ip2srt /= -1) then
        ip2o = ip2srt
    else
        ip2o = ip2
    endif

    lnomvar = cnomvar
    letiksrt = cetksrt

    if (npac == 1023) then
        local_npac = npack_orig
    else
        local_npac = npac
    endif

    ! SI LE NOM EXISTE DANS LA TABLE BATIT PAR L USAGER ALORS
    ! LE fld EST MODIFIE  EX: fld(NI, NJ) = (fld(NI, NJ)+ECART)*FACTEUR
    call conver(fld, ni, nj, cnomvar)

    ! filtrage du fld de sortie si le fld n'est pas un stream latlon
    if (fltoggle(2)) then
        if (cgtyp == 'Y') then
            call app_log(APP_WARNING, 'ecritur: Cannot filter fields on Y grid')
        else
            call app_log(APP_INFO, 'ecritur: Field filtered on write')
            call filtre (fld, NI, NJ, fltntimes(2), fltlist(1, 2), fltwgtlng(2))
        endif
    endif

    if (printsr)  then
        call imprims(cnomvar, fld, ni, nj)
    endif

    !iun = lnkdiun(idx_ozsrt)
    if (outputFileMode == 1) then
        if (compression_level == 0) then
            cdatyp = 1
        else
            if (local_npac < 0 .and. local_npac >= -16) then
                cdatyp = 134
            else if (local_npac == -32) then
                cdatyp = 133
            else
                cdatyp = 1
            endif
        endif


        if (iwrit == +1) then
            ! if (.not. message) then
            !     iopc = fstopc('TOLRNC', 'DEBUGS', .true.)
            ! endif
            rewrit = .true.

            ! ier = fstecr(fld, dummy, local_npac, iun, idat, deet, npas, ni, nj, nk, ip1, ip2o, ip3o, ltypsrt, cnomvar, cetksrt, cgtyp, llg1, llg2, llg3, llg4, cdatyp, rewrit )
        else
            ! if (.not. message) then
            !     iopc = fstopc('TOLRNC', 'DEBUGS', .true.)
            ! endif
            rewrit = .false.
            ! ier = fstecr(fld, dummy, local_npac, iun, idat, deet, npas, ni, nj, nk, ip1, ip2o, ip3o, ltypsrt, cnomvar, cetksrt, cgtyp, llg1, llg2, llg3, llg4, cdatyp, rewrit )
        endif
        if (.not. message) then
            iopc = fstopc('TOLRNC', 'DEBUGS', .true.)
        endif
            record%data => c_loc(fld)
            record%npak = local_npac
            record%date = idat
            record%deet = deet
            record%npas = npas
            record%ni = ni
            record%nj = nj
            record%nk = nk
            record%ip1 = ip1
            record%ip2 = ip2o
            record%ip3 = ip3o
            record%typvar = ltypsrt
            record%nomvar = cnomvar
            record%etiket = cetksrt
            record%grtyp = cgtyp
            record%ig1 = llg1
            record%ig2 = llg2
            record%ig3 = llg3
            record%ig4 = llg4
            record%datyp = cdatyp
            record%rewrite = rewrit
            outputFile%write(record)
    else
        if (outputFileMode == 2) then
            call app_log(APP_WARNING, 'ecritur: "MS" file type are not supported anymore')
        endif
        if (outputFileMode == 3 .or. outputFileMode == 4) then
            if (outputFileMode == 4) then
                cdatyp = 1
                write (chaine, '(a2, 2x, a4, a12, a1, 3x)') ltypsrt, lnomvar, letiksrt, cgtyp
                write (iun) npac, idat, deet, npas, ni, nj, nk, ip1, ip2o, ip3o, llg1, llg2, llg3, llg4, cdatyp, chaine
            endif

            write(iun) fld
            if (message) then
                write(app_msg, "(2x, ' ENREG.ECRIT ', 2(a2, '- '), 3(i5, '- '), 'TAILLE ', 2(i5, '- '), 'FICHIER SEQUENTIEL', i4)") &
                    ltypsrt, cnomvar, ip1, ip2o, ip3o, ni, nj, iun
            endif
        else if (outputFileMode == 5) then
            if (validate_date) then
                call chk_userdate(datev)
            endif

            delta_t = deet * npas / 3600.0
            call incdatr(idatv, idat, delta_t)

            gdout = ezgetgdout()
            if (gdout < 0) gdout = 0
            if (cnomvar(1:2) /= 'LA') then
                if (associated(tmplat)) then
                    deallocate(tmplat)
                    nullify(tmplat)
                    allocate(tmplat(ni, nj))
                endif
            endif

            if (cnomvar(1:2) /= 'LO') then
                if (associated(tmplon)) then
                    deallocate(tmplon)
                    nullify(tmplon)
                    allocate(tmplon(ni, nj))
                endif
            endif

            ier = gdll(gdout, tmplat, tmplon)
            call pgsmwr(2, fld, ni, nj, nk, qcform, qposition, qitems, qcsepar, cnomvar, ctypvar, cetiket, idat, idatv, dateform, ip1, ip2, ip3, tmplat, tmplon)
        else
            if (message) then
                call app_log(APP_ERROR, 'ecritur: Unknown file')
            endif
        endif
      endif
end


subroutine iecritur(fld, npac, idat, deet, npas, ni, nj, nk, ip1, ip2, ip3, ctypvar, cnomvar, cetiket, cgtyp, llg1, llg2, llg3, llg4)
    use ISO_FORTRAN_ENV, only : real64
    use app
    use files, only : outputFile, outputFileMode
    use packing, only : npack_orig
    use dates, only: validate_date
    implicit none

    !> First dimension of field
    integer, intent(in) :: ni
    !> Second dimension of field
    integer, intent(in) :: nj
    !> Third dimension of field
    integer, intent(in) :: nk
    !> Field to write
    integer, intent(in) :: fld(ni, nj, nk)
    !> Convention of the data in the field to write
    integer, intent(in) :: npac
    !> Origin date (CMC stamp)
    integer, intent(in) :: idat
    !> Duration of each timestep in seconds
    integer, intent(in) :: deet
    !> Timestep number
    integer, intent(in) :: npas
    !> Level
    integer, intent(in) :: ip1
    !> Forecast hour
    integer, intent(in) :: ip2
    !> User defined identifier
    integer, intent(in) :: ip3
    !> Variable type
    character(len = 2), intent(in) :: ctypvar
    !> Variable name
    character(len = 4), intent(in) :: cnomvar
    !> Label
    character(len = 12), intent(in) :: cetiket
    !> Grid type
    character(len = 1), intent(in) :: cgtyp
    !> First grid descriptor
    integer, intent(in) :: llg1
    !> Second grid descriptor
    integer, intent(in) :: llg2
    !> Third grid descriptor
    integer, intent(in) :: llg3
    !> Forth grid descriptor
    integer, intent(in) :: llg4

    integer, external :: fstopc

#include "enrege.cdk90"
#include "idents.cdk90"
#include "qqqfilt.cdk90"

    character(len = 24) :: chaine
    character(len = 12) :: cetksrt
    character(len = 12) :: letiksrt
    character(len = 4) :: lnomvar
    character(len = 2) :: ltypsrt

    integer :: i
    integer :: iun, ip3o, ip2o
    integer :: cdatyp, iopc, ier, local_npac
    logical :: rewrit

    integer, external :: gdll, ezgetgdout

    real(kind = real64) :: delta_t

    type(fst_record) :: record

    if (etiksrt(1) == -1) then
        cetksrt = cetiket
    else
        write(cetksrt, '(3A4)') (etiksrt(i), i=1, nwetiks)
    endif

    if (typesrt == -1) then
        ltypsrt = ctypvar
    else
        write(ltypsrt, '(A2)') typesrt
    endif

    if (ip3srt /= -1) then
        ip3o = ip3srt
    else
        ip3o = ip3
    endif

    if (ip2srt /= -1) then
        ip2o = ip2srt
    else
        ip2o = ip2
    endif

    lnomvar = cnomvar
    letiksrt = cetksrt

    if(npac == 1023) then
        local_npac = npack_orig
    else
        local_npac = npac
    endif

    !iun=lnkdiun(idx_ozsrt)
    if (outputFileMode == 1) then
        if (compression_level == 0) then
            cdatyp = 2
        else
            if (local_npac < 0 .and. local_npac >= -16) then
                cdatyp = 130
            else
                cdatyp = 2
            endif
        endif

        if (iwrit == +1) then
            ! if (.not. message) then
            !     iopc = fstopc('TOLRNC', 'DEBUGS', .true.)
            ! endif
            rewrit = .true.

            ! ier = fstecr(fld, dummy, local_npac, iun, idat, deet, npas, ni, nj, nk, &
            !   ip1, ip2o, ip3o, ltypsrt, cnomvar, cetksrt, cgtyp, llg1, llg2, llg3, llg4, cdatyp, rewrit )
        else
            ! if (.not. message) then
            !     iopc = fstopc('TOLRNC', 'DEBUGS', .true.)
            ! endif
            rewrit = .false.
            ! ier = fstecr(fld, dummy, local_npac, iun, idat, deet, npas, ni, nj, nk, &
            !   ip1, ip2o, ip3o, ltypsrt, cnomvar, cetksrt, cgtyp, llg1, llg2, llg3, llg4, cdatyp, rewrit )
        endif
            record%data => c_loc(fld)
            record%npak = local_npac
            record%date = idat
            record%deet = deet
            record%npas = npas
            record%ni = ni
            record%nj = nj
            record%nk = nk
            record%ip1 = ip1
            record%ip2 = ip2o
            record%ip3 = ip3o
            record%typvar = ltypsrt
            record%nomvar = cnomvar
            record%etiket = cetksrt
            record%grtyp = cgtyp
            record%ig1 = llg1
            record%ig2 = llg2
            record%ig3 = llg3
            record%ig4 = llg4
            record%datyp = cdatyp
            record%rewrite = rewrit
            outputFile%write(record)
    else
        if (outputFileMode == 2) then
            call app_log(APP_WARNING, 'ecritur: "MS" file type are not supported anymore')
        else if (outputFileMode == 3 .or. outputFileMode == 4) then
            if (outputFileMode == 4) then
                cdatyp = 1
                write (chaine, '(a2, 2x, a4, a12, a1, 3x)') ltypsrt, lnomvar, letiksrt, cgtyp
                write (iun) npac, idat, deet, npas, ni, nj, nk, ip1, ip2o, ip3o, &
                    llg1, llg2, llg3, llg4, cdatyp, chaine
            endif

            write(iun) fld
            if (message) then
                write(app_msg, "(2x, 'ecritur:  Record written ', 2(a2, '- '), 3(i5, '- '), 'size ', 2(i5, '- '), 'file SEQUENTIEL', i4)") &
                    ltypsrt, cnomvar, ip1, ip2o, ip3o, ni, nj, iun
            endif
        else if (outputFileMode == 5) then
            if (validate_date) then
                call chk_userdate(datev)
            endif

            delta_t = deet * npas / 3600.0
            call incdatr(idatv, idat, delta_t)
        else
            if (message) then
               call app_log(APP_WARNING, 'ecritur: Unknown file')
            endif
        endif
    endif
end
