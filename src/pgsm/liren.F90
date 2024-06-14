!> Lire un champ dans accumulateur
subroutine liren_orig(nom, typvar, idat, niv, ihr, ip3, etiqet, iunit)
    use app
    use rmn_fst24
    use accum, only: idatt, ideet, npas, nni, nnj, nnk, jpp1, jpp2, jpp3, ctypv, cnumv, cetik, cigty, igg1, igg2, igg3, igg4
    use pgsm_mod, only : ip1style, tmpif0, message
    use chck, only : ichck
    implicit none

    !> Nomvar
    integer, intent(in) :: nom
    !> Typvar (a = analysis, p = forecast)
    integer, intent(in) :: typvar
    !> Level
    integer, intent(in) :: niv(2)
    !> Forecast hour
    integer, intent(in) :: ihr
    !> IP3
    integer, intent(in) :: ip3
    !> Label
    integer, intent(in) :: etiqet(3)
    !> File in which to search for the grid. (FENTREE|FSORTIE)
    integer, intent(in) :: iunit

    external pgsmabt, imprime, messags
    integer, external :: argdims, fstopc, fstcvt

    !APPEL     VIA DIRECTIVE
    !         LIREE(NOM, TYPE, IDAT, NIV, IHR, IP3, ETIQUET)
    !         LIRES(NOM, TYPE, IDAT, NIV, IHR, IP3, ETIQUET)

#include "blancs.cdk90"

    character(len = 12) :: cetiqet
    character(len = 4) :: cnomvar
    character(len = 2) :: ctypvar
    character(len = 1) :: cigtyp
    integer :: iip3
    integer :: iopc
    integer :: letiket(3)
    integer :: lniv
    character(len = 8) :: fmtstr

    type(fst_file), pointer :: file
    type(fst_query) :: query
    type(fst_record) :: record

    if (fentree == iunit) then
        file = inputFiles(1)
    else if (fsortie == iunit) then
        file = outputFile
    else
        call app_log(APP_ERROR, 'comme: Unrecognized file! Must be FENTREE or FSORTIE')
        call pgsmabt
    end if

    iip3 = ip3
    if (ip3 .eq. 4095) iip3 = -1

    ! Modification de hollerith a caractere
    cnomvar = '    '
    ctypvar = '  '
    cetiqet = '            '
    cigtyp  = ' '

    letiket(1) = etiqet(1)
    letiket(2) = blancs
    letiket(3) = blancs
    if (argdims(7) .gt. 1) then
        letiket(2) = etiqet(2)
    endif
    if (argdims(7) .gt. 2) then
        letiket(3) = etiqet(3)
    endif

    lniv = niv(1)
    if (argdims(4) > 1) then
        p = transfer(niv(1), p)
        call convip_plus(lniv, p, -1*niv(2)-1000, ip1style, fmtstr, .false.)
    endif

    ier = fstcvt(nom, typvar, letiket, -1, cnomvar, ctypvar, cetiqet, cigtyp, .true.)

    if (iunit) then
        query = inputFiles(1)%new_query(datev = idat, etiket = cetiqet, ip1 = lniv, ip2 = ihr, ip3 = iip3, typvar = ctypvar, nomvar = cnomvar)
    else
        query = outputFile%%new_query(datev = idat, etiket = cetiqet, ip1 = lniv, ip2 = ihr, ip3 = iip3, typvar = ctypvar, nomvar = cnomvar)
    end if
    if (query%find_next(record)) then
        call app_log(APP_ERROR, 'liren: Record does not exist')
        call pgsmabt
    endif
    call query%free()

    !  #  clef pour directive pluse, moinse, ecrits....
    ichck = 1

    idatt = record%dateo
    ideet = record%deet
    npas = record%npas
    nni = record%ni
    nnj = record%nj
    nnk = record%nk
    jpp1 = record%ip1
    jpp2 = record%ip2
    jpp3 = record%ip3
    ctypv = record%typvar
    cnumv = record%nomvar
    cetik = record%etiket
    cigty = record%grtyp
    igg1 = record%ig1
    igg2 = record%ig2
    igg3 = record%ig3
    igg4 = record%ig4

    ! Verifier si grille gaussienne ni doit etre pair
    if (record%grtyp .eq. 'G' .and. mod(nni, 2) .ne. 0) call messags(nni)

    allocate(tmpif0(nni, nnj))

    if ( .not. message) iopc = fstopc('TOLRNC', 'DEBUGS', .true.)
    if (record%read(data = c_loc(tmpif0(1, 1)))) then
        call app_log(APP_ERROR, 'liren: Record does not exist')
        call pgsmabt
    endif
    call prefiltre(tmpif0, record%ni, record%nj, record%grtyp)

    if (printen) call imprime(cnomvar, tmpif0, nni, nnj)

    icnt = 1
    if (ip3 .eq. 4095) icnt = jpp3
end subroutine


subroutine lirsr(nom, typvar, idat, niv, ihr, ip3, etiqet)
    implicit none

    call liren_orig(nom, typvar, idat, niv, ihr, ip3, etiqet, .true.)
end subroutine


subroutine lirsr(nom, typvar, idat, niv, ihr, ip3, etiqet)
    implicit none

    call liren_orig(nom, typvar, idat, niv, ihr, ip3, etiqet, .false.)
end subroutine
