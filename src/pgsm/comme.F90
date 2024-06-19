!> Lire un champ dans accumulateur
subroutine comme(iunit, nom, type, idat, ip1, ip2, ip3, etiqet)
    use app
    use rmn_fst24
    use accum, only : nni, nnj, nnk, idatt, ideet, npas, jpp1, jpp2, jpp3, igg1, igg2, igg3, igg4
    use pgsm_mod, only : tmplat, tmplon, message
    use grilles, only : cgrtyp, cgtypxy, gdout, lg1, lg2, lg3, lg4, li, lj
    use files, only : fentree, fsortie, inputFiles, outputFile
    implicit none

    !> File in which to search for the grid. (FENTREE|FSORTIE)
    integer, intent(in) :: iunit
    !> Nomvar
    integer, intent(in) :: nom
    !> Typvar
    integer, intent(in) :: type
    !> Dateo
    integer, intent(in) :: idat
    integer, intent(in) :: ip1
    integer, intent(in) :: ip2
    integer, intent(in) :: ip3
    !> Label
    integer, intent(in) :: etiqet(3)

    external pgsmabt
    integer, external :: fstopc
    integer, external :: fstcvt
    integer, external :: ezqkdef, ezgxprm, gdll, argdims

#include "blancs.cdk90"

    integer :: ig1ref, ig2ref, ig3ref, ig4ref

    character(len = 12) :: cetiqet
    character(len = 4) :: cnomvar
    character(len = 4) :: cbidon
    character(len = 2) :: ctypvar
    integer :: i, bidon, ier, iopc

    integer :: letiket(3)

    type(fst_file), pointer :: file => null()
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

    ! Modification de hollerith a caractere
    cnomvar = '    '
    ctypvar = '  '
    cetiqet = '            '

    letiket(1) = etiqet(1)
    letiket(2) = blancs
    letiket(3) = blancs
    if (argdims(8) > 1) then
        letiket(2) = etiqet(2)
    endif
    if (argdims(8) > 2) then
        letiket(3) = etiqet(3)
    endif

    ier = fstcvt(nom, type, letiket, bidon, cnomvar, ctypvar, cetiqet, cbidon, .true.)
    if (etiqet(1) /= -1) then
        write(cetiqet, '(3A4)') (etiqet(i), i=1, argdims(9))
    else
        cetiqet = '            '
    endif

    if (cnomvar == '    ' .and. ctypvar .eq. '  ' .and. cetiqet == '            ' .and. ip1 == -1 .and. ip2 == -1 .and. ip3 == -1 .and. idat == -1) then
        call app_log(APP_ERROR, 'comme: Selection parameters too vague')
        call pgsmabt
    endif

    query = file%new_query(nomvar = cnomvar, typvar = ctypvar, datev = idat, ip1 = ip1, ip2 = ip2, ip3 = ip3, etiket = cetiqet)
    if (query%find_next(record)) then
        call app_log(APP_ERROR, 'comme: Record does not exist')
        call pgsmabt
    endif
    call query%free()
    nni = record%ni
    nnj = record%nj
    nnk = record%nk
    if (nnk > 1) then
        call app_log(APP_ERROR, 'comme: PGSM does not accept 3 dimension fields (NK>1)')
        call pgsmabt
    endif
    idatt = record%dateo
    ideet = record%deet
    npas = record%npas
    jpp1 = record%ip1
    jpp2 = record%ip2
    jpp3 = record%ip3
    igg1 = record%ig1
    igg2 = record%ig2
    igg3 = record%ig3
    igg4 = record%ig4

    if (record%grtyp /= 'Z' .and. record%grtyp /= 'Y') then
        gdout = ezqkdef(nni, nnj, record%grtyp, igg1, igg2, igg3, igg4, iunit)
        ier = ezgxprm(gdout, li, lj, cgrtyp, lg1, lg2, lg3, lg4, cgtypxy, ig1ref, ig2ref, ig3ref, ig4ref)
        allocate(tmplon(li, lj))
        allocate(tmplat(li, lj))
    else
        if (iunit == fentree) then
            call gritp12(7, igg1, igg2, igg3)
        else
            call gritp12(8, igg1, igg2, igg3)
        endif
    endif

    if ( .not. message) iopc = fstopc('TOLRNC', 'DEBUGS', .true. )
end
