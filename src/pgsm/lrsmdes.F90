!> Chaque pt d'un champ lu est mi au carre dans accumulateur
subroutine lrsmde_orig(nom, type, idat, niv, ihr, ip3, etiqet, iunit)
    use app
    use rmn_fst24
    use accum, only : nni, nnj, nnk, idatt, ideet, npas, jpp1, jpp2, jpp3, igg1, igg2, igg3, igg4, cnumv, ctypv, cetik, cigty, icnt
    use pgsm_mod, only : ier, ip1style, message, tmpif0, printen
    use files, only : fentree, fsortie, inputFiles, outputFile
    use chck, only : ichck
    implicit none

    !> Nomvar
    integer, intent(in) :: nom
    !> Typvar
    integer, intent(in) :: type
    !> Dateo
    integer, intent(in) :: idat
    !> Ip1
    integer, intent(in) :: niv(2)
    !> Ip2
    integer, intent(in) :: ihr
    !> Ip3
    integer, intent(in) :: ip3
    !> Label
    integer, intent(in) :: etiqet(3)
    !> File from which to read (fentree|fsortie)
    integer, intent(in) :: iunit

    ! lire un champ sur fichier d'entre ou de sorti et sauve dans
    ! l'accumulateur chaque pt au carre et les directives suivantes
    ! lirmode ou lirmods ajouteront chaque champ(pt au carre) a
    ! l'accumulateur la somme des champs est garder dans l'accumulateur
    ! et peut etre sauve par la directive ecrits.

    !APPEL   VIA DIRECTIVE
    !        LIRMDE(NOM, TYPE, IDAT, NIV, IHR, IP3, ETIQET)
    !        LIRMDS(NOM, TYPE, IDAT, NIV, IHR, IP3, ETIQET)

#include "blancs.cdk90"

    external pgsmabt, imprime, messags
    integer, external :: argdims, fstopc, fstcvt

    character(len = 12) :: cetiket
    character(len = 4) :: cnomvar
    character(len = 1) :: cbidon
    character(len = 2) :: ctypvar

    integer :: iip3
    integer :: i, j, iopc, lniv
    integer :: letiket(3)
    real :: p
    character(len = 8) :: string

    type(fst_file), pointer :: file
    type(fst_query) :: query
    type(fst_record) :: record

    file => null()
    if (fentree == iunit) then
        file = inputFiles(1)
    else if (fsortie == iunit) then
        file = outputFile
    else
        call app_log(APP_ERROR, 'lrsmdes: Unrecognized file! Must be FENTREE or FSORTIE')
        call pgsmabt
    end if

    if (ip3 == 4095) then
        iip3 = -1
    else
        iip3 = ip3
    end if

    ! MODIFICATION DE HOLLERITH A CARACTERE
    cnomvar = '    '
    ctypvar = '  '
    cetiket = '            '

    letiket(1) = etiqet(1)
    letiket(2) = blancs
    letiket(3) = blancs
    if (argdims(7) > 1) then
        letiket(2) = etiqet(2)
    endif
    if (argdims(7) > 2) then
        letiket(3) = etiqet(3)
    endif

    if (argdims(4) > 1) then
        lniv = niv(1)
        p = transfer(niv(1), p)
        call convip_plus(lniv, p, -1 * niv(2) - 1000, ip1style, string, .false.)
    endif

    !> \bug When this subroutine still contain an entry, all the lines above were skipped when called from lrsmds
    ier = fstcvt(nom, type, letiket , -1, cnomvar, ctypvar, cetiket, cbidon, .true.)

    query = file%new_query(nomvar = cnomvar, typvar = ctypvar, datev = idat, ip1 = lniv, ip2 = ihr, ip3 = ip3, etiket = cetiket)
    if (query%find_next(record)) then
        call app_log(APP_ERROR, 'lrsmde: Record does not exist')
        call pgsmabt
    endif
    call query%free()

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

    ! VERIFIER SI GRILLE GAUSSIENNE NI DOIT ETRE PAIR
    if (record%grtyp == 'G' .and. mod(nni, 2) .ne. 0) call messags(nni)

    if (.not. message) iopc= fstopc('TOLRNC', 'DEBUGS', .true.)

    allocate(tmpif0(nni, nnj))

    icnt = 1
    if (ip3 == 4095) icnt = jpp3

    if (.not. file%read(record, data = c_loc(tmpif0(1, 1))))  then
        call app_log(APP_ERROR, 'lrsmde: Record does not exist')
        call pgsmabt
    endif
    call prefiltre(tmpif0, record%ni, record%nj, record%nomvar, record%grtyp)
    if (printen) call imprime(cnomvar, tmpif0, nni, nnj)

    do j = 1, nnj
        do i = 1, nni
            tmpif0(i, j) = tmpif0(i, j) * tmpif0(i, j)
        enddo
    enddo
end subroutine lrsmde_orig


subroutine lrsmds(nom, type, idat, niv, ihr, ip3, etiqet)
    use files, only : fsortie
    implicit none

    !> Nomvar
    integer, intent(in) :: nom
    !> Typvar
    integer, intent(in) :: type
    !> Dateo
    integer, intent(in) :: idat
    !> Ip1
    integer, intent(in) :: niv(2)
    !> Ip2
    integer, intent(in) :: ihr
    !> Ip3
    integer, intent(in) :: ip3
    !> Label
    integer, intent(in) :: etiqet(3)

    call lrsmde_orig(nom, type, idat, niv, ihr, ip3, etiqet, fsortie)
end subroutine lrsmds


subroutine lrsmde(nom, type, idat, niv, ihr, ip3, etiqet)
    use files, only : fentree
    implicit none

    !> Nomvar
    integer, intent(in) :: nom
    !> Typvar
    integer, intent(in) :: type
    !> Dateo
    integer, intent(in) :: idat
    !> Ip1
    integer, intent(in) :: niv(2)
    !> Ip2
    integer, intent(in) :: ihr
    !> Ip3
    integer, intent(in) :: ip3
    !> Label
    integer, intent(in) :: etiqet(3)

    call lrsmde_orig(nom, type, idat, niv, ihr, ip3, etiqet, fentree)
end subroutine lrsmde
