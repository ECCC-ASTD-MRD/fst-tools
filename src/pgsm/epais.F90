!> difference entre 2 champs de hauteur
subroutine epaisur(iheur, npar, niveau)
    use app
    use packing, only : npack
    use pgsm_mod, only : nwetike, etikent, tmpif1, tmpif2, message, typeent, ip3ent
    use accum, only : npas
    use grilles, only : cgrtyp, gdin, gdout, li, lj, lg1, lg2, lg3, lg4
    use param, only : dat, deet
    use symetry, only : symetri
    implicit none

    !> Field hour
    integer, intent(in) :: iheur
    !> Number of levels
    integer, intent(in) :: npar
    !> Level table
    integer, intent(in) :: niveau(2)

    external ecritur, pgsmabt, imprime, loupsou, fstopc, messags
    integer, external :: fstopc
    integer, external :: ezsint, ezqkdef, ezdefset
    integer, external :: chkenrpos

    ! lire sur fichier standard 2 champs de hauteur
    ! prendre la difference entre les 2 champs ecrire le
    ! resultat sur fichier approprie(standard, ms, seq)

#include "gdz.cdk90"

    character(len = 12) :: cetiket
    character(len = 4) :: cnomvar
    character(len = 2) :: ctypvar
    character(len = 1) :: cigtyp

    integer :: iopc
    logical :: junk

    type(fst_query) :: query
    type(fst_record) :: rec1
    type(fst_record) :: rec2

    ! heure ou iheur dans cette routine ne peut-etre -1 heure(tout) pas valide
    if (iheur == -1) then
        call app_log(APP_ERROR, 'epaisur: HEURE cannot be -1(ALL) with EPAIS directive')
        call pgsmabt
    endif

    ! identifier le numero de chaque record avec fstinf
    call chk_userdate(datev)

    ! modification de hollerith a caractere
    if (etikent(1) /= -1) then
        write(cetiket, '(3A4)') (etikent(i), i=1, nwetike)
    else
        cetiket = '        '
    endif

    if (typeent /= -1) then
        write(ctypvar, '(A2)') typeent
    else
        ctypvar = '  '
    endif

    cigtyp = ' '

    query = file%new_query(datev = datev, etiket = cetiket, ip1 = niveau(1), ip2 = iheur, ip3 = ip3ent, typvar = ctypvar, nomvar = 'GZ')
    if (.not. query%findNext(rec1)) then
        call app_log(APP_ERROR, 'epaisur: Record does not exist in input file')
        return
    endif
    call query%free()

    query = file%new_query(datev = datev, etiket = cetiket, ip1 = niveau(2), ip2 = iheur, ip3 = ip3ent, typvar = ctypvar, nomvar = 'GZ')
    if (.not. query%findNext(rec2)) then
        call app_log(APP_ERROR, 'epaisur: Record does not exist in input file')
        return
    endif
    call query%free()

    if (rec1%nk > 1 .or. rec2%nk) then
        call app_log(APP_ERROR, 'epaisur: PGSM does not accept 3 dimension fields (NK>1)')
        call pgsmabt
    endif

    ! verifier si grille gaussienne ni doit etre pair
    if (rec1%grtyp == 'G' .and. mod(rec1%ni, 2) /= 0) then
        call messags(rec1%ni)
    endif

    allocate(tmpif1(rec1%ni, rec1%nj))
    if ( .not. message) then
        junk = fstopc('TOLRNC', 'DEBUGS', .true. )
    endif

    call chk_userdate(datev)

    if (.not. file%read(rec1, data = c_loc(tmpif1(1, 1))))  then
        call app_log(APP_ERROR, 'epaisur: Failed to read field')
        return
    endif
    call prefiltre(tmpif1, rec1%ni, rec1%nj, rec1%grtyp)

    if (printen) call imprime(rec1%nomvar, tmpif1, rec1%ni, rec1%nj)

    ! verifier si grille gaussienne ni doit etre pair
    if (rec2%grtyp == 'G' .and. mod(rec2%ni, 2) /= 0)  then
        call messags(rec2%ni)
    endif

    allocate(tmpif2(max0(li, rec2%ni), max0(rec2%nj, lj)))
    if ( .not. message)  then
        junk = fstopc('TOLRNC', 'DEBUGS', .true. )
    endif

    call chk_userdate(datev)

    if (.not. file%read(rec2, data = c_loc(tmpif2(1, 1))))  then
        call app_log(APP_ERROR, 'epaisur: Failed to read field')
        return
    endif
    call prefiltre(tmpif2, rec2%ni, rec2%nj, rec2%grtyp)
    if (printen) call imprime(rec2%nomvar, tmpif2, rec2%ni, rec2%nj)

    ! difference entre les deux champs
    call loupsou(tmpif1, tmpif2, rec2%ni * rec2%nj)

    ! interpolation horizontale
    if (rec2%grtyp == 'A' .or. rec2%grtyp == 'B' .or. rec2%grtyp == 'G') then
        if (rec2%ig1 /= 0) junk = symetri(rec2%nomvar)
    endif

    if (cgrtyp == '*') then
        ier = chkenrpos(rec2%ig1, rec2%ig2, rec2%ig3)
        call ecritur(tmpif1, npack, rec2%dateo, rec2%deet, rec2%npas, rec2%ni, rec2%nj, rec2%nk, &
            niveau(1), niveau(2), iheur, rec2%typvar, 'DZ', rec2%etiket, rec2%grtyp, rec2%ig1, rec2%ig2, rec2%ig3, rec2%ig4)
    else
        gdin = ezqkdef(rec2%ni, rec2%nj, rec2%grtyp, rec2%ig1, rec2%ig2, rec2%ig3, rec2%ig4, inputFiles(1)%get_unit())
        ier = ezdefset(gdout, gdin)
        ier = ezsint(tmpif2, tmpif1)

        call ecritur(tmpif2, npack, rec2%dateo, rec2%deet, rec2%npas, li, lj, rec2%nk, &
            niveau(1), niveau(2), iheur, rec2%typvar, 'DZ', rec2%etiket, cgrtyp, lg1, lg2, lg3, lg4)
    endif

    deallocate(tmpif1)
    deallocate(tmpif2)
end
