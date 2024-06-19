!> Extraire la difference entre deux champs dont les heures sont differents
subroutine macpcp(cnom, npar, itime)
    use app
    use packing, only : npack
    use pgsm_mod, only : nwetike, etikent, message, typeent, ip3ent
    use accum, only : npas
    use grilles, only : cgrtyp, gdin, gdout, li, lj, lg1, lg2, lg3, lg4
    use param, only : dat, deet
    use symetry, only : symetri
    implicit none

    !> Nom du champ
    character(len = 4), intent(in) :: cnom
    !> Nombre de locations utilisees dans itime
    integer, intent(in) :: npar
    !> Table contenant 2 heures ou niveaux
    integer, dimension(2), intent(in) :: itime

    external ecritur
    external loupneg, loupsou, pgsmabt, imprime, messags
    integer, external :: ezqkdef, ezdefset, ezsint, chkenrpos, datev
    integer, external :: fstopc
    logical, external :: symetri

    character(len = 12) :: cetiket
    character(len = 2) :: ctypvar

    real, dimension(:), allocatable :: lclif1, lclif2

    integer :: i
    integer :: junk
    logical :: sym

    type(fst_query) :: query
    type(fst_record) :: rec1
    type(fst_record) :: rec2

    if (npar .ne. 2) then
        if (message) call app_log(APP_ERROR, 'macpcp: Wrong call to MACPCP, must have 3 arguments')
        return
    endif

    ! # doit etre egal a zero dans fichier d'ENTRE
    call chk_userdate(datev)

    ! modification de hollerith a caractere
    if (etikent(1) .ne. -1) then
        write(cetiket, '(3A4)') (etikent(i), i=1, nwetike)
    else
        cetiket = '            '
    endif

    if (typeent .ne. -1) then
        write(ctypvar, '(A2)') typeent
    else
        ctypvar = '  '
    endif

    query = file%new_query(datev = datev, etiket = cetiket, ip1 = 0, ip2 = itime(1), ip3 = ip3ent, typvar = ctypvar, nomvar = cnom)
    if (.not. query%findNext(rec1)) then
        call app_log(APP_ERROR, 'macpcp: Record does not exist in input file')
        return
    endif
    call query%free()

    query = file%new_query(datev = datev, etiket = cetiket, ip1 = 0, ip2 = itime(2), ip3 = ip3ent, typvar = ctypvar, nomvar = cnom)
    if (.not. query%findNext(rec2)) then
        call app_log(APP_ERROR, 'macpcp: Record does not exist in input file')
        return
    endif
    call query%free()

    if (rec1%nk > 1 .or. rec2%nk > 1) then
        call app_log(APP_ERROR, 'macpcp: PGSM does not accept 3 dimension fields (NK>1)')
        call pgsmabt
    endif

    ! verifier si grille gaussienne ni doit etre pair
    if (rec1%grtyp .eq. 'G' .and. mod(rec1%ni, 2) .ne. 0) call messags(rec1%ni)

    allocate(lclif1(rec1%ni * rec1%nj))
    if ( .not. message) junk = fstopc('TOLRNC', 'DEBUGS', .true. )

    if (.not. file%read(rec1, data = c_loc(lclif1(1, 1))))  then
        call app_log(APP_ERROR, 'macpcp: Failed to read field')
        return
    endif
    call prefiltre(lclif1, rec1%ni, rec1%nj, rec1%grtyp)

    if (printen) call imprime(rec1%nomvar, lclif1, rec1%ni, rec1%nj)

    ! verifier si grille gaussienne ni doit etre pair
    if (rec2%grtyp .eq. 'G' .and. mod(rec2%ni, 2) .ne. 0) call messags(rec2%ni)

    allocate(lclif2(max0(li * lj, rec2%ni * rec2%nj)))
    if ( .not. message) junk = fstopc('TOLRNC', 'DEBUGS', .true. )

    if (.not. file%read(rec2, data = c_loc(lclif2(1, 1))))  then
        call app_log(APP_ERROR, 'macpcp: Failed to read field')
        return
    endif
    call prefiltre(lclif2, rec2%ni, rec2%nj, rec2%grtyp)

    if (printen) call imprime(rec2%nomvar, lclif2, rec2%ni, rec2%nj)

    ! difference entre les deux champs
    call loupsou(lclif1, lclif2, rec2%ni * rec2%nj)

    ! interpolation horizontale
    if (cgrtyp .eq. '*') then
        ier = chkenrpos(rec2%ig1, rec2%ig2, rec2%ig3)
    else
        if (rec2%grtyp == 'A' .or. rec2%grtyp == 'B' .or. rec2%grtyp == 'G') then
            if (rec2%ig1 /= 0) sym = symetri(rec2%nomvar)
        endif
        gdin = ezqkdef(rec2%ni, rec2%nj, rec2%grtyp, rec2%ig1, rec2%ig2, rec2%ig3, rec2%ig4, inputFiles(1)%get_unit())
        ier = ezdefset(gdout, gdin)
        ier = ezsint(lclif2, lclif1)
    endif

    !     #   jp1 - contient heure du premier champ
    !     #   jp2 - contient heure du deuxieme champ
    jp1 = itime(1)
    jp2 = itime(2)
    jp3 = 0

    ! deet et npas contiennent les dernieres valeurs lues dans le dernier record

    ! eliminer toutes les valeurs du champ negative precip et acumulateur d'ajustement ne peuvent etre negatif
    call loupneg(lclif2, 0.0, 1, li * lj, 1)

    ! ecrire sur fichier standard, ms, sequentiel
    if (cgrtyp .eq. '*') then
        call ecritur(lclif1, npack, rec2%dateo, rec2%deet, rec2%npas, rec2%ni, rec2%nj, rec2%nk, rec2%ip1, rec2%ip2, rec2%ip3, &
            rec2%typvar, rec2%nomvar, rec2%etiket, rec2%grtyp, rec2%ig1, rec2%ig2, rec2%ig3, rec2%ig4)
    else
        call ecritur(lclif2, npack, rec2%dateo, rec2%deet, rec2%npas, li, lj, rec2%nk, rec2%ip1, rec2%ip2, rec2%ip3, &
            rec2%typvar, rec2%nomvar, rec2%etiket, cgrtyp, lg1, lg2, lg3, lg4)
    endif

    deallocate(lclif2)
    deallocate(lclif1)
end
