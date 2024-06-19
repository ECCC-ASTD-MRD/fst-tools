!> interpole difference entre deux champs
subroutine chmpdif (noment, nomsrt, ip1tab, ip2tab, ip3tab, ip1s, ip2s, ip3s)
    use app
    use rmn_fst24
    use packing, only : npack
    use pgsm_mod, only : ier, nwetike, etikent, ip1style, tmplat, message, typeent, printen
    use grilles, only : cgrtyp, gdin, gdout, li, lj, lg1, lg2, lg3, lg4, ngr
    use champs, only : npar
    use files, only : inputFiles
    implicit none

    !> Field name in the input file
    integer, intent(in) :: noment
    !> Field name in the output file (default = -1 -> nomsrt = noment )
    integer, intent(out) :: nomsrt
    !> List of pairs or input field level number
    integer, intent(in) :: ip1tab(40)
    !> List of pairs or input field forecast hour
    integer, intent(in) :: ip2tab(40)
    !> 
    integer, intent(in) :: ip3tab(40)
    integer, intent(in) :: ip1s, ip2s, ip3s

    ! extraire la difference entre deux champs par rapport a ip1, ip2, ou ip3 determine par l'usager:
    !          chmpdif ("gz", "dz", [500, 1000], 6, 0) liste sur ip1
    !          chmpdif ("pr", "pr", 0, [0, 12], 0) liste sur ip2
    !          chmpdif ("tz", "zt", 0, 12, [1, 2, 3, 4] liste sur ip3

    !appel     via directive chmpdif (noment, nomsrt, ip1tab, ip2tab, ip3tab)

    external ecritur
    external loupsou, pgsmabt, grille2
    external imprime, messags
    integer, external :: argdims
    integer, external :: fstopc
    integer, external :: ezqkdef, ezsint, ezdefset
    integer, external :: chkenrpos

    character(len = 12) :: cetiket
    character(len = 2) :: ctypvar
    character(len = 4) :: cnoment, cnomsrt

    integer :: lesips(3), jp(3)
    integer :: lcl_ip1tab(40)
    integer :: num1, num2, num3, nloop
    integer :: i, j, k, ii, jj, kk, iloop, n, datev
    real :: ptr
    character(len = 8) :: string
    integer :: npts

    real, dimension(:), allocatable, target :: lclif1, lclif2

    type(fst_query) :: query
    type(fst_record) :: rec1
    type(fst_record) :: rec2

    if (npar < 5) then
        if (message) then
            call app_log(APP_WARNING, 'chmpdif: Directive CHMPDIF should have at least 5 arguments')
        endif
        return
    endif

    if ( .not. associated(tmplat) .and. cgrtyp /= '*') then
        if (message) then
            call app_log(APP_WARNING, 'chmpdif: Grid not defined, will use PS(2805)')
        endif
        ngr = 8
        call grille2(3, 51, 55, 26., 28., 381000., 350., 1)
    endif

    ! trouver nombre d'arguments dans une liste (ip1, ip2, ip3)
    num1 = argdims(3)
    num2 = argdims(4)
    num3 = argdims(5)

    nloop = 0

    if (num1 > 1) nloop = num1
    if (num2 > 1) nloop = num2
    if (num3 > 1) nloop = num3

    if (nloop .eq. 0) then
        call app_log(APP_WARNING, 'chmpdif: No list of [IP1], [IP2], [IP3]')
        return
    endif

    ! Ajustement pour IP1s reels
    if (argdims(3) > 1) then
        if (ip1tab(1) > 1000000 .and. ip1tab(2) < 0) then
            do i = 1, num1, 2
                ptr = transfer(ip1tab(i), ptr)
                call convip_plus(lcl_ip1tab(i/2+1), ptr, -1*ip1tab(i+1)-1000, ip1style, string, .false. )
            enddo
            num1 = num1 / 2
            nloop = nloop / 2
        else
            do i = 1, num1
                lcl_ip1tab(i) = ip1tab(i)
            enddo
        endif
    else
        do i = 1, num1
            lcl_ip1tab(i) = ip1tab(i)
        enddo
    endif

    ! verifier si il , y a plus d'une liste
    if (num1 > 1 .and. num2 > 1) then
        call app_log(APP_WARNING, 'chmpdif: IP1 and IP2 contain a variable list')
        return
    endif

    if (num1 > 1 .and. num3 > 1) then
        call app_log(APP_WARNING, 'chmpdif: IP1 and IP3 contain a variable list')

        return
    endif

    if (num2 > 1 .and. num3 > 1) then
        call app_log(APP_WARNING, 'chmpdif: IP2 and IP3 contain a variable list')
        return
    endif

    ! execution de chaque paire dans la liste
    do iloop = 1, nloop, 2
        if (num1 > 1) then
            i = iloop
            ii = iloop+1
            j = 1
            jj = 1
            k = 1
            kk = 1
        endif

        if (num2 > 1) then
            j = iloop
            jj = iloop+1
            i = 1
            ii = 1
            k = 1
            kk = 1
        endif

        if (num3 > 1) then
            k = iloop
            kk = iloop+1
            j = 1
            jj = 1
            i = 1
            ii = 1
        endif

        call chk_userdate(datev)

        ! modification de hollerith a caractere
        write(cnoment, '(A4)') noment
        write(cnomsrt, '(A4)') nomsrt

        if (etikent(1) /= -1) then
            write(cetiket, '(3A4)') (etikent(n), n = 1, nwetike)
        else
            cetiket = '        '
        endif

        if (typeent /= -1) then
            write(ctypvar, '(A2)') typeent
        else
            ctypvar = '  '
        endif

        query = inputFiles(1)%new_query(datev = datev, etiket = cetiket, ip1 = lcl_ip1tab(i), ip2 = ip2tab(j), ip3 = ip3tab(k), &
            typvar = ctypvar, nomvar = cnoment)
        if (.not. query%find_next(rec1)) then
            call app_log(APP_WARNING, 'chmpdif: Record does not exist in input file, check NOMVAR, IP1, IP2, IP3')
            return
        endif
        call query%free()

        query = inputFiles(1)%new_query(datev = datev, etiket = cetiket, ip1 = lcl_ip1tab(ii), ip2 = ip2tab(jj), ip3 = ip3tab(kk), &
            typvar = ctypvar, nomvar = cnoment)
        if (.not. query%find_next(rec2)) then
            call app_log(APP_WARNING, 'chmpdif: Record does not exist in input file, check NOMVAR, IP1, IP2, IP3')
            return
        endif
        call query%free()

        if (rec1%nk > 1 .or. rec2%nk > 1) then
            call app_log(APP_ERROR, 'chmpdif: PGSM does not accept 3 dimension fields (NK>1)')
            call pgsmabt
        endif

        ! verifier dimension des deux champs d'entre
        if (rec1%ni /= rec2%ni .or. rec1%nj /= rec2%nj .or. rec1%nk /= rec2%nk) then
            call app_log(APP_WARNING, 'chmpdif: Dimensions of the 2 fields differ, check input file NI, NJ, NK')
            return
        endif

        ! verifier si grille gaussienne ni doit etre pair
        if (rec1%grtyp .eq. 'G' .and. mod(rec1%ni, 2) /= 0) call messags(rec1%ni)

        allocate(lclif1(rec1%ni * rec1%nj))
        if ( .not. message) ier = fstopc('TOLRNC', 'DEBUGS', .true. )
        call chk_userdate(datev)

        if (.not. inputFiles(1)%read(rec1, data = c_loc(lclif1(1))))  then
            call app_log(APP_ERROR, 'chmpdif: Failed to read field')
            return
        endif
        call prefiltre(lclif1, rec1%ni, rec1%nj, rec1%grtyp)
        if (printen) call imprime(rec1%nomvar, lclif1, rec1%ni, rec1%nj)

        ! verifier si grille gaussienne ni doit etre pair
        if (rec2%grtyp .eq. 'G' .and. mod(rec2%ni, 2) /= 0)  call messags(rec2%ni)

        npts = li * lj
        if (npts < rec2%ni * rec2%nj) then
            npts = rec2%ni * rec2%nj
        endif

        allocate(lclif2(npts))
        if ( .not. message) ier = fstopc('TOLRNC', 'DEBUGS', .true. )
        call chk_userdate(datev)

        if (.not. inputFiles(1)%read(rec2, data = c_loc(lclif2(1))))  then
            call app_log(APP_ERROR, 'chmpdif: Failed to read field')
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
            gdin = ezqkdef(rec2%ni, rec2%nj, rec2%grtyp, rec2%ig1, rec2%ig2, rec2%ig3, rec2%ig4, inputFiles(1)%get_unit())
            ier = ezdefset(gdout, gdin)
            ier = ezsint(lclif2, lclif1)
        endif

        ! ecrire le ip1, ip2, ip3 correspondant aux definitions
        if (num1 > 1) then
            jp(1) = lcl_ip1tab(i)
            jp(2) = lcl_ip1tab(ii)
            jp(3) = ip2tab(j)
        endif

        if (num2 > 1) then
            jp(1) = lcl_ip1tab(i)
            jp(2) = ip2tab(j)
            jp(3) = ip2tab(jj)
        endif

        if (num3 > 1) then
            jp(1) = lcl_ip1tab(i)
            jp(2) = ip3tab(k)
            jp(3) = ip3tab(kk)
        endif

        if (cnomsrt .eq. '    ') then
            cnomsrt = cnoment
        endif

        lesips(1) = ip1s
        lesips(2) = ip2s
        lesips(3) = ip3s

        do i = 1, 3
            if (lesips(i) .eq. 65001) then
                jp(i) = rec1%ip1
            endif

            if (lesips(i) .eq. 65002) then
                jp(i) = rec2%ip1
            endif

            if (lesips(i) .eq. 65003) then
                jp(i) = rec1%ip2
            endif

            if (lesips(i) .eq. 65004) then
                jp(i) = rec2%ip2
            endif

            if (lesips(i) .eq. 65005) then
                jp(i) = rec1%ip3
            endif

            if (lesips(i) .eq. 65006) then
                jp(i) = rec2%ip3
            endif
        enddo

        ! ecrire sur fichier standard, ms, sequentiel
        if (cgrtyp .eq. '*') then
            call ecritur(lclif1, npack, rec2%dateo, rec2%deet, rec2%npas, rec2%ni, rec2%nj, rec2%nk, jp(1), jp(2), jp(3), &
                rec2%typvar, cnomsrt, rec2%etiket, rec2%grtyp, rec2%ig1, rec2%ig2, rec2%ig3, rec2%ig4)
        else
            call ecritur(lclif2, npack, rec2%dateo, rec2%deet, rec2%npas, li, lj, 1, jp(1), jp(2), jp(3), &
                rec2%typvar, cnomsrt, rec2%etiket, cgrtyp, lg1, lg2, lg3, lg4)
        endif

        ! remetre espace des champs de travail
        deallocate(lclif2)
        deallocate(lclif1)
    enddo
end
