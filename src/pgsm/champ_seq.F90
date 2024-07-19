!> Miroir de la directive champ pour fichiers sequentiels
subroutine champ_seq (listn, listip1, waitOrGo)
    use app
    use rmn_fst24
    use files
    use packing, only : npack
    use pgsm_mod, only : ier, ip1style, message, tmpif1, tmpif2, tmplat
    use accum, only : npas
    use grilles, only : cgrtyp, gdin, gdout, lg1, lg2, lg3, lg4, li, lj
    use heuress, only : heures, nhur
    use symetry, only : symetri
    use champs, only : npar
    use champseq, only : wait
    implicit none

    !> Liste de nomvar
    integer, dimension(*), intent(in) :: listn
    !> Liste de niveau
    integer, dimension(*), intent(in) :: listip1
    !> Commutateur d'accumulation de directives
    integer, intent(in) :: waitOrGo

    !> Appel via directive champ_seq(listn, listip1, waitOrGo)

    integer, external :: argdims
    external ecritur, pgsmabt, grille2
    integer, external :: ezqkdef, ezsint, ezdefset

    integer, parameter :: nmaxlist1 = 16
    integer, parameter :: nmaxlist2 = 16

    character(len = 4), save :: listnom(nmaxlist1, nmaxlist2) = '    '
    integer, save :: listniv(nmaxlist1, nmaxlist2) = -1
    integer, save :: ntitems = 0
    integer, save :: nitems1(nmaxlist1) = 0
    integer, save :: nitems2(nmaxlist2) = 0

    logical :: heureok, processed
    character(len = 8) :: string
    real :: ptr
    integer :: i, j, k

    type(fst_record) :: record
    type(fst_query) :: query

    if (npar /= 3) then
        if (message) then
            call app_log(APP_ERROR, 'champ_seq: Directive CHAMP_SEQ should have 3 arguments')
        endif
        return
    endif

    if (.not. associated(tmplat)) then
        if (message) then
            call app_log(APP_WARNING, 'champ_seq: Grid not defined, will use PS(2805)')
        endif
        call grille2(3, 51, 55, 26., 28., 381000., 350., 1)
    endif

    !   trouver nombre d'arguments dans une liste (ip1, ip2, ip3)
    ntitems = ntitems + 1
    if (ntitems > nmaxlist1) then
        call app_log(APP_ERROR, 'champ_seq: Limit of 16 directives CHAMP_SEQ has been passed')
        call pgsmabt
    endif

    if (argdims(1) > nmaxlist2) then
        call app_log(APP_ERROR, 'champ_seq: Limit of 16 variable names has been passed')
        call pgsmabt
    endif

    if (argdims(2) > nmaxlist2) then
        call app_log(APP_ERROR, 'champ_seq: Limit of 16 vertical levels has been passed')
        call pgsmabt
    endif

    nitems1(ntitems) = argdims(1)
    nitems2(ntitems) = argdims(2)
    do i = 1, argdims(1)
        write(listnom(ntitems, i), '(A2)') listn(i)
    enddo

    do i = 1, argdims(2)
        listniv(ntitems, i) = listip1(i)
    enddo

    if (listniv(ntitems, 1) > 1000000 .and. listniv(ntitems, 2)  < 0) then
        do i = 1, argdims(2), 2
            ptr = transfer(listniv(ntitems, i), ptr)
            call convip_plus(listniv(ntitems, i / 2 + 1), ptr, -1 * listniv(ntitems, (i + 1)) - 1000, ip1style, string, .false.)
         enddo
         nitems2(ntitems) = argdims(2) / 2
    endif

    if (waitOrGo == WAIT) return

    ier = inputFiles(1)%rewind()
    if (ier > 1) then
        call app_log(APP_ERROR, 'champ_seq: Rewinding file has failed!')
        call pgsmabt
    endif

    query = inputFiles(1)%new_query()

    do while (query%find_next(record))
        processed = .false.
        if (record%nk > 1) then
            call app_log(APP_ERROR, 'champ_seq: PGSM does not allow for 3 dimension fields (NK > 1)')
            call pgsmabt
        endif

        heureok = .false.
        if (heures(1) == -1) then
            heureok = .true.
        else
            do k = 1, nhur
                if (record%ip2 == heures(k)) then
                    heureok = .true.
                endif
            enddo
        endif
        npas = record%npas

        if (heureok .and. .not. processed) then
            do i = 1, ntitems
                if (.not. processed) then
                    do j = 1, nitems1(i)
                        if (listnom(i, j) == record%nomvar .or. listnom(i, j)  == ' ' .and. .not. processed) then
                            do k = 1, nitems2(i)
                                if (listniv(i, k) == record%ip1 .or. listniv(i, k) == -1 .and. .not. processed) then
                                    allocate(tmpif1(record%ni, record%nj))
                                    allocate(tmpif2(li, lj))
                                    if (.not. inputFiles(1)%read(record, data = c_loc(tmpif1(1, 1))))  then
                                        call app_log(APP_ERROR, 'champ_seq: Failed to read field')
                                        return
                                    endif
                                    call prefiltre(tmpif1, record%ni, record%nj, record%grtyp)

                                    gdin = ezqkdef(record%ni, record%nj, record%grtyp, record%ig1, record%ig2, record%ig3, record%ig4, 1)
                                    ier = ezdefset(gdout, gdin)
                                    ier = ezsint(tmpif2, tmpif1)

                                    call ecritur(tmpif2, npack, record%dateo, record%deet, record%npas, li, lj, 1, &
                                        record%ip1, record%ip2, record%ip3, &
                                        record%typvar, record%nomvar, record%etiket, &
                                        cgrtyp, lg1, lg2, lg3, lg4)

                                    deallocate(tmpif2)
                                    deallocate(tmpif1)
                                    processed = .true.
                                endif
                            enddo
                        endif
                    enddo
                endif
            enddo
        endif
    enddo
    call query%free()

    ! l'interpolation est terminee - On a passé a travers le fichier
    do i = 1, ntitems
        do j = 1, nitems2(i)
            listnom(i, j) = '  '
            listniv(i, j) = -1
        enddo
        nitems2(i) = 0
    enddo
    ntitems = 0
end
