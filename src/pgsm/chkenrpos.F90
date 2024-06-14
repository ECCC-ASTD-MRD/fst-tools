integer function chkenrpos(ip1, ip2, ip3)
    use rmn_fst24
    use files, only : inputFiles, outputFile
    implicit none

    integer, intent(in) :: ip1
    integer, intent(in) :: ip2
    integer, intent(in) :: ip3

    logical :: yinyang_grid

    type(fst_query) :: tic_query
    type(fst_query) :: tac_query
    type(fst_query) :: yy_query
    type(fst_record) :: tic
    type(fst_record) :: tac
    type(fst_record) :: yy

    integer :: tic_found = 0
    integer :: tac_found = 0
    integer :: yy_found = 0

    yinyang_grid = .false.
    chkenrpos = -1

    if (outputFileMode == 5) then
        chkenrpos = 0
        return
    endif

    if (outputFileMode == 1) then
        tic_query = outputFile%new_query(ip1 = ip1, ip2 = ip2, ip3 = ip3, nomvar = '>>  ')
        tic_found = tic_query%find_count()
        call tic_query%free()
        tac_query = outputFile%new_query(ip1 = ip1, ip2 = ip2, ip3 = ip3, nomvar = '^^  ')
        tac_found = tac_query%find_count()
        call tac_query%free()

        if (tic_found > 0 .and. tac_found > 0) then
            chkenrpos = 0
        endif

        yy_query = outputFile%new_query(ip1 = ip1, ip2 = ip2, ip3 = ip3, nomvar = '^>  ')
        yy_found = yy_query%find_count()
        call yy_query%free()
        if (yy_found > 0) then
            chkenrpos = 1
        endif

        if (chkenrpos == 1 .and. tac_count >= 0) then
            ! \todo Comparing record keys makes no sense; what is a sensible equivalent
            if (yy_found > tic_found .and. yy_found > tac_found) then
                chkenrpos = 0
            endif
        endif

        if (chkenrpos >= 0) return
    endif

    tic_query = inputFile(1)%new_query(ip1 = ip1, ip2 = ip2, ip3 = ip3, nomvar = '>>  ')
    tic_found = tic_query%find_count()
    tac_query = inputFile(1)%new_query(ip1 = ip1, ip2 = ip2, ip3 = ip3, nomvar = '^^  ')
    tac_found = tac_query%find_count()
    yy_query = inputFile(1)%new_query(ip1 = ip1, ip2 = ip2, ip3 = ip3, nomvar = '^>  ')
    yy_found = yy_query%find_count()

    if (tic_found < 0 .or. tac_found < 0) then
        chkenrpos = -1
    else
        chkenrpos = 0
    endif

    if (yy_found >= 0) then
        chkenrpos = 1
        yinyang_grid = .true.
    endif

    if (chkenrpos == 1 .and. tic_found >= 0) then
        !> \todo This isn't a direct equivalent to what's used to be here, but the old code didn't make sense since it compared record handle values
        if (yy_found > tic_found .or. yy_found > tac_found) then
            chkenrpos = 0
            yinyang_grid = .false.
        endif
    endif
    if (chkenrpos == -1) return

    if (yinyang_grid) then
        yy_query%read_next(yy)

        call ecritur(yy%data, -yy%nbits, yy%dateo, yy%deet, yy%npas, yy%ni, yy%nj, yy%nk, &
            yy%ip1, yy%ip2, yy%ip3, yy%typvar, yy%nomvar, yy%etiket, &
            yy%grtyp, yy%ig1, yy%ig2, yy%ig3, yy%ig4)

        call yy%free()
    else
        tic_query%read_next(tic)
        tac_query%read_next(tac)

        call ecritur(tic%data, -tic%nbits, tic%dateo, tic%deet, tic%npas, tic%ni, tic%nj, tic%nk, &
            tic%ip1, tic%ip2, tic%ip3, tic%typvar, tic%nomvar, tic%etiket, &
            tic%grtyp, tic%ig1, tic%ig2, tic%ig3, tic%ig4)
        call ecritur(tac%data, -tac%nbits, tac%dateo, tac%deet, tac%npas, tac%ni, tac%nj, tac%nk, &
            tac%ip1, tac%ip2, tac%ip3, tac%typvar, tac%nomvar, tac%etiket, &
            tac%grtyp, tac%ig1, tac%ig2, tac%ig3, tac%ig4)

        call tic%free()
        call tac%free()
    endif
    call tic_query%free()
    call tac_query%free()
    call yy_query%free()
end
