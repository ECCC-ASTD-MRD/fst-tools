integer function chkenrpos(ip1, ip2, ip3)
    use rmn_fst24
    use files, only : inputFiles, outputFile, outputFileMode
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

        if (chkenrpos == 1 .and. tac_found >= 0) then
            ! \todo Comparing record keys makes no sense; what is a sensible equivalent
            if (yy_found > tic_found .and. yy_found > tac_found) then
                chkenrpos = 0
            endif
        endif

        if (chkenrpos >= 0) return
    endif

    tic_query = inputFiles(1)%new_query(ip1 = ip1, ip2 = ip2, ip3 = ip3, nomvar = '>>  ')
    tic_found = tic_query%find_count()
    call tic_query%rewind()
    tac_query = inputFiles(1)%new_query(ip1 = ip1, ip2 = ip2, ip3 = ip3, nomvar = '^^  ')
    tac_found = tac_query%find_count()
    call tac_query%rewind()
    yy_query = inputFiles(1)%new_query(ip1 = ip1, ip2 = ip2, ip3 = ip3, nomvar = '^>  ')
    yy_found = yy_query%find_count()
    call yy_query%rewind()

    if (tic_found < 0 .or. tac_found < 0) then
        chkenrpos = -1
    else
        chkenrpos = 0
    endif

    if (yy_found > 0) then
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
        if (.not. yy_query%read_next(yy)) then
            call app_log(APP_ERROR, 'chkenrpos: Failed to read record')
            call pgsmabt
        end if

        if (.not. outputFile%write(yy)) then
            call app_log(APP_ERROR, 'chkenrpos: Failed to write record')
            call pgsmabt
        end if
        call yy%free()
    else
        if (.not. tic_query%read_next(tic)) then
            call app_log(APP_ERROR, 'chkenrpos: Failed to read record')
            call pgsmabt
        end if
        if (.not. tac_query%read_next(tac)) then
            call app_log(APP_ERROR, 'chkenrpos: Failed to read record')
            call pgsmabt
        end if

        if (.not. outputFile%write(tic)) then
            call app_log(APP_ERROR, 'chkenrpos: Failed to write record')
            call pgsmabt
        end if
        if (.not. outputFile%write(tac)) then
            call app_log(APP_ERROR, 'chkenrpos: Failed to write record')
            call pgsmabt
        end if
        call tic%free()
        call tac%free()
    endif
    call tic_query%free()
    call tac_query%free()
    call yy_query%free()
end
