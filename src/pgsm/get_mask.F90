integer function get_mask(mask, fld) result(status)
    use app
    use rmn_fst24
    use files, only : inputFiles
    implicit none

    type (fst_record), intent(out) :: mask
    type (fst_record), intent(in) :: fld

    integer :: ier

    type(fst_query) :: query

    status = -1

    if (fld%typvar(2:2) /= '@') then
        write(app_msg, *) 'get_mask: This is not a masked field', fld%nomvar, fld%typvar
        call app_log(APP_ERROR, app_msg)
        return
    endif

    if (fld%typvar(1:1) == '@') then
        write(app_msg, *) 'get_mask: This is a mask field', fld%nomvar, fld%typvar
        call app_log(APP_ERROR, app_msg)
        return
    endif

    query = inputFiles(1)%new_query(datev = fld%datev, etiket = fld%etiket, ip1 = fld%ip1, ip2 = fld%ip2, ip3 = fld%ip3, &
        typvar = '@@', nomvar = fld%nomvar)
    if (.not. query%find_next(mask)) then
        call app_log(APP_ERROR, "get_mask: Failed to read mask record")
        return
    end if
    call query%free()

    if (mask%ni == fld%ni .and. mask%nj == fld%nj .and. mask%nk == fld%nk .and. &
        mask%grtyp == fld%grtyp .and. mask%ig1 == fld%ig1 .and. mask%ig2 == fld%ig2 .and. &
        mask%ig3 == fld%ig3 .and. mask%ig4 == fld%ig4) then
        status = 0
        return
    endif

    query = inputFiles(1)%new_query(datev = fld%datev, etiket = fld%etiket, ip1 = -1, ip2 = fld%ip2, ip3 = fld%ip3, &
        typvar = '@@', nomvar = fld%nomvar)
    if (.not. query%find_next(mask)) then
        call app_log(APP_ERROR, "get_mask: Failed to read mask record")
        return
    end if
    call query%free()

    if (mask%ni == fld%ni .and. mask%nj == fld%nj .and. mask%nk == fld%nk .and. &
        mask%grtyp == fld%grtyp .and. mask%ig1 == fld%ig1 .and. mask%ig2 == fld%ig2 .and. &
        mask%ig3 == fld%ig3 .and. mask%ig4 == fld%ig4) then
        status = 0
        return
    endif

    query = inputFiles(1)%new_query(datev = fld%datev, etiket = fld%etiket, ip1 = fld%ip1, ip2 = fld%ip2, ip3 = fld%ip3, &
        typvar = '@@', nomvar = '@@@@')
    if (.not. query%find_next(mask)) then
        call app_log(APP_ERROR, "get_mask: Failed to read mask record")
        return
    end if
    call query%free()

    if (mask%ni == fld%ni .and. mask%nj == fld%nj .and. mask%nk == fld%nk .and. &
        mask%grtyp == fld%grtyp .and. mask%ig1 == fld%ig1 .and. mask%ig2 == fld%ig2 .and. &
        mask%ig3 == fld%ig3 .and. mask%ig4 == fld%ig4) then
        status = 0
        return
    endif

    query = inputFiles(1)%new_query(datev = fld%datev, etiket = fld%etiket, ip1 = -1, ip2 = fld%ip2, ip3 = fld%ip3, &
        typvar = '@@', nomvar = '@@@@')
    if (.not. query%find_next(mask)) then
        call app_log(APP_ERROR, "get_mask: Failed to read mask record")
        return
    end if
    call query%free()

    if (mask%ni == fld%ni .and. mask%nj == fld%nj .and. mask%nk == fld%nk .and. &
        mask%grtyp == fld%grtyp .and. mask%ig1 == fld%ig1 .and. mask%ig2 == fld%ig2 .and. &
        mask%ig3 == fld%ig3 .and. mask%ig4 == fld%ig4) then
        status = 0
        return
    endif

    write(app_msg, *) 'get_mask: Associated mask not found'
    call app_log(APP_ERROR, app_msg)
end function get_mask
