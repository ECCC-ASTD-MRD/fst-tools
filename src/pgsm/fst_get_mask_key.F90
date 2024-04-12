integer function fst_get_mask_key(mask_record, fld_record) result(status)
    use rmn_fst24
    use files
    use app
    implicit none

    ! integer :: mask_key, fld_key
    type (fsd_record), intent(out) :: mask_record
    type (fsd_record), intent(in) :: fld_record

    integer :: ier

    integer, external :: fstprm, fstinf

    character(len=4)  :: fld_nomvar, mask_nomvar
    character(len=2)  :: fld_typvar, mask_typvar
    character(len=12) :: fld_etiket, mask_etiket
    character(len=1)  :: fld_grtyp,  mask_grtyp

    integer :: fld_dateo, fld_deet, fld_npas, fld_ni, fld_nj, fld_nk, fld_nbits, fld_datyp
    integer :: fld_ip1, fld_ip2, fld_ip3, fld_ig1, fld_ig2, fld_ig3, fld_ig4
    integer :: fld_lng, fld_dltf, fld_ubc, fld_swa, fld_datev, fld_extra2, fld_extra3

    integer :: mask_dateo, mask_deet, mask_npas, mask_ni, mask_nj, mask_nk, mask_nbits, mask_datyp
    integer :: mask_ip1, mask_ip2, mask_ip3, mask_ig1, mask_ig2, mask_ig3, mask_ig4
    integer :: mask_lng, mask_dltf, mask_ubc, mask_swa, mask_datev, mask_extra2, mask_extra3

    integer :: allones

    type(fst_query) :: query

    allones = -1

    ! ier = fstprm(fld_key, fld_dateo, fld_deet, fld_npas, fld_ni, fld_nj, fld_nk, &
    !             fld_nbits, fld_datyp, fld_ip1, fld_ip2, fld_ip3, fld_typvar, fld_nomvar, fld_etiket, &
    !             fld_grtyp, fld_ig1, fld_ig2, fld_ig3, fld_ig4, fld_swa, fld_lng, fld_dltf, fld_ubc, &
    !             fld_datev, fld_extra2, fld_extra3)

    if (fld_record%typvar(2:2) /= '@') then
        write(app_msg, *) 'fst_get_mask_key: This is not a masked field', fld_record%nomvar, fld_record%typvar
        call app_log(APP_ERROR, app_msg)
        status = -1
        return
    endif

    if (fld_record%typvar(1:1) == '@') then
        write(app_msg, *) 'fst_get_mask_key: This is a mask field', fld_record%nomvar, fld_record%typvar
        call app_log(APP_ERROR, app_msg)
        status = -1
        return
    endif

    mask_datev  = fld_datev
    mask_ip1    = fld_ip1
    mask_ip2    = fld_ip2
    mask_ip3    = fld_ip3
    mask_nomvar = fld_nomvar
    mask_typvar = '@@'

    ! mask_key = fstinf(iun, mask_ni, mask_nj, mask_nk, mask_datev, mask_etiket, mask_ip1, mask_ip2, mask_ip3, mask_typvar, mask_nomvar)
    query = inputFiles(1)%new_query(datev = mask_datev, etiket = mask_etiket, ip1 = mask_ip1, ip2 = mask_ip2, ip3 = mask_ip3, typvar = mask_typvar, nomvar = mask_nomvar)
    query%find_next(mask_record)
    call query%free()


    ! if (mask_key >= 0) then
    !     ier = fstprm(mask_key, mask_dateo, mask_deet, mask_npas, mask_ni, mask_nj, mask_nk, &
    !             mask_nbits, mask_datyp, mask_ip1, mask_ip2, mask_ip3, mask_typvar, mask_nomvar, mask_etiket, &
    !             mask_grtyp, mask_ig1, mask_ig2, mask_ig3, mask_ig4, mask_swa, mask_lng, mask_dltf, mask_ubc, &
    !             mask_datev, mask_extra2, mask_extra3)
    !     if (mask_ni == fld_ni .and. mask_nj == fld_nj .and. mask_nk == fld_nk .and. &
    !         mask_grtyp == fld_grtyp .and. mask_ig1 == fld_ig1 .and. mask_ig2 == fld_ig2 .and. &
    !         mask_ig3 == fld_ig3 .and. mask_ig4 == fld_ig4) then
    !         status = 0
    !         return
    !     endif
    ! endif
    if (mask_record%ni == fld_record%ni .and. mask_record%nj == fld_record%nj .and. mask_record%nk == fld_record%nk .and. &
        mask_record%grtyp == fld_record%grtyp .and. mask_record%ig1 == fld_record%ig1 .and. mask_record%ig2 == fld_record%ig2 .and. &
        mask_record%ig3 == fld_record%ig3 .and. mask_record%ig4 == fld_record%ig4) then
        status = 0
        return
    endif

    mask_ip1 = allones
    ! mask_key = fstinf(iun, mask_ni, mask_nj, mask_nk, mask_datev, mask_etiket, mask_ip1, mask_ip2, mask_ip3, mask_typvar, mask_nomvar)
    query = inputFiles(1)%new_query(datev = mask_datev, etiket = mask_etiket, ip1 = mask_ip1, ip2 = mask_ip2, ip3 = mask_ip3, typvar = mask_typvar, nomvar = mask_nomvar)
    query%find_next(mask_record)
    call query%free()

    ! if (mask_key >= 0) then
    !     ier = fstprm(mask_key, mask_dateo, mask_deet, mask_npas, mask_ni, mask_nj, mask_nk, &
    !             mask_nbits, mask_datyp, mask_ip1, mask_ip2, mask_ip3, mask_typvar, mask_nomvar, mask_etiket, &
    !             mask_grtyp, mask_ig1, mask_ig2, mask_ig3, mask_ig4, mask_swa, mask_lng, mask_dltf, mask_ubc, &
    !             mask_datev, mask_extra2, mask_extra3)
    !     if (mask_ni == fld_ni .and. mask_nj == fld_nj .and. mask_nk == fld_nk .and. &
    !         mask_grtyp == fld_grtyp .and. mask_ig1 == fld_ig1 .and. mask_ig2 == fld_ig2 .and. &
    !         mask_ig3 == fld_ig3 .and. mask_ig4 == fld_ig4) then
    !     status = 0
    !     return
    !     endif
    ! endif
    if (mask_record%ni == fld_record%ni .and. mask_record%nj == fld_record%nj .and. mask_record%nk == fld_record%nk .and. &
        mask_record%grtyp == fld_record%grtyp .and. mask_record%ig1 == fld_record%ig1 .and. mask_record%ig2 == fld_record%ig2 .and. &
        mask_record%ig3 == fld_record%ig3 .and. mask_record%ig4 == fld_record%ig4) then
        status = 0
        return
    endif

    mask_nomvar = '@@@@'
    mask_ip1 = fld_ip1
    ! mask_key = fstinf(iun, mask_ni, mask_nj, mask_nk, mask_datev, mask_etiket, mask_ip1, mask_ip2, mask_ip3, mask_typvar, mask_nomvar)
    query = inputFiles(1)%new_query(datev = mask_datev, etiket = mask_etiket, ip1 = mask_ip1, ip2 = mask_ip2, ip3 = mask_ip3, typvar = mask_typvar, nomvar = mask_nomvar)
    query%find_next(mask_record)
    call query%free()

    ! if (mask_key >= 0) then
    !     ier = fstprm(mask_key, mask_dateo, mask_deet, mask_npas, mask_ni, mask_nj, mask_nk, &
    !             mask_nbits, mask_datyp, mask_ip1, mask_ip2, mask_ip3, mask_typvar, mask_nomvar, mask_etiket, &
    !             mask_grtyp, mask_ig1, mask_ig2, mask_ig3, mask_ig4, mask_swa, mask_lng, mask_dltf, mask_ubc, &
    !             mask_datev, mask_extra2, mask_extra3)
    !     if (mask_ni == fld_ni .and. mask_nj == fld_nj .and. mask_nk == fld_nk .and. &
    !         mask_grtyp == fld_grtyp .and. mask_ig1 == fld_ig1 .and. mask_ig2 == fld_ig2 .and. &
    !         mask_ig3 == fld_ig3 .and. mask_ig4 == fld_ig4)  then
    !     status = 0
    !     return
    !     endif
    ! endif
    if (mask_record%ni == fld_record%ni .and. mask_record%nj == fld_record%nj .and. mask_record%nk == fld_record%nk .and. &
        mask_record%grtyp == fld_record%grtyp .and. mask_record%ig1 == fld_record%ig1 .and. mask_record%ig2 == fld_record%ig2 .and. &
        mask_record%ig3 == fld_record%ig3 .and. mask_record%ig4 == fld_record%ig4) then
        status = 0
        return
    endif

    mask_ip1 = allones
    ! mask_key = fstinf(iun, mask_ni, mask_nj, mask_nk, mask_datev, mask_etiket, mask_ip1, mask_ip2, mask_ip3, mask_typvar, mask_nomvar)
    query = inputFiles(1)%new_query(datev = mask_datev, etiket = mask_etiket, ip1 = mask_ip1, ip2 = mask_ip2, ip3 = mask_ip3, typvar = mask_typvar, nomvar = mask_nomvar)
    query%find_next(mask_record)
    call query%free()

    ! if (mask_key >= 0) then
    !     ier = fstprm(mask_key, mask_dateo, mask_deet, mask_npas, mask_ni, mask_nj, mask_nk, &
    !             mask_nbits, mask_datyp, mask_ip1, mask_ip2, mask_ip3, mask_typvar, mask_nomvar, mask_etiket, &
    !             mask_grtyp, mask_ig1, mask_ig2, mask_ig3, mask_ig4, mask_swa, mask_lng, mask_dltf, mask_ubc, &
    !             mask_datev, mask_extra2, mask_extra3)
    !     if (mask_ni == fld_ni .and. mask_nj == fld_nj .and. mask_nk == fld_nk .and. &
    !         mask_grtyp == fld_grtyp .and. mask_ig1 == fld_ig1 .and. mask_ig2 == fld_ig2 .and. &
    !         mask_ig3 == fld_ig3 .and. mask_ig4 == fld_ig4)  then
    !     status = 0
    !     return
    !     endif
    ! endif

    if (mask_record%ni == fld_record%ni .and. mask_record%nj == fld_record%nj .and. mask_record%nk == fld_record%nk .and. &
        mask_record%grtyp == fld_record%grtyp .and. mask_record%ig1 == fld_record%ig1 .and. mask_record%ig2 == fld_record%ig2 .and. &
        mask_record%ig3 == fld_record%ig3 .and. mask_record%ig4 == fld_record%ig4) then
        status = 0
        return
    endif

    write(app_msg, *) 'fst_get_mask_key: Associated mask not found'
    call app_log(APP_ERROR, app_msg)
    status = -1
end function fst_get_mask_key
