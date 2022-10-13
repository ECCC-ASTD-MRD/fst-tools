!> Loop over search results to compute each field's stats
subroutine loop_fields(iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)
    implicit none

    integer, intent(in) :: iun
    integer, intent(out) :: ni, nj, nk
    integer, intent(in) :: datev
    character(len = 12), intent(in) :: etiket
    integer, intent(in) :: ip1, ip2, ip3
    character(len = 2), intent(in) :: typvar
    character(len = 4), intent(in) :: nomvar

    integer, external :: fstprm, fstinf, fstsui, fstluk, fstopl
    character(len = 1) :: grtyp
    integer :: ier
    integer :: i

    integer :: key, date0, deet, npas, nbits, datyp
    integer :: swa, lng, dltf, ubc
    integer :: ig1, ig2, ig3, ig4, extra1, extra2, extra3

    real :: rtemp
    integer :: itemp

    equivalence (rtemp, itemp)

    real, allocatable, dimension(:) :: buf

    character(len = 12) :: etiket2
    character(len = 4) :: nomvar2

    ier = fstopl('REDUCTION32', .true., .false.)

    ! Get field size for the first iteration
    key = fstinf(iun, ni, nj, nk,  datev, etiket, ip1, ip2, ip3, typvar, nomvar)

    do while (key >= 0)
        allocate(buf(ni * nj * nk))

        ! Get field descriptors
        ier = fstprm(key, date0, deet, npas, ni, nj, nk, nbits, &
            datyp, ip1, ip2, ip3, typvar, nomvar2, etiket2, grtyp, &
            ig1, ig2, ig3, ig4, swa, lng, dltf, ubc, &
            extra1, extra2, extra3)

        if (nomvar2 /= '!!') then
            ier = fstluk(buf, key, ni, nj, nk)

            if (datyp == 2 .or. datyp == 4) then
                do i = 1, ni * nj * nk
                    rtemp = buf(i)
                    buf(i) = real(itemp)
                end do
            end if

            call statfld4(nomvar2, typvar, ip1, ip2, ip3, date0, etiket2, buf, ni, nj, nk)
        else
            write(6, *)' **   SKIPPING RECORD "!!", CAN''T PROCESS  **'
        end if

        deallocate(buf)

        ! ni, nj and nk will be modified by fstsui
        key = fstsui(iun, ni, nj, nk)
    end do
end subroutine
