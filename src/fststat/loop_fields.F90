!> Loop over search results to compute each field's stats
subroutine loop_fields(source)
    use rmn_fst24
    implicit none

    integer, external :: fstopl
    type(fst_file)    :: source
    type(fst_record)  :: record

    integer :: i,j,k, ier
    logical :: success

    real(kind = real32), dimension(:, :, :), pointer :: data_r4
    real :: rtemp
    integer :: itemp

    equivalence (rtemp, itemp)

    ier = fstopl('REDUCTION32', .true., .false.)

    success = source%set_search_criteria()

    do while(source%find_next(record))
        if (record%nomvar /= '!!') then
            success = record%read()
            call record % get_data_array(data_r4) 

            if (record%datyp == 2 .or. record%datyp == 4) then
                do k = 1, record%nk
                   do j = 1, record%nj
                      do i = 1, record%ni
                        rtemp = data_r4(i,j,k)
                        data_r4(i,j,k) = real(itemp)
                      end do
                   end do
                end do
            end if

            call statfld4(record)
        else
            write(6, *)' **   SKIPPING RECORD "!!", CAN''T PROCESS  **'
        end if

    end do
end subroutine
