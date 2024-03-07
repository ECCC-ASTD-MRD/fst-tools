!> Loop over search results to compute each field's stats
subroutine loop_fields(source)
    use rmn_fst24
    implicit none

    integer, external :: fstopl
    type(fst_file)    :: source
    type(fst_record)  :: record

    integer :: i, ier
    logical :: success

    real :: rtemp
    integer :: itemp

    equivalence (rtemp, itemp)

    ier = fstopl('REDUCTION32', .true., .false.)

    success = source%set_search_criteria()

    do while(source%find_next(record))
 
        if (record%nomvar /= '!!') then
            success = record%read()

            if (record%datyp == 2 .or. record%datyp == 4) then
                do i = 1, record%ni * record%nj * record%nk
                    rtemp = record%data(i)
                    record%data(i) = real(itemp)
                end do
            end if

            call statfld4(record%nomvar)
        else
            write(6, *)' **   SKIPPING RECORD "!!", CAN''T PROCESS  **'
        end if

        deallocate(buf)

    end do
end subroutine
