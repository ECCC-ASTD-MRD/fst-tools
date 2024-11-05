!> Loop over search results to compute each field's stats
subroutine loop_fields(source)
    use rmn_fst24
    implicit none

    integer, external :: fstopl
    type(fst_file)    :: source
    type(fst_record)  :: record
    type(fst_query)  :: query

    integer :: i,j,k, ier
    logical :: success

    real(kind = real32), dimension(:, :, :), pointer :: data_r4
    integer(kind = int32), dimension(:, :, :), pointer :: data_i4
    real :: rtemp
    integer :: itemp
    type(c_ptr) :: ptr_c
    integer(int64) :: ptr

    equivalence (rtemp, itemp)

    ier = fstopl('REDUCTION32', .true., .false.)

    query = source%new_query()

    do while(query%find_next(record))
        if (record%nomvar /= '!!') then
            success = record%read()
            if (.not. success) then
                call app_log(APP_WARNING, 'Unable to read record')
                cycle
            end if

            if (record%data_type == FST_TYPE_SIGNED .or. record%data_type == FST_TYPE_UNSIGNED) then
                call record % get_data_array(data_r4, in_check_type = .false.) 
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

    call record % free()
    call query % free()
end subroutine
