!> Calcule la moyenne, la variance, le rminimum et le maximum d'un champs et imprime les resultats
subroutine statfld4(record)
    use app
    use ieee_arithmetic
    use iso_fortran_env
    use rmn_fst24
    implicit none

    type(fst_record) :: record
 
    integer :: i, j, k
    integer(C_INT64_T) :: size, num_nan, num_inf
    real(kind = real32), dimension(:, :, :), pointer :: data_r4
    real(kind = real64) :: sum, moy, var
    real :: rmin, rmax
    integer :: imin, jmin, kmin, imax, jmax, kmax, kind
    character(len = 15) :: Level
    real :: rlevel

    size=record%ni*record%nj*record%nk
    call record % get_data_array(data_r4, in_check_type = .false.) 

    num_nan = 0
    num_inf = 0

    ! Calculer la moyenne
    sum = 0.0
    do k = 1, record%nk
        do j = 1, record%nj
            do i = 1, record%ni
                if (ieee_is_nan(data_r4(i,j,k))) then
                    num_nan = num_nan + 1
                    cycle 
                else if (.not. ieee_is_finite(data_r4(i,j,k))) then
                    num_inf = num_inf + 1
                endif
                sum = sum + data_r4(i,j,k)
            end do
        end do
    end do
    if (num_nan == size) then
        moy = ieee_value(moy, ieee_quiet_nan)
    else
        moy = sum / float(size - num_nan)
    end if

    ! Calculer la variance
    sum = 0.0
    do k = 1, record%nk
        do j = 1, record%nj
            do i = 1, record%ni
                if (.not. ieee_is_finite(data_r4(i,j,k))) cycle
                sum = sum + ((data_r4(i,j,k) - moy) * (data_r4(i,j,k) - moy))
            end do
        end do
    end do
    if (size == num_nan + num_inf) then
        var = ieee_value(var, ieee_quiet_nan)
    else
        var = sqrt (sum / float(size - num_nan - num_inf))
    end if

    ! Identifier le minimum et le maximum.
    imin = 1
    jmin = 1
    kmin = 1
    imax = 1
    jmax = 1
    kmax = 1
    rmax = ieee_value(rmax, ieee_negative_inf)
    rmin = ieee_value(rmax, ieee_positive_inf)

    do k = 1, record%nk
        do j = 1, record%nj
            do i = 1, record%ni
                if (ieee_is_nan(data_r4(i,j,k))) cycle
                if (data_r4(i,j,k) > rmax) then
                    rmax  = data_r4(i,j,k)
                    imax = i
                    jmax = j
                    kmax = k
                end if
                if (data_r4(i,j,k) < rmin) then
                    rmin  = data_r4(i,j,k)
                    imin = i
                    jmin = j
                    kmin = k
                end if
            end do
        end do
    end do

    call convip_plus(record%ip1, rlevel, kind, -1, level, .true.)

    write(app_msg, 10) record%nomvar, record%typvar, level, record%ip2, record%ip3, record%datev, record%etiket, &
        moy, var, imin, jmin + (kmin - 1) * record%nj, rmin, imax, jmax + (kmax - 1) * record%nj, rmax
    call app_log(APP_VERBATIM, app_msg)

    if (num_nan > 0) then
        write(app_msg, '(a, i10)') '  Number of NaN in record: ', num_nan
        call app_log(APP_WARNING, app_msg)
    end if
    if (num_inf > 0) then
        write(app_msg, '(a, i10)') '  Number of infinities in record: ', num_inf
        call app_log(APP_WARNING, app_msg)
    end if

10  format (' ', a4, 1x, a2, 1x, a15, 1x, i4, 1x, i3, 1x, i9, 1x, a12, 1x, &
        ' Mean:', e13.6, ' StDev:', e13.6, &
        ' Min:[(', i5, ',', i5, '):',  e11.4, ']', &
        ' Max:[(', i5, ',', i5, '):', e11.4, ']')

end subroutine
