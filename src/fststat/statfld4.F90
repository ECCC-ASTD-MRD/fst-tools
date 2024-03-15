!> Calcule la moyenne, la variance, le rminimum et le maximum d'un champs et imprime les resultats
subroutine statfld4(record)
    use iso_fortran_env
    use rmn_fst24
    implicit none

    type(fst_record) :: record
 
    integer :: i, j, k
    integer(C_INT64_T) :: size 
    real(kind = real32), dimension(:, :, :), pointer :: data_r4
    real(kind = real64) :: sum, moy, var
    real :: rmin, rmax
    integer :: imin, jmin, kmin, imax, jmax, kmax, kind
    character(len = 15) :: Level
    real :: rlevel

    size=record%ni*record%nj*record%nk
    call record % get_data_array(data_r4) 

    ! Calculer la moyenne
    sum = 0.0
    do k = 1, record%nk
        do j = 1, record%nj
            do i = 1, record%ni
                sum = sum + data_r4(i,j,k)
            end do
        end do
    end do
    moy = sum / float(size)

    ! Calculer la variance
    sum = 0.0
    do k = 1, record%nk
        do j = 1, record%nj
            do i = 1, record%ni
                sum = sum + ((data_r4(i,j,k) - moy) * (data_r4(i,j,k) - moy))
            end do
        end do
    end do
    var = sqrt (sum / float(size))

    ! Identifier le minimum et le maximum.
    imin = 1
    jmin = 1
    kmin = 1
    imax = 1
    jmax = 1
    kmax = 1
    rmax = data_r4(1,1,1)
    rmin = data_r4(1,1,1)

    do k = 1, record%nk
        do j = 1, record%nj
            do i = 1, record%ni
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

    write(6, 10) record%nomvar, record%typvar, level, record%ip2, record%ip3, record%datev, record%etiket, &
        moy, var, imin, jmin + (kmin - 1) * record%nj, rmin, imax, jmax + (kmax - 1) * record%nj, rmax

10  format (' ', a4, 1x, a2, 1x, a15, 1x, i4, 1x, i3, 1x, i9, 1x, a12, 1x, &
        ' Mean:', e13.6, ' StDev:', e13.6, &
        ' Min:[(', i5, ',', i5, '):',  e11.4, ']', &
        ' Max:[(', i5, ',', i5, '):', e11.4, ']')

end subroutine
