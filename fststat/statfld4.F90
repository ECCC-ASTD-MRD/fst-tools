!> Calcule la moyenne, la variance, le rminimum et le maximum d'un champs et imprime les resultats
subroutine statfld4(nomvar, typvar, ip1, ip2, ip3, date, etiket, field, ni, nj, nk)
    use iso_fortran_env

    implicit none

    !> Nom de la variable du champs
    character(len = 4), intent(in) :: nomvar
    !> Type de la variable du champs
    character(len = 2), intent(in) :: typvar
    !> Identificateurs et date d'origine du champs
    integer, intent(in) :: ip1, ip2, ip3, date
    !> Étiquette du champs
    character(len = 12), intent(in) :: etiket

    !> Dimensions du champs
    integer, intent(in) :: ni, nj, nk
    !> Champs
    real, dimension(ni, nj, nk), intent(in) :: field

    integer :: i, j, k
    real(kind = real64) :: sum, moy, var
    real :: rmin, rmax
    integer :: imin, jmin, kmin, imax, jmax, kmax, kind
    character(len = 15) :: Level
    real :: rlevel


    ! Calculer la moyenne
    sum = 0.0
    do k = 1, nk
        do j = 1, nj
            do i = 1, ni
                sum = sum + field(i, j, k)
            end do
        end do
    end do
    moy = sum / float(ni * nj * nk)

    ! Calculer la variance
    sum = 0.0
    do k = 1, nk
        do j = 1, nj
            do i = 1, ni
                sum = sum + ((field(i, j, k) - moy) * (field(i, j, k) - moy))
            end do
        end do
    end do
    var = sqrt (sum / float(ni * nj * nk))

    ! Identifier le minimum et le maximum.
    imin = 1
    jmin = 1
    kmin = 1
    imax = 1
    jmax = 1
    kmax = 1
    rmax = field(1, 1, 1)
    rmin = field(1, 1, 1)

    do k = 1, nk
        do j = 1, nj
            do i = 1, ni
                if (field(i, j, k) > rmax) then
                    rmax  = field(i, j, k)
                    imax = i
                    jmax = j
                    kmax = k
                end if
                if (field(i, j, k) < rmin) then
                    rmin  = field(i, j, k)
                    imin = i
                    jmin = j
                    kmin = k
                end if
            end do
        end do
    end do

    call convip_plus(ip1, rlevel, kind, -1, level, .true.)

    write(6, 10) nomvar, typvar, level, ip2, ip3, date, etiket, &
        moy, var, imin, jmin + (kmin - 1) * nj, rmin, &
        imax, jmax + (kmax - 1) * nj, rmax

10  format (' ', a4, 1x, a2, 1x, a15, 1x, i4, 1x, i3, 1x, i9, 1x, a12, 1x, &
        ' Mean:', e13.6, ' StDev:', e13.6, &
        '  Min:[(', i5, ',', i5, '):',  e11.4, ']', &
        ' Max:[(', i5, ',', i5, '):', e11.4, ']')

end subroutine
