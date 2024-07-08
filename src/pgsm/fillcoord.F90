subroutine fillcoord(lat, lon)
    use grilles, only : coordll, ncoords
    implicit none

    real, intent(out) :: lat(ncoords)
    real, intent(out) :: lon(ncoords)

    integer :: i

    do i = 1, ncoords
        lat(i) = coordll(i, 1)
        lon(i) = coordll(i, 2)
    enddo
end
