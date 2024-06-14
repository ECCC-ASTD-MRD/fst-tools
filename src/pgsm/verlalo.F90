!> Verify that latitudes and longitudes are within the limits
subroutine verlalo(clat, clon, npts)
    use app
    implicit none

    !> Number of points
    integer, intent(in) :: npts
    !> Latitudes
    real, intent(inout) :: clat(npts)
    !> Longitudes
    real, intent(inout) :: clon(npts)

    integer :: i

    do i = 1, npts
        if (clon(i) < 0.0) clon(i) = clon(i)+360.0
        if (clon(i) >= 360.0) clon(i) = clon(i) - 360.0
        if (clat(i) < -90.005 .or. clat(i) > 90.005) then
            write(app_msg, *) 'verlalo: Wrong latitude: ', clat(i)
            call app_log(APP_ERROR, app_msg)
        endif
        clat(i) = max(-90.0, min(90.0, clat(i)))
    enddo
end
