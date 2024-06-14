!> Calculer U et V à partir de la direction et la vitesse du vent
subroutine vdauv(srtentu, srtentv, clong, dgtord, npts)
    use app
    use grilles, only : dgrwxy, cgtypxy
    implicit none

    !> Number of points in each field
    integer, intent(in) :: npts
    !> As input: direction, as output: U vector
    real, intent(inout) :: srtentu(npts)
    !> As input: speed, as output: V vector
    real, intent(inout) :: srtentv(npts)
    !> Field of longitudes in degree 0-360
    real, intent(in) :: clong(npts)
    !> Conversion factor from degree to radian
    real, intent(in) :: dgtord

    !APPEL
    !     - VIA UVECTUR 
    !     - CALL VDAUV(SRTENTU, SRTENTV, CLONG, DGTORD, npts)

    external pgsmabt, messags

    real :: angle, u, v
    integer :: i

    ! Si le type de grille "L"
    if (cgtypxy == 'L') then
        do i = 1, npts
            angle = dgtord * (srtentv(i) - clong(i))
            u = srtentu(i) * sin(angle)
            v = -srtentu(i) * cos(angle)
            srtentu(i) = u
            srtentv(i) = v
        enddo

    ! Si le type de grille grtypxy = "N" hem nord
    else if (cgtypxy == 'N') then
        do i = 1, npts
            !> \bug dgrwxy is never initialized!
            angle = dgtord * (dgrwxy + srtentv(i))
            u = srtentu(i) * cos(angle)
            v = srtentu(i) * sin(angle)
            srtentu(i) = u
            srtentv(i) = v
        enddo
    else
        call app_log(APP_ERROR, 'vdauv: GRILLE not "L" or "N", no valid code for type "s"')
        call pgsmabt
    endif
end 
