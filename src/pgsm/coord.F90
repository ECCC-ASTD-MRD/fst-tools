subroutine coord(lescoords, mode)
    use app
    use grilles, only : coordll, ncoords, nmaxcoords
    implicit none

    real, dimension(*), INTENT(IN) :: lescoords
    integer, INTENT(IN) :: mode

    integer, external :: argdims

    integer :: i, localncoords

    localncoords = argdims(1)
    if (mod(localncoords, 2) /= 0) then
        call app_log(APP_ERROR,'coord: Number of coordinates odd')
        call pgsmabt
    endif

    if (mode == 0) then
        ncoords = 0
    endif

    do i = 1, localncoords, 2
        if (i < nmaxcoords) then
            coordll(ncoords + i / 2 + 1, 1) = lescoords(i)
            coordll(ncoords + i / 2 + 1, 2) = lescoords(i + 1)
        else
            write(app_msg, *) 'coord: Too many points, Max=', nmaxcoords
            call app_log(APP_WARNING, app_msg)
        endif
    enddo

    ncoords = ncoords + localncoords / 2
end
