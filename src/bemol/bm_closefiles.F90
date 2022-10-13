  subroutine bm_closefiles(lnkdiun, nbfiles, udst, ucfs, ucore, ucoarse)
    implicit none

    integer nbfiles
    integer lnkdiun(nbfiles)
    integer udst, ucfs, ucore, ucoarse
    integer i, ier, fstfrm, fclos

    do i=1,nbfiles
       ier = fstfrm(lnkdiun(i))
       ier = fclos(lnkdiun(i))
    enddo

    if (udst /= -1) then
       ier = fstfrm(udst)
       ier = fclos(udst)
    endif

    if (ucfs /= -1) then
       ier = fstfrm(ucfs)
       ier = fclos(ucfs)
    endif

    if (ucore /= -1 .and. ucore /= udst) then
       ier = fstfrm(ucore)
       ier = fclos(ucore)
    endif

    if (ucoarse /= -1 .and. ucoarse /= ucore .and. ucoarse /= udst) then
       ier = fstfrm(ucoarse)
       ier = fclos(ucoarse)
    endif

    return
  end subroutine bm_closefiles
