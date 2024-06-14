
!> Calcul latitude longitude de chaque pt d'une grille grib
subroutine grigrib(ig1, ig2, ig3, ig4)
    use app
    use pgsm_mod, only : tmplat, tmplon
    use grilles, only : cgrtyp, gdout, lg1, lg2, lg3, lg4, li, lj
    implicit none

    character(len = 1) :: gtyout
    real :: xg(20)
    integer :: nni, nnj, ier, npts
    integer :: ig1, ig2, ig3, ig4

    integer, external :: ezqkdef, gdll

    real, dimension(:, :), allocatable :: x, y

    cgrtyp = '!'
    call igaxg95(gtyout, xg, 15, cgrtyp, ig1, ig2, ig3, ig4)
    if (gtyout.eq.'H') then
        nni = nint(xg(8))
        nnj = nint(xg(9))
    endif

    allocate(tmplat(nni, nnj))
    allocate(tmplon(nni, nnj))
    allocate(x(nni, nnj))
    allocate(y(nni, nnj))


    li = nni
    lj = nnj
    lg1 = ig1
    lg2 = ig2
    lg3 = ig3
    lg4 = ig4

    npts = nni * nnj
    call ez_llflamb(tmplat, tmplon, x, y, npts, cgrtyp, ig1, ig2, ig3, ig4)

    gdout = ezqkdef(nni, nnj, cgrtyp, ig1, ig2, ig3, ig4, 1)
    ier = gdll(gdout, tmplat, tmplon)

    deallocate(x)
    deallocate(y)
end
