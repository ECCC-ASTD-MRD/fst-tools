!> Calcul lat long de chaque pt d'une grille gaussienne
subroutine grigaus(nni, nnj, nhem)
    use app
    use pgsm_mod, only: tmplat, tmplon
    use grilles, only : cgrtyp, gdout, li, lj, lg1, lg2, lg3, lg4
    implicit none

    ! CALCULER LA LATITUDE ET LA LONGITUDE DE TOUS LES POINTS
    ! DE LA GRILLE DE SORTIE GAUSSIENNE LONGITUDE EQUIDISTANTE
    ! LATITUDE GAUSSIENNE

    external pgsmabt
    integer, external :: ezqkdef, gdll

    integer nni, nnj, nhem, ier, nroot
    real, dimension(:), pointer :: tmproot

    allocate(tmplat(nni, nnj))
    allocate(tmplon(nni, nnj))

    lg1 = nhem

    li = nni
    lj = nnj

    if (lg1 < 0 .or. lg1 > 2) then
        call app_log(APP_ERROR, 'grigaus: GRILLE(GAUSS... L11 must be GLOBAL, NORD, SUD ')
        call pgsmabt
    endif

    cgrtyp = 'G'
    lg2 = 0
    lg3 = 0
    lg4 = 0

    if (lg1 == 0) then
        nroot = nnj
    else
        nroot = nnj * 2
    endif

    allocate(tmproot(nroot))

    tmproot(1) = 100.0
    gdout = ezqkdef(li, lj, cgrtyp, lg1, lg2, lg3, lg4, 0)
    ier = gdll(gdout, tmplat, tmplon)

    deallocate(tmproot)
end
