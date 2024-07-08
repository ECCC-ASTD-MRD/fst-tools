!> Calcul lat long de chaque pt d'une grille std "b"
subroutine gristdb(nni, nnj, hem)
    use app
    use pgsm_mod, only: tmplat, tmplon
    use grilles, only : cgrtyp, gdout, li, lj, lg1, lg2, lg3, lg4
    implicit none

    integer, intent(in) :: nni, nnj, hem

    integer, external :: ezqkdef, gdll

    integer :: ier

    li = nni
    lj = nnj

    allocate(tmplat(nni, nnj))
    allocate(tmplon(nni, nnj))

    lg1 = hem
    cgrtyp = 'B'
    lg2 = 0
    lg3 = 0
    lg4 = 0

    if (lg1 < 0 .or. lg1 > 2) then
        call app_log(APP_ERROR, 'gristdb: GRILLE(STDB... must be GLOBAL, NORD, SUD ')
        call pgsmabt
    endif

    gdout = ezqkdef(li, lj, cgrtyp, lg1, lg2, lg3, lg4, 0)
    ier = gdll(gdout, tmplat, tmplon)
end
