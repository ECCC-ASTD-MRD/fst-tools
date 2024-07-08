!> Calcul latitude longitude de chaque pt d'une grille p.s.
subroutine gristereo(nni, nnj, d60, dgrw, clat, clon)
    use app
    use pgsm_mod, only : tmplat, tmplon
    use grilles, only : li, lj, cgrtyp, lg1, lg2, lg3, lg4, gdout
    implicit none

    external cigaxg, cxgaig
    integer, external :: ezqkdef, gdll

    integer :: nni, nnj, ier
    real :: d60, dgrw, pp1, pp2, pp3, pp4, clat, clon

    allocate(tmplat(nni, nnj))
    allocate(tmplon(nni, nnj))

    li = nni
    lj = nnj

    cgrtyp = 'T'

    call cxgaig(cgrtyp, lg1, lg2, lg3, lg4, d60, dgrw, clat, clon)
    call cigaxg(cgrtyp, pp1, pp2, pp3, pp4, lg1, lg2, lg3, lg4)

    gdout = ezqkdef(li, lj, cgrtyp, lg1, lg2, lg3, lg4, 0)
    ier = gdll(gdout, tmplat, tmplon)
end
