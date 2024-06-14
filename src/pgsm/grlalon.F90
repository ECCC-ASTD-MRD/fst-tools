!> Calcul latitude longitude de chaque pt d'une grille latlon
subroutine grlalon(nni, nnj, p1, p2, p3, p4)
    use app
    use pgsm_mod, only: tmplat, tmplon
    use grilles, only : cgrtyp, gdout, lg1, lg2, lg3, lg4, li, lj
    implicit none

    integer, intent(in) :: nni, nnj
    real, intent(in) :: p1, p2, p3, p4

    external cigaxg, cxgaig
    integer, external :: ezqkdef, gdll

    integer :: ier
    real :: pp1, pp2, pp3, pp4

    li = nni
    lj = nnj

    allocate(tmplat(nni, nnj))
    allocate(tmplon(nni, nnj))

    ! RECALCUL  LAT, LONG, DELTA LAT, DELTA LONG
    ! MEME VALEUR A L'ENTRE COMME A LA SORTIE
    cgrtyp = 'L'
    call cxgaig(cgrtyp, lg1, lg2, lg3, lg4, p1, p2, p3, p4)
    call cigaxg(cgrtyp, pp1, pp2, pp3, pp4, lg1, lg2, lg3, lg4)

    gdout = ezqkdef(li, lj, cgrtyp, lg1, lg2, lg3, lg4, 0)
    ier = gdll(gdout, tmplat, tmplon)
end
