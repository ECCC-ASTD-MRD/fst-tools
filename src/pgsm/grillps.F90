!> Calcul latitude longitude de chaque point d'une grille p.s.
subroutine grillps(nni, nnj, pi, pj, d60, dgrw, hem)
    use app
    use pgsm_mod, only: tmplat, tmplon
    
    use grilles, only : cgrtyp, gdout, li, lj, lg1, lg2, lg3, lg4
    implicit none

    integer, intent(in) :: nni, nnj, hem
    real, intent(in) :: pi, pj, d60, dgrw

    external pgsmabt, cigaxg, cxgaig

    integer, external :: ezqkdef, gdll

    integer :: ier, ihm
    real :: pp1, pp2, pp3, pp4

    allocate(tmplat(nni, nnj))
    allocate(tmplon(nni, nnj))

    li = nni
    lj = nnj

    ihm = hem
    if (ihm == 1) then
        cgrtyp = 'N'
    else
        cgrtyp = 'S'
    endif

    call cxgaig(cgrtyp, lg1, lg2, lg3, lg4, pi, pj, d60, dgrw)
    call cigaxg(cgrtyp, pp1, pp2, pp3, pp4, lg1, lg2, lg3, lg4)

    if (ihm < 1 .or. ihm > 2) then
        call app_log(APP_ERROR, 'grillps: GRILLE(PS... must be NORD or SUD')
        call pgsmabt
    endif

    gdout = ezqkdef(li, lj, cgrtyp, lg1, lg2, lg3, lg4, 0)
    ier = gdll(gdout, tmplat, tmplon)
end
