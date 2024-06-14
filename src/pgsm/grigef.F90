!> Calcul lat long de chaque pt d'une grille "e"
subroutine grigef(it, nni, nnj, xlat1, xlon1, xlat2, xlon2)
    use app
    use pgsm_mod, only: tmplat, tmplon, tmplatg, tmplong
    use grilles, only : cgrtyp, gdout, lg1, lg2, lg3, lg4, li, lj
    implicit none

    ! Calculer la latitude et la longitude de tous les points de la grille de sortie standard 'e' lat et long equidistant longitude 0 et 360 present

    integer, external :: ezqkdef, gdll

    integer nni, nnj, it, ier
    real xlat1, xlon1, xlat2, xlon2
    li = nni
    lj = nnj
    ! RESERVER MEMOIR POUR LATITUDE ET LONGITUDE
    allocate(tmplat(nni, nnj))
    allocate(tmplon(nni, nnj))
    allocate(tmplatg(nni, nnj))
    allocate(tmplong(nni, nnj))
    cgrtyp = 'E'
    call cxgaig(cgrtyp, lg1, lg2, lg3, lg4, xlat1, xlon1, xlat2, xlon2)
    gdout = ezqkdef(li, lj, cgrtyp, lg1, lg2, lg3, lg4, 0)
    ier = gdll(gdout, tmplat, tmplon)
end
