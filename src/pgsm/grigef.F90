!> Calcul lat long de chaque pt d'une grille "e"
subroutine grigef(nni, nnj, xlat1, xlon1, xlat2, xlon2)
    use app
    implicit none

    integer, intent(in) :: nni
    integer, intent(in) :: nnj
    real, intent(in) :: xlat1
    real, intent(in) :: xlon1
    real, intent(in) :: xlat2
    real, intent(in) :: xlon2

    ! Calculer la latitude et la longitude de tous les points de la grille de sortie standard 'e' lat et long equidistant longitude 0 et 360 present       

#include "llccmm.cdk90"
#include "grilles.cdk90"

    external :: cxgaig
    integer, external :: ezqkdef, gdll

    integer :: ier

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
