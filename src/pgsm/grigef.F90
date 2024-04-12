!> Calcul lat long de chaque pt d'une grille "e"
subroutine grigef(it, nni, nnj, xlat1, xlon1, xlat2, xlon2)
    use app
    implicit none

!     OBJET(GRISTDB)
!     CALCULER LA LATITUDE ET LA LONGITUDE DE TOUS LES POINTS
!     DE LA GRILLE DE SORTIE STANDARD 'e' LAT ET LONG EQUIDISTANT
!     LONGITUDE ZERO ET 360 PRESENT.

    external ezqkdef, gdll
    integer ezqkdef, gdll

#include "llccmm.cdk90"
#include "grilles.cdk90"

    integer nni, nnj, it, ier
    real xlat1, xlon1, xlat2, xlon2
    li = nni
    lj = nnj
    ! RESERVER MEMOIR POUR LATITUDE ET LONGITUDE
    allocate(tmplat(nni, nnj))
    allocate(tmplon(nni, nnj))
    allocate(tmplatg(nni, nnj))
    allocate(tmplong(nni, nnj))
    cgrtyp='E'
    call cxgaig(cgrtyp, lg1, lg2, lg3, lg4, xlat1, xlon1, xlat2, xlon2)
    gdout = ezqkdef(li, lj, cgrtyp, lg1, lg2, lg3, lg4, 0)
    ier = gdll(gdout, tmplat, tmplon)

end
