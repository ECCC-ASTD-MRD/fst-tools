!> Calcul latitude longitude de chaque pt d'une grille std
subroutine grilstd(nni, nnj, hem)
    use app
    implicit none

!OBJET(GRILSTD)
!          CALCULER LA LATITUDE ET LA LONGITUDE DE TOUS LES POINTS
!          DE LA GRILLE DE SORTIE STANDARD INTERVAL REGULIER MAIS DECALE
!          1/2 POINT DU POLE ET DE L'EQUATEUR

    external pgsmabt, grll
    external ezqkdef, gdll
    integer ezqkdef, gdll

#include "llccmm.cdk90"
#include "grilles.cdk90"

    integer nni, nnj, hem, ier

    li = nni
    lj = nnj

    allocate(tmplat(nni, nnj))
    allocate(tmplon(nni, nnj))

    lg1 = hem
    cgrtyp = 'A'
    lg2 = 0
    lg3 = 0
    lg4 = 0

    gdout = ezqkdef(li, lj, cgrtyp, lg1, lg2, lg3, lg4, 0)
    ier = gdll(gdout, tmplat, tmplon)

    if (lg1 /= 0 .and. lg1 /= 1 .and. lg1 /= 2) then
        call app_log(APP_ERROR, 'griltd: GRILLE(STD... must be GLOBAL, NORD, SUD ')
        call pgsmabt
    endif
end
