!> Calcul latitude longitude de chaque pt d'une grille std
subroutine grilstd(nni, nnj, hem)
    use app
    use pgsm_mod, only: tmplat, tmplon
    use grilles, only : cgrtyp, gdout, li, lj, lg1, lg2, lg3, lg4
    implicit none

    !> Nombre de points est-ouest
    integer, intent(in) :: nni
    !> Nombre de points nord-sud
    integer, intent(in) :: nnj
    !> 0=GLOBAL;  1=H. NORD;  2=H. SUD
    integer, intent(in) :: hem

    !          CALCULER LA LATITUDE ET LA LONGITUDE DE TOUS LES POINTS
    !          DE LA GRILLE DE SORTIE STANDARD INTERVAL REGULIER MAIS DECALE
    !          1/2 POINT DU POLE ET DE L'EQUATEUR

    external pgsmabt
    integer, external :: ezqkdef, gdll

    integer :: ier

    li = nni
    lj = nnj

    allocate(tmplat(nni, nnj))
    allocate(tmplon(nni, nnj))

    cgrtyp = 'A'
    lg1 = hem
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
