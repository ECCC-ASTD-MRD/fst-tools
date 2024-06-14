!> Identification du champ appeler routine appropriee
subroutine champ(nom, ipr1, ipr2, ipr3, ipr4, ipr5, ipr6, ipr7, ipr8, ipr9, ipr10, &
      ipr11, ipr12, ipr13, ipr14, ipr15, ipr16, ipr17, ipr18, ipr19, ipr20, &
      ipr21, ipr22, ipr23, ipr24, ipr25, ipr26, ipr27, ipr28, ipr29, ipr30)

    use app
    use files
    use pgsm_mod, only : vvent, wdvent, ip1style, tmplat, message
    use grilles, only : cgrtyp, ngr
    use heuress, only : nhur, nheures
    implicit none

    !> Nom du champ (directives de l'usager)
    integer, intent(in) :: nom(2)
    !> Descripteurs suplementaires (heures - niveaux) demandees par l'usager. 30 maximum
    integer, intent(in) :: ipr1(2), ipr2(2), ipr3(2), ipr4(2), ipr5(2), ipr6(2), ipr7(2) &
                           ipr8(2), ipr9(2), ipr10(2), ipr11(2), ipr12(2), ipr13(2), ipr14(2), ipr15(2) &
                           ipr16(2), ipr17(2), ipr18(2), ipr19(2), ipr20(2), ipr21(2), ipr22(2), ipr23(2) &
                           ipr24(2), ipr25(2), ipr26(2), ipr27(2), ipr28(2), ipr29(2), ipr30(2)

    !         POINT D'ENTREE APPELE PAR LA DIRECTIVE
    !         CHAMP(NOM, PARM1, PARM2....PARMX)
    !         EX.  CHAMP(Z, 1000, 850, 700, 500)
    !         LE S/P CHAMP APPELLE LE SOUS PROGRAMME APPROPRIE
    !         POUR LE TYPE DE CHAMP DEMANDE
    ! Appel via directive CHAMP(NOM, IPR1......IPR30)

    external grille2, epaisur, macpcp, uvectur, scalair
    integer, external :: argdims

#include "defin.cdk90"
#include "champs.cdk90"
#include "pairs.cdk90"

    integer :: ihr, ihrs, nparm

    integer :: i, np, nw, trouve
    integer :: kinds(30)

    character(len = 8) :: cnom, string
    real :: ptr

    if (inputMode == SEQUENTIEL) then
            app_status=app_end(-1)
            call app_log(APP_ERROR, 'champ: Cannot use directive CHAMP with a sequential input file, use directive CHAMP_SEQ instead')
        return
    endif

    nchamp = min0(31, nchamp)

    champpr(30) = ipr30(1)
    champpr(29) = ipr29(1)
    champpr(28) = ipr28(1)
    champpr(27) = ipr27(1)
    champpr(26) = ipr26(1)
    champpr(25) = ipr25(1)
    champpr(24) = ipr24(1)
    champpr(23) = ipr23(1)
    champpr(22) = ipr22(1)
    champpr(21) = ipr21(1)
    champpr(20) = ipr20(1)
    champpr(19) = ipr19(1)
    champpr(18) = ipr18(1)
    champpr(17) = ipr17(1)
    champpr(16) = ipr16(1)
    champpr(15) = ipr15(1)
    champpr(14) = ipr14(1)
    champpr(13) = ipr13(1)
    champpr(12) = ipr12(1)
    champpr(11) = ipr11(1)
    champpr(10) = ipr10(1)
    champpr(9)  = ipr9(1)
    champpr(8)  = ipr8(1)
    champpr(7)  = ipr7(1)
    champpr(6)  = ipr6(1)
    champpr(5)  = ipr5(1)
    champpr(4)  = ipr4(1)
    champpr(3)  = ipr3(1)
    champpr(2)  = ipr2(1)

    if (nchamp >= 2) then
        champpr(1)  = ipr1(1)
    else
        champpr(1)  = -1
    endif

    kinds(30) = ipr30(2)
    kinds(29) = ipr29(2)
    kinds(28) = ipr28(2)
    kinds(27) = ipr27(2)
    kinds(26) = ipr26(2)
    kinds(25) = ipr25(2)
    kinds(24) = ipr24(2)
    kinds(23) = ipr23(2)
    kinds(22) = ipr22(2)
    kinds(21) = ipr21(2)
    kinds(20) = ipr20(2)
    kinds(19) = ipr19(2)
    kinds(18) = ipr18(2)
    kinds(17) = ipr17(2)
    kinds(16) = ipr16(2)
    kinds(15) = ipr15(2)
    kinds(14) = ipr14(2)
    kinds(13) = ipr13(2)
    kinds(12) = ipr12(2)
    kinds(11) = ipr11(2)
    kinds(10) = ipr10(2)
    kinds(9)  = ipr9(2)
    kinds(8)  = ipr8(2)
    kinds(7)  = ipr7(2)
    kinds(6)  = ipr6(2)
    kinds(5)  = ipr5(2)
    kinds(4)  = ipr4(2)
    kinds(3)  = ipr3(2)
    kinds(2)  = ipr2(2)
    kinds(1)  = ipr1(2)

    nw = min(argdims(1), 2)

    if (nw == 1) then
        if (nom(1) == -1) then
            cnom = ' '
        else
            write(cnom, '(A4)') nom(1)
        endif
    else
        write(cnom, '(3A4)') (nom(i), i=1, nw)
    endif
    call low2up(cnom, cnom)

    nchmp = nchamp

    nparm = max0(1, nchmp - 1)

    do i=1, nparm
        if (argdims(i+1) > 1) then
            ptr = transfer(champpr(i), ptr)
            call convip_plus(champpr(i), ptr, -1*kinds(i)-1000, ip1style, string, .false.)
        endif
    enddo

    if (.not. associated(tmplat) .and. cgrtyp /= '*') then
        if (message) then
            call app_log(APP_WARNING, 'champ: Grid not defined, will use PS(2805)')
        endif
        ngr = 8
        call grille2(3, 51, 55, 26., 28., 381000., 350., 1)
    endif

    ! VERIFIER SI DIRECTIVE HEURE EXISTE OBLIGATOIRE AVEC CHAMP
    if (nhur == 0) then
        if (cnom /= 'DFPR'.or.cnom /= 'DFST') then
            call app_log(APP_ERROR, 'champ: HEURE directive must be defined')
            return
        endif
    endif

    if (npair > NPAIRMX) npair = NPAIRMX

    do ihrs = 1, nhur
        ihr = heures(ihrs)

        ! CALCUL DES VECTEURS OU DE LA VITESSE DU VENT
        trouve = 0
        do np = 1, npair
            if (cnom == paire(np)(1:8)) trouve=np
        enddo

        ! SI ON A TROUVE ON VA A L'INTERPOLATION
        if (trouve  /=  0) then
            vvent  = .false.
            wdvent = .false.
            if (paire(trouve)(17:20) /= '??  ') then
                vvent= .true.
            endif
            if (paire(trouve)(21:24) == 'WD  ') then
                wdvent= .true.
                vvent = .true.
            endif

            if (cgrtyp == '*') then
                call app_log(APP_WARNING, 'champ: GRILLE(AUCUNE) only works for variables PCP, EPAIS, DFST ET NUAG')
            else
                call uvectur(paire(trouve)(9:12), paire(trouve)(13:16), paire(trouve)(17:20), ihr, nparm, champpr)
            endif

        else if (cnom == 'DFGZ') then
            ! CALCUL LA DIFFERENCE ENTRE DEUX "GZ"
            call epaisur(ihr, nparm, champpr)

        else if (cnom == 'DFST') then
            ! CALCUL LA DIFFERENCE ENTRE DEUX "ST ACCUMULATEUR D'AJUSTEMENT"
            if (ihrs == 1)   then
                call macpcp('ST  ', nparm, champpr)
                if (message) then
                if (nhur>1) call app_log(APP_WARNING, 'champ: HEURE directive not necessary (ST)')
                endif
            endif

        else if (cnom == 'DFPR') then
            ! CALCUL LA DIFFERENCE ENTRE DEUX "PR" PRECIPITATION
            if (ihrs == 1) then
                call macpcp('PR  ', nparm, champpr)
                if (message) then
                if (nhur>1) call app_log(APP_WARNING, 'champ: HEURE directive not necessary (PRECIP)')
                endif
            endif

        else if (cnom == 'NUAG') then
            ! INTERPOLATION DES NUAGES BAS, MOYEN, HAUT
            call scalair('NB  ', ihr, 1, champpr)
            call scalair('NM  ', ihr, 1, champpr)
            call scalair('NH  ', ihr, 1, champpr)

        else
            ! AUTRE NOM  (GZ, TT, DD, WW, QQ, ES, DZ, ST, PR........)
            if (cgrtyp == '*') then
                call app_log(APP_WARNING, 'champ: GRILLE(AUCUNE) only works for variables PCP, EPAIS, DFST ET NUAG')
            else
                call scalair(cnom, ihr, nparm, champpr)
            endif
        endif

    enddo
end
