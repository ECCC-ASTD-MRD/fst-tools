!> Imprime champ lu sur fichier d entree ou de sortie
subroutine imprime(cnom, champ, ni, nj)
    use app
    implicit none

    !> Nomvar
    character(len = 4), intent(in) :: cnom
    !> East-West field dimension
    integer, intent(in) :: ni
    !> North-South field dimension
    integer, intent(in) :: nj
    !> Field to print
    real, intent(in) :: champ(ni, nj)

    ! IMPRIME AVEC LA DIRECTIVE PRINTEN RECORD LUE SUR FICHIER D ENTREE
    ! OU IMPRIME AVEC LA DIRECTIVE PRINTSR RECORD QUE L ON VA ECRIRE
    ! L USAGER CONTROLE LE NOMBRE DE LOCATIONS A IMPRIMER 
    ! FENETRE DU CHAMP A IMPRIMER DEFINIE PAR L'USAGER
    ! DANS LA DIRECTIVE PRINTEN/PRINTSR MODIFIE LE COMMON
    ! LIRES OU ECRIRES PRINTEN=OUI, NIS, NJS, NIF, NJF, NINC, NJNC
    ! NIS = POINT DE DEPART DANS LA DIRECTION I (EST-OUEST)
    ! NJS = POINT DE DEPART DANS LA DIRECTION J (NORD-SUD)
    ! NIF = DERNIER POINT DANS LA DIRECTION I (EST-OUEST)
    ! NJF = DERNIER POINT DANS LA DIRECTION J (NORD-SUD) 
    ! NINC= INTERVAL DANS LA DIRECTION I
    ! NJNC= INTERVAL DANS LA DIRECTION J

    !APPEL   VIA DIRECTIVE
    !  PRINTEN(OUI, NIS, NJS, NIF, NJF, NINC, NJNC)
    !  PRINTSR(OUI, NIS, NJS, NIF, NJF, NINC, NJNC)

    character(*), parameter :: fmtread = "(' PRINT CHAMP(LU) NOM=', a2, '  NIS=', i3, '  NJS=', i3, '  NIFF=', i3, '  NJFF=', i3, '  NINC=', i3, '  NJNC=', i3)"
    character(*), parameter :: fmtrow = "('  RANGEE NO ', i3)"
    character(*), parameter :: fmtval = "(1h , 10e13.5)"

    integer :: i, j, njff, niff
    integer, parameter :: nis = 1
    integer, parameter :: njs = 1
    integer, parameter :: nif = 1000
    integer, parameter :: njf = 1000
    integer, parameter :: ninc = 10
    integer, parameter :: njnc = 10

    niff = nif
    njff = njf
    if (nif > ni) niff = ni
    if (njf > nj) njff = nj

    write(app_msg, fmtread) cnom, nis, njs, niff, njff, ninc, njnc
    call app_log(APP_INFO, app_msg)

    do j = njs, njff, njnc
        write(app_msg, fmtrow) j
        call app_log(APP_INFO, app_msg)
        write(app_msg, fmtval) (champ(i, j), i = nis, niff, ninc)
        call app_log(APP_INFO, app_msg)
    enddo

    if (niff < nis)  then
        write(app_msg, *) ' NIS < NIF DIRECTIVE PRINTEN=OUI, NIS, NJS, NIF, NJF, NINC, NJNC'
        call app_log(APP_INFO, app_msg)
    endif

    if (njff < njs)  then
        write(app_msg, *) ' NJS < NJF DIRECTIVE PRINTEN=OUI, NIS, NJS, NIF, NJF, NINC, NJNC'
        call app_log(APP_INFO, app_msg)
    endif
end subroutine imprime


subroutine imprims(cnom, champ, ni, nj)
    use app
    use ecrires, only : niif, njjf, niis, njjs, niinc, njjnc
    implicit none

    !> Nomvar
    character(len = 4), intent(in) :: cnom
    !> East-West field dimension
    integer, intent(in) :: ni
    !> North-South field dimension
    integer, intent(in) :: nj
    !> Field to print
    real, intent(in) :: champ(ni, nj)

    character(*), parameter :: fmtwrite = "(' PRINT CHAMP(ECRIT) NOM=', a2, '  NIS=', i3, '  NJS=', i3, '  NIFF=', i3, '  NJFF=', i3, '  NINC=', i3, '  NJNC=', i3)"
    character(*), parameter :: fmtrow = "('  RANGEE NO ', i3)"
    character(*), parameter :: fmtval = "(1h , 10e13.5)"

    integer :: i, j, niifs, njjfs

    niifs = niif
    njjfs = njjf
    if (niif > ni) niifs = ni
    if (njjf > nj) njjfs = nj

    write(app_msg, fmtwrite) cnom, niis, njjs, niifs, njjfs, niinc, njjnc
    call app_log(APP_INFO, app_msg)

    do j = njjs, njjfs, njjnc 
        write(app_msg, fmtrow) j
        call app_log(APP_INFO, app_msg)
        write(app_msg, fmtval) (champ(i, j), i = niis, niifs, niinc)
        call app_log(APP_INFO, app_msg)
    enddo

    if (niifs < niis)  then
        write(app_msg, *) ' NIS < NIF DIRECTIVE PRINTSR=OUI, NIS, NJS, NIF, NJF, NINC, NJNC'
        call app_log(APP_INFO, app_msg)
    endif
    if (njjfs < njjs)  then
        write(app_msg, *) ' NJS < NJF DIRECTIVE PRINTSR=OUI, NIS, NJS, NIF, NJF, NINC, NJNC'
        call app_log(APP_INFO, app_msg)
    endif
end
