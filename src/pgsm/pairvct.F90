!> REMPLACE OU AJOUTE NOM AU DICTIONNAIRE COMMON/PAIR/...
subroutine pairvct(nomusag, varuu, varvv, varmodule, vardir)
    use app
    use pairs, only : npairuv
    implicit none

    !> Name given by the user in the PAIRES directive
    integer, intent(in) :: nomusag(2)
    !> Name of the first variable in the pair
    integer, intent(in) :: varuu
    !> Name of the second variable in the pair
    integer, intent(in) :: varvv
    ! SI VARMODULE .NE. 0, NOM DE 2 CARACTERES IDENTIFIANT LE CHAMP DE SORTIE VITESSE DU VENT  EX:"UV"
    ! SI VARMODULE .EQ. 0, INTERPOLATION DE 2 CHAMPS AVEC ORIENTATION GEOGRAPHIQUE
    ! NOM DU PREMIER CHAMP POUR LA SORTIE=VARUU
    ! NOM DU DEUXIEME CHAMP=VARVV
    integer, intent(in) :: varmodule
    integer, intent(in) :: vardir

    !          REMPLACE OU AJOUTE DANS LA TABLE PAIRE DU COMMON/PAIR/..
    !          POUR REFERENCE PAR L'USAGER QUI PERMET CERTAINES INTERPOLATIONS
    !          DE VARIABLES PAIRES. 2 SETS DE VARIABLES PAIRES INITIALISE
    !          DANS PGSM UU, VV  US, VS.

    !APPEL
    !         - VIA DIRECTIVE PAIRES(NOMUSAG, VARUU, VARVV, VARMODULE)

    integer, external :: argdims

    character(len = 8) :: cnomusr
    character(len = 4) :: cvaruu
    character(len = 4) :: cvarvv
    character(len = 4) :: ccontrl
    character(len = 4) :: cvarwd
    integer :: i, nw

    if (npairuv < 3 .or. npairuv > 5) then
        call app_log(APP_ERROR, 'pairvct: Check PAIRES directives for 3 or 4 arguments')
        return
    endif

    nw = min(argdims(1), 2)
    write (cnomusr, '(2a4)') (nomusag(i), i=1, nw)

    write (cvaruu, '(a4)') varuu
    write (cvarvv, '(a4)') varvv
    write (ccontrl, '(a4)') varmodule
    write (cvarwd, '(a4)') vardir

    if (varmodule == 0) ccontrl = '??'
    if (vardir == 0) cvarwd = '??'

    write (app_msg, *) 'pairvct: PAIRES: ', cnomusr, cvaruu, cvarvv, ccontrl, cvarwd
    call app_log(APP_INFO, app_msg)

    call pairvc2(cnomusr, cvaruu, cvarvv, ccontrl, cvarwd)
end


!> Replace or add a pair to the list
subroutine pairvc2(cnomusr, cvaruu, cvarvv, ccontrl, cvarwd)
    use app
    use pairs, only : npair, paire, mxpairs
    implicit none

    character(len = 8), intent(in) :: cnomusr
    character(len = 4), intent(in) :: cvaruu
    character(len = 4), intent(in) :: cvarvv
    character(len = 4), intent(in) :: ccontrl
    character(len = 4), intent(in) :: cvarwd

    ! Appel via directive PAIRES(NOMUSAG, VARUU, VARVV, VARMODULE)
    integer :: np
    logical :: remplac

    ! Verifier si nom existe dans la table si oui on remplace si non on ajoute si la table n'est pas pleine
    remplac = .false.
    do np = 1, npair
        if (cnomusr == paire(np)(1:8)) then
            paire(np)( 1: 8) = cnomusr
            paire(np)( 9:12) = cvaruu
            paire(np)(13:16) = cvarvv
            paire(np)(17:20) = ccontrl
            paire(np)(21:24) = cvarwd
            remplac = .true.
            write (app_msg, *) 'pairvct: PAIRE(NP): ', paire(np)
            call app_log(APP_INFO, app_msg)
        endif
    enddo

    if (remplac) then
        call app_log(APP_INFO, 'pairvct: 2 pair variables replaced')
        return
    endif

    ! Si on n'a pas remplace dans la table on ajoute
    npair = npair + 1
    if (npair > mxpairs) then
        write(app_msg, "(1x, 'pairvct: Too many pairs in table  NPAIR=', i5, /'   NPAIRMX=', i5)") npair, mxpairs
        call app_log(APP_ERROR, app_msg)
        return
    endif

    paire(np)( 1: 8) = cnomusr
    paire(np)( 9:12) = cvaruu
    paire(np)(13:16) = cvarvv
    paire(np)(17:20) = ccontrl
    paire(np)(21:24) = cvarwd
    write (app_msg, *) 'pairvct: PAIRE(NP): ', paire(np)
    call app_log(APP_INFO, app_msg)
end
