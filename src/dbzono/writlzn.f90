subroutine writlzn(ideu, s, sh, nsig, tabctl, nombrec, dateo, delt, npax, etiket, dlat, nlat, tourne)
    use app
    use rmn_libc, only: c_exit

    implicit none

    !Auteur: G.Pellerin (AUG93)
    !         001 B.Dugas    (mar06) Declarations pour FSTD2000
    !
    !revision 001 G.PELLERIN -Dec93- Cle Tourne pour rotation de grille

    !      ecrit les niveaux sigma et niveaux intermediaires
    !      ecrit les angles selon longitude pour graphisme
    !      dans le fichier standard ZONFILE

    integer, intent(in) :: ideu
    integer, intent(in) :: nsig
    real, intent(inout) :: s(nsig)
    real, intent(in) :: sh(nsig)
    integer, intent(in) :: nombrec
    integer, intent(in) :: tabctl(nombrec)
    integer, intent(in) :: dateo
    integer, intent(in) :: delt
    integer, intent(in) :: npax
    character(len = 12), intent(in) :: etiket
    integer, intent(in) :: nlat
    real, intent(in) :: dlat(nlat)
    logical, intent(in) :: tourne

    integer, external :: fstouv, fstecr, fstfrm

    integer, parameter :: ip1 = 0
    integer, parameter :: ip2 = 0
    integer, parameter :: ip3 = 0
    integer, parameter :: njc = 1
    integer, parameter :: nkc = 1
    integer, parameter :: npas = 0
    integer, parameter :: npak = -24
    integer, parameter :: ig1 = 0
    integer, parameter :: ig2 = 0
    integer, parameter :: ig3 = 0
    integer, parameter :: ig4 = 0
    integer, parameter :: datyp = 1
    logical, parameter :: rewrit = .false.
    character(len = 2), parameter :: typvar = 'Z'

    integer, parameter :: maxlat = 256
    real :: temp(maxlat)

    integer :: ierr
    integer :: deet
    character(len = 2) :: nomvar
    character(len = 1) :: grtyp

    ! Hack to prevent warnings about unused parameters
    if (.false.) print *, delt, npax

    ! Equivalence avec le tableau de controle.
    deet = tabctl(2)

    ! declare le fichier cible
    ierr = fstouv(ideu, 'RND')

    ! definir les variables pour fstecr.
    if (.not. tourne) then
        grtyp = 'G'
    else
        grtyp = 'L'
    end if

    ! Ecrit les niveaux intermediaires pour le graphisme O
    nomvar  = 'S^'
    S(1) = 1.

    ierr = fstecr(s, temp,                    &
                  npak, ideu, dateo, deet, npas,   &
                  nsig, njc, nkc, ip1, ip2, ip3,  &
                  typvar, nomvar, etiket, grtyp,  &
                  ig1, ig2, ig3, ig4, datyp,       &
                  rewrit)
    if (ierr < 0) then
        call app_log(APP_ERROR, ' Unable to write "S^" sigma before closing down.')
        app_status = app_end(-1)
        call c_exit(1)
    end if

    ! Ecrit les niveaux intermediaires pour le graphisme O
    nomvar  = 'SH'
    ierr = fstecr(sh, temp,                  &
                  npak, ideu, dateo, deet, npas,  &
                  nsig, njc, nkc, ip1, ip2, ip3, &
                  typvar, nomvar, etiket, grtyp, &
                  ig1, ig2, ig3, ig4, datyp,      &
                  rewrit)
    if (ierr < 0) then
        call app_log(APP_ERROR, ' Unable to write "SH" sigma before closing down.')
        app_status = app_end(-1)
        call c_exit(2)
    end if

    ! Ecrit les latitudes si non regulieres pour le graphisme O
    nomvar = 'L>'
    if (.not. tourne) then
        ierr = fstecr(dlat, temp,                 &
                      npak, ideu, dateo, deet, npas,   &
                      nlat, njc, nkc, ip1, ip2, ip3,  &
                      typvar, nomvar, etiket, grtyp,  &
                      ig1, ig2, ig3, ig4, datyp,       &
                      rewrit)
        if (ierr < 0) then
            call app_log(APP_ERROR, ' Unable to write "L^" theta before closing down.')
            app_status = app_end(-1)
            call c_exit(3)
        end if
    end if

    ierr = fstfrm(ideu)
end
