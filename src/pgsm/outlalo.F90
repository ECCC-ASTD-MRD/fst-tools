!> Ecrire sur tape2 1 rec latitudes et 1 rec longitudes
subroutine outlalo(ip1, ip2, ip3)
    !> Extraire dans la memoire les latitudes et les longitudes et calculer tous les parametres necessaires
    !> pour call ecrire sur un fichier standard tape2

    use app
    use packing, only : npack
    use pgsm_mod, only : nlalo, tmplat, tmplon, message
    use grilles, only : cgrtyp, gdout, li, lj, lg1, lg2, lg3, lg4, ngr
    implicit none

    integer, intent(in) :: ip1
    integer, intent(in) ::ip2
    integer, intent(in) ::ip3

    external ecritur, grille2
    integer, external :: gdll

#define  between(a, b, c) MIN(c, MAX(b, a))

    integer :: jjp1, jjp2, jjp3, jp1, jp2, jp3, ier

    if (.not. associated(tmplat)) then
        if (message) call app_log(APP_WARNING, 'outlalo: Grid not defined, will use GRILLE P.S.(2805)')
        ngr = 8
        call grille2(3, 51, 55, 26., 28., 381000., 350., 1) 
    endif

    if (nlalo > 3) then
        call app_log(APP_WARNING, 'outlalo: More then 3 arguments to OUTLALO, will only use first 3')
    endif

    jp1 = ip1
    jp2 = ip2
    jp3 = ip3

    if (nlalo == 1) then
        jp2 = 0
        jp3 = 0
    endif

    if (nlalo == 2) jp3 = 0

    jjp1 = between(0, jp1, 32767)
    jjp2 = between(0, jp2, 32767)
    jjp3 = between(0, jp3, 4095)

    ier = gdll(gdout, tmplat, tmplon)

    call ecritur(tmplat, npack, 0, 0, 0, li, lj, 1, jjp1, jjp2, jjp3, 'C ', 'LA  ', 'LATITUDES   ', cgrtyp, lg1, lg2, lg3, lg4) 
    call ecritur(tmplon, npack, 0, 0, 0, li, lj, 1, jjp1, jjp2, jjp3, 'C ', 'LO  ', 'LONGITUDES  ', cgrtyp, lg1, lg2, lg3, lg4) 
end
