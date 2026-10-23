!**S/P ECRIRE SUR TAPE2 1 REC LATITUDES ET 1 REC LONGITUDES
subroutine outlalo(ip1, ip2, ip3)
    use app
    implicit none

    integer ip1, ip2, ip3

    !AUTEUR P. SARRAZIN AOUT 84 DRPN DORVAL P.Q. CANADA

    !OBJET(OUTLALO)
    !        EXTRAIRE DANS LA MEMOIRE LES LATITUDES ET LES LONGITUDES
    !        ET CALCUL TOUS LES PARAMETRES NECESSAIRES POUR CALL ECRIRE
    !        SUR UN FICHIER STANDARD TAPE2

    !ARGUMENTS
    !  IN    IP1     OPTIONEL POUR USAGER DEFAUT = 0
    !  IN    IP2     OPTIONEL POUR USAGER DEFAUT = 0
    !  IN    IP3     OPTIONEL POUR USAGER DEFAUT = 0

    external :: pgsmabt, ecritur, grille2, messags
    integer, external :: gdll

#define  between(a, b, c) MIN(c, MAX(b, a))
#include "llccmm.cdk90"
#include "dates.cdk90"
#include "charac.cdk90"
#include "indptr.cdk90"
#include "grilles.cdk90"
#include "voir.cdk90"
#include "packin.cdk90"

    integer jjp1, jjp2, jjp3, jjp4, jp1, jp2, jp3, k, ier

    if (.not. associated(tmplat)) then
        if (message) call app_log(APP_WARNING, 'outlalo: Grid not defined, will use GRILLE P.S.(2805)')
        ngr = 8
        ! Just using the jjp* as temporary containers of the right type
        jjp1 = transfer(26.0, jjp1)
        jjp2 = transfer(28.0, jjp2)
        jjp3 = transfer(381000.0, jjp3)
        jjp4 = transfer(350.0, jjp4)
        call grille2(gr_ps, 51, 55, jjp1, jjp2, jjp3, jjp4, 1, 0)
    endif

    if (nlalo .gt. 3) then
        call app_log(APP_WARNING, 'outlalo: More then 3 arguments to OUTLALO, will only use first 3')
    endif

    ! INITIALISER ARGUMENTS POUR ECRITUR
    ! lat lon 2 dimension seulement
    k = 1

    jp1 = ip1
    jp2 = ip2
    jp3 = ip3

    if (nlalo.eq.1) then
        jp2 = 0
        jp3 = 0
    endif

    if (nlalo.eq.2) jp3 = 0

    jjp1 = between(0, jp1, 32767)
    jjp2 = between(0, jp2, 32767)
    jjp3 = between(0, jp3, 4095)

    ier = gdll(gdout, tmplat, tmplon)

    call ecritur(tmplat, npack, jdate, 0, 0, li, lj, k, jjp1, jjp2, jjp3, 'C ', 'LA  ', 'LATITUDES   ', cgrtyp, lg1, lg2, lg3, lg4)

    !     INITIALISER POUR LONGITUDES
    call ecritur(tmplon, npack, jdate, 0, 0, li, lj, k, jjp1, jjp2, jjp3, 'C ', 'LO  ', 'LONGITUDES  ', cgrtyp, lg1, lg2, lg3, lg4)
end
