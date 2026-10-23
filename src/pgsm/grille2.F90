!> Determine la sorte de grille demande par usager
subroutine grille2(it, p1, p2, p3, p4, p5, p6, p7, p8)
    use app
    implicit none

    integer :: it, p1, p2, p3, p4, p5, p6, p7, p8

    !AUTEUR   - P. SARRAZIN JANVIER 82 DRPN DORVAL P.Q. CANADA
    !           MODIFIER JANVIER 87 P.SARRAZIN DORVAL P.Q. CANADA
    !           MODIFIER MAI 87 P. SARRAZIN DORVAL P.Q. CANADA
    !
    !OBJET(GRILLE)
    !          VERIFI  LE NOMBRE D ARGUMENTS LA VALEUR DU PREMIER ARGUMENT
    !          DETERMINE LE TYPE DE GRILLE DE SORTIE, RETOURNE LA MEMOIRE
    !          POUR IXLAT INITIALISE A ZERO DANS LE MAIN PGSM.
    !
    !ARGUMENTS
    !   IN   IT    - 1= GRILLE STANDARD
    !                2= GRILLE LAT-LON
    !                3= GRILLE P.S. POLAIRE STEREOGRAPHIQUE
    !                4= TAPE4 FICHIER CONTENANT LATITUDES LONGITUDES
    !                 OU COORDONNEES EST-OUEST OU NORD-SUD
    !                5- STDB GRILLE STANDARD 'B'
    !                6= GRILLE GAUSSIENNE
    !                7= GRILLE TAPE1 1 REC LAT, Y  1 REC LON, X
    !                8= GRILLE TAPE2 ECRIT 1 REC LAT, Y 1 REC LON, X SUR TAPE2
    !                9= GRILLE GEF
    !               10= GRILLE GRIB
    !               11= COORDONNEES LOCALES

#include "llccmm.cdk90"
#include "indptr.cdk90"
#include "grilles.cdk90"

    external :: comme, pgsmabt, grilstd, grlalon, grillps, griltp4, grigaus, gristdb, grigib, grigef, gristereo, gritp12, grigrib

    real :: xlat1
    real :: xlon1
    real :: xlat2
    real :: xlon2
    real :: pi
    real :: pj
    real :: d60
    real :: dgrw

    if (iset == -2) then
        call app_log(APP_ERROR, 'grille: Directive SORTIE has to be called before directive GRILLE')
        call pgsmabt
    endif

    cgtypxy = 'L'

    if (associated(tmplat)) deallocate(tmplat)
    if (associated(tmplon)) deallocate(tmplon)
    if (associated(tmplatg)) deallocate(tmplatg)
    if (associated(tmplong)) deallocate(tmplong)

    if (it < 0 .or. it > 14) then
        call app_log(APP_ERROR, 'grille: Unknown grid')
        call pgsmabt
    endif

    if (it == gr_a) then
        ! CALCUL GRILLE STD
        if (ngr == 4) then
            ! NI =P1  NOMBRE DE POINTS EST-OUEST
            ! NJ =P2  NOMBRE DE POINTS NORD SUD
            ! LG1=P3  0=GLOBAL;  1=H. NORD;  2=H. SUD
            ! grilstd(integer, integer, integer)
            call grilstd(p1, p2, p3)
        else
            call app_log(APP_ERROR, 'grille: Wrong GRILLE(STD, NI, NJ, NORD/SUD/GLOBAL) call')
            call pgsmabt
        endif

    elseif (it == gr_latlon) then
        ! CALCUL GRILLE LATLON
        if (ngr == 7) then
            ! NI=P1      NOMBRE DE POINTS EST-OUEST
            ! NJ=P2      NOMBRE DE POINTS NORD-SUD
            ! XLAT0=P3   1 IERE LAT EN BAS A GAUCHE
            ! XLON0=P4   1 IERE LONG EN BAS A GAUCHE
            ! DLAT=P5    ESPACEMENT ENTRE CHAQUE LATITUDE
            ! DLON=P6    ESPACEMENT ENTRE CHAQUE LONGITUDE
            ! grlalon(integer, integer, real, real, real real)
            xlat1 = transfer(p3, xlat1)
            xlon1 = transfer(p4, xlon1)
            xlat2 = transfer(p5, xlat2)
            xlon2 = transfer(p6, xlon2)
            call grlalon(p1, p2, xlat1, xlon1, xlat2, xlon2)
        else
            call app_log(APP_ERROR, 'grille: Wrong GRILLE(LATLON, NI, NJ, XLAT0, XLON0, DLAT, DLON) call')
            call pgsmabt
        endif

    elseif (it == gr_ps) then
        ! CALCUL GRILLE PS

        ! NI=P1.........NOMBRE DE POINTS DANS DIRECTION EST-OUEST
        ! NJ=P2.........NOMBRE DE POINTS DANS DIRECTION NORD-SUD
        ! PI=P3.........POSITION DU POLE DIRECTION EST-OUEST (GRID POINT)
        ! PJ=P4.........POSITION DU POLE DIRECTION NORD SUD (GRID POINT)
        ! D60=P5........DISTANCE ENTRE 2 GRID POINTS EN METRES
        ! DGRW=P6.......ORIENTATION DE LA GRILLE PAR RAPPORT A GREENWICH
        ! NORD/SUD=P7...HEMISPHERE NORD/SUD
        ! grillps(integer, integer, real, real, real, real, integer)
        pi = transfer(p3, pi)
        pj = transfer(p4, pj)
        d60 = transfer(p5, d60)
        dgrw = transfer(p6, dgrw)
        if (ngr == 7) then
            call grillps(p1, p2, pi, pj, d60, dgrw, 1)
        else if (ngr == 8) then
            call grillps(p1, p2, pi, pj, d60, dgrw, p7)
        else
            call app_log(APP_ERROR, 'grille: Wrong GRILLE(PS, NI, NJ, PI, PJ, D60, DGRW, NORD/SUD) call')
            call pgsmabt
        endif

    elseif (it == gr_tape4) then
        ! CALCUL GRILLE TAPE4

        ! NI=P1.....NOMBRE DE POINTS EST-OUEST
        ! NJ=P2.....NOMBRE DE POINTS NORD-SUD
        ! IP1=P3....VALEUR DE IP1 TRANSFER DANS IG1 POUR TAPE2
        ! IP2=P4....VALEUR DE IP2 TRANSFER DANS IG2 POUR TAPE2
        ! IP3=P5....VALEUR DE IP3 TRANSFER DANS IG3 POUR TAPE2
        ! griltp4(integer, integer, integer, integer, integer)
        if (ngr == 6) then
            call griltp4(p1, p2, p3, p4, p5)
        else if (ngr == 3) then
            call griltp4(p1, p2, -1, -1, -1)
        else
            call app_log(APP_ERROR, 'grille: Wrong GRILLE(TAPE4, NI, NJ [, IP1, IP2, IP3]) call, has to have 3 or 6 arguments')
            call pgsmabt
        endif

    elseif (it == gr_g) then
        ! CALCUL GRILLE GAUSSIENNE
        if (ngr == 4) then
            if (mod(p1, 2) .ne. 0) then
                call app_log(APP_ERROR, 'grille: Cannot produce a gaussian field with and odd number of longitudes')
                call pgsmabt
            endif
            ! NI =P1  NOMBRE DE POINTS EST-OUEST
            ! NJ =P2  NOMBRE DE POINTS NORD SUD
            ! LG1=P3  0=GLOBAL;  1=H. NORD;  2=H. SUD
            ! grigaus(integer, integer, integer)
            call grigaus(p1, p2, p3)
        else
            call app_log(APP_ERROR, 'grille: Wrong GRILLE(GAUSS, NI, NJ, NORD/SUD/GLOBAL) call')
            call pgsmabt
        endif

    elseif (it == gr_b) then
        ! CALCUL GRILLE STDB
        if (ngr == 4) then
            ! NI =P1  NOMBRE DE POINTS EST-OUEST
            ! NJ =P2  NOMBRE DE POINTS NORD SUD
            ! LG1=P3  0=GLOBAL;  1=H. NORD;  2=H. SUD
            ! gristdb(integer, integer, integer)
            call gristdb(p1, p2, p3)
        else
            call app_log(APP_ERROR, 'grille: Wrong GRILLE(STDB, NI, NJ, NORD/SUD/GLOBAL) call')
            call pgsmabt
        endif

    elseif (it == gr_tape1 .or. it == gr_tape2 .or. it == gr_stations) then
        ! CALCUL GRILLE TAPE1/TAPE2 LAT-LON OU X-Y
        if (ngr == 4) then
            ! IT=7......LIRE TAPE 1 ECRIT SUR TAPE2
            ! IT=8......LIRE TAPE 2 ECRIT SUR TAPE2
            ! IP1=P1....IDENTIFICATION DU RECORD VALEUR MAX=2047
            ! IP2=P2....IDENTIFICATION DU RECORD VALEUR MAX=2047
            ! IP3=P3....IDENTIFICATION DU RECORD VALEUR MAX=2047
            ! gritp12(integer, integer, integer, integer)
            call gritp12(it, p1, p2, p3)
        else
            call app_log(APP_ERROR, 'grille: Wrong GRILLE(TAPE1/TAPE2, IP1, IP2, IP3) call')
            call pgsmabt
        endif

    elseif (it == gr_comme) then
        if (ngr == 9) then
            ! comme(integer, integer, integer, integer, integer, integer, integer, integer(3))
            call comme(p1, p2, p3, p4, p5, p6, p7, p8)
        else
            call app_log(APP_ERROR, 'grille: Wrong GRILLE(COMME, FENTREE/FSORTIE, NOMVAR, TYPVAR, DATEV, IP1, IP2, IP3, ETIKET) call')
            call pgsmabt
        endif

    elseif (it == gr_grib) then
        if (ngr == 5) then
            ! grigrib(integer, integer, integer, integer)
            call grigrib(p1, p2, p3, p4)
        else
            call app_log(APP_ERROR, 'grille: Wrong GRILLE(GRIB, IG1, iG2, IG3, IG4) call')
            call pgsmabt
        endif

    elseif (it == 15) then
        if (ngr == 7) then
            ! IT=7......LIRE TAPE 1 ECRIT SUR TAPE2
            ! IT=8......LIRE TAPE 2 ECRIT SUR TAPE2
            ! IP1=P1....IDENTIFICATION DU RECORD VALEUR MAX=2047
            ! IP2=P2....IDENTIFICATION DU RECORD VALEUR MAX=2047
            ! IP3=P3....IDENTIFICATION DU RECORD VALEUR MAX=2047
            ! grigef(integer, integer, real, real, real, real)
            xlat1 = transfer(p3, xlat1)
            xlon1 = transfer(p4, xlon1)
            xlat2 = transfer(p5, xlat2)
            xlon2 = transfer(p6, xlon2)
            call grigef(p1, p2, xlat1, xlon1, xlat2, xlon2)
        else
            call app_log(APP_ERROR, 'grille: Wrong GRILLE(E, NI, NJ, XLAT1, XLON1, XLAT2, XLON2) call')
            call pgsmabt
        endif

    elseif (it == gr_stereo) then
        if (ngr == 7) then
            ! gristereo(integer, integer, real, real, real, real)
            pi = transfer(p3, pi)
            pj = transfer(p4, pj)
            d60 = transfer(p5, d60)
            dgrw = transfer(p6, dgrw)
            call gristereo(p1, p2, pi, pj, d60, dgrw)
        else
            call app_log(APP_ERROR, 'grille: Wrong GRILLE(STEREO, NI, NJ, D60, DGRW, CLAT, CLON) call')
            call pgsmabt
        endif
    endif
end
