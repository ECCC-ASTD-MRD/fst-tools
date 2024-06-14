!> Determiner la sorte de grille demande par usager
subroutine grille2(it, p1, p2, p3, p4, p5, p6, p7, p8)
    use app
    use pgsm_mod, only: tmplat, tmplon, tmplatg, tmplong
    use grilles, only : cgrtyp, ngr
    implicit none

    !> Grid type
    ! 1= grille standard
    ! 2= grille lat-lon
    ! 3= grille p.s. polaire stereographique
    ! 4= tape4 fichier contenant latitudes longitudes ou coordonnees est-ouest ou nord-sud
    ! 5- stdb grille standard 'b'
    ! 6= grille gaussienne
    ! 7= grille tape1 1 rec lat, y  1 rec lon, x
    ! 8= grille tape2 ecrit 1 rec lat, y 1 rec lon, x sur tape2
    ! 9= grille gef
    ! 10= grille grib
    ! 11= coordonnees locales
    integer, intent(in) :: it
    integer, intent(in) :: p1, p2, p3, p4, p5, p6, p7, p8

    external pgsmabt, grilstd, grlalon, grillps, griltp4, grigaus, gristdb, gritp12

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
            call grlalon(p1, p2, p3, p4, p5, p6)
        else
            call app_log(APP_ERROR, 'grille: Wrong GRILLE(LATLON, NI, NJ, XLAT0, XLON0, DLAT, DLON) call')
            call pgsmabt
        endif

    elseif (it == gr_ps) then
        ! CALCUL GRILLE PS
        if (ngr == 7) then
            ! NI=P1.........NOMBRE DE POINTS DANS DIRECTION EST-OUEST
            ! NJ=P2.........NOMBRE DE POINTS DANS DIRECTION NORD-SUD
            ! PI=P3.........POSITION DU POLE DIRECTION EST-OUEST (GRID POINT)
            ! PJ=P4.........POSITION DU POLE DIRECTION NORD SUD (GRID POINT)
            ! D60=P5........DISTANCE ENTRE 2 GRID POINTS EN METRES
            ! DGRW=P6.......ORIENTATION DE LA GRILLE PAR RAPPORT A GREENWICH
            ! NORD/SUD=P7...HEMISPHERE NORD/SUD
            call grillps(p1, p2, p3, p4, p5, p6, 1)
        else if (ngr == 8) then
            call grillps(p1, p2, p3, p4, p5, p6, p7)
        else
            call app_log(APP_ERROR, 'grille: Wrong GRILLE(PS, NI, NJ, PI, PJ, D60, DGRW, NORD/SUD) call')
            call pgsmabt
        endif

    elseif (it == gr_tape4) then
        ! CALCUL GRILLE TAPE4
        if (ngr == 6) then
            call griltp4(p1, p2, p3, p4, p5)
        else if (ngr == 3) then
            ! NI=P1.....NOMBRE DE POINTS EST-OUEST
            ! NJ=P2.....NOMBRE DE POINTS NORD-SUD
            ! IP1=P3....VALEUR DE IP1 TRANSFER DANS IG1 POUR TAPE2
            ! IP2=P4....VALEUR DE IP2 TRANSFER DANS IG2 POUR TAPE2
            ! IP3=P5....VALEUR DE IP3 TRANSFER DANS IG3 POUR TAPE2
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
            call gristdb(p1, p2, p3)
        else
            call app_log(APP_ERROR, 'grille: Wrong GRILLE(STDB, NI, NJ, NORD/SUD/GLOBAL) call')
            call pgsmabt
        endif
    elseif (it == gr_tape1 .or. it == gr_tape2 .or. it == gr_stations) then
        ! CALCUL GRILLE TAPE1/TAPE2 LAT-LON OU X-Y

        ! SI IT=7 TAPE1 ENTRE
        ! SI IT=8 TAPE2 SORTI
        if (ngr == 4) then
            ! IT=7......LIRE TAPE 1 ECRIT SUR TAPE2
            ! IT=8......LIRE TAPE 2 ECRIT SUR TAPE2
            ! IP1=P1....IDENTIFICATION DU RECORD VALEUR MAX=2047
            ! IP2=P2....IDENTIFICATION DU RECORD VALEUR MAX=2047
            ! IP3=P3....IDENTIFICATION DU RECORD VALEUR MAX=2047
            call gritp12(it, p1, p2, p3)
        else
            call app_log(APP_ERROR, 'grille: Wrong GRILLE(TAPE1/TAPE2, IP1, IP2, IP3) call')
            call pgsmabt
        endif

    elseif (it == gr_comme) then
        if (ngr == 9) then
            call comme(p1, p2, p3, p4, p5, p6, p7, p8)
        else
            call app_log(APP_ERROR, 'grille: Wrong GRILLE(COMME, FENTREE/FSORTIE, NOMVAR, TYPVAR, DATEV, IP1, IP2, IP3, ETIKET) call')
            call pgsmabt
        endif

    elseif (it == gr_grib) then
        if (ngr == 5) then
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
            call grigef(it, p1, p2, p3, p4, p5, p6)
        else
            call app_log(APP_ERROR, 'grille: Wrong GRILLE(E, NI, NJ, XLAT1, XLON1, XLAT2, XLON2) call')
            call pgsmabt
        endif

    elseif (it == gr_stereo) then
        if (ngr == 7) then
            call gristereo(p1, p2, p3, p4, p5, p6)
        else
            call app_log(APP_ERROR, 'grille: Wrong GRILLE(STEREO, NI, NJ, D60, DGRW, CLAT, CLON) call')
            call pgsmabt
        endif
    endif
end
