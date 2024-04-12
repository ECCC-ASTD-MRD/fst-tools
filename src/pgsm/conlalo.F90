!> Calculer lat long de chaque pt d'une grille type "y" ou "z"
subroutine conlalo(lat, lon, ni, nj, grtyp, grtypxy, ig1, ig2, ig3, ig4)
    use app
    implicit none

    external conlal2

    integer lat, lon, ni, nj, grtyp, grtypxy, ig1, ig2, ig3, ig4
    character*1 cgrtyp, cgtypxy

    write(cgrtyp , '(A1)') grtyp
    write(cgtypxy, '(A1)') grtypxy

    WRITE(app_msg, 101) cgrtyp, cgtypxy
    call app_log(APP_INFO, app_msg)
101  format(' CONLALO:', 'CGRTYP: ', a1, 'CGTYPXY: ', a1)

    call conlal2(lat, lon, ni, nj, cgrtyp, cgtypxy, ig1, ig2, ig3, ig4)
end


!> Calculer la latitude et la longitude de tous les points de la grille de sortie de type "y" ou "z"
subroutine conlal2(lat, lon, ni, nj, cgrtyp, cgtypxy, ig1, ig2, ig3, ig4)
    use app
    implicit none

    external cigaxg, llfxy, pgsmabt, messags

    integer ni, nj, ig1, ig2, ig3, ig4, i, j, hem
    real lat(ni, nj), lon(ni, nj), dlat, lat0, dlon, lon0
    real pii, pjj, d60, dgrw, buflat, buflon, dla, dlo

    character*1 cgrtyp, cgtypxy


    if (cgrtyp == 'Z') then
        ! Attention boucle suivante nj permet d'avoir des ni>nj sans probleme
        do j = nj, 1, -1
            lat(1, j) = lat(j, 1)
        enddo

        do i = 1, ni
            do j = 1, nj
                lat(i, j) = lat(1, j)
                lon(i, j) = lon(i, 1)
            enddo
        enddo
    endif

    hem = 1
    if (cgtypxy == 'S') hem = 2

    if (cgtypxy == 'N'.or.cgtypxy == 'S')  then
        call cigaxg(cgtypxy, pii, pjj, d60, dgrw, ig1, ig2, ig3, ig4)

        do i = 1, ni
            do j = 1, nj
                buflat = lat(i, j) - pjj
                buflon = lon(i, j) - pii
                call llfxy(dla, dlo, buflon, buflat, d60, dgrw, hem)
            if (dlo.le.0.0) dlo = dlo + 360.0
                lat(i, j) = dla
                lon(i, j) = dlo
            enddo
        enddo
    else if (cgtypxy == 'L') then
        call cigaxg(cgtypxy, lat0, lon0, dlat, dlon, ig1, ig2, ig3, ig4)

        do i = 1, ni
            do j = 1, nj
                lat(i, j) = lat(i, j) * dlat + lat0
                lon(i, j) = lon(i, j) * dlon + lon0
            enddo
        enddo
    else
        call app_log(APP_ERROR, 'conlalo: GRILLE not "N", "S", "L"')
        call pgsmabt
    endif
end



!> CALCULER LA LATITUDE ET LA LONGITUDE DE TOUS LES POINTS DE LA GRILLE DE SORTIE DE TYPE "Y" OU "Z"
subroutine conlale(lat, lon, latg, long, ni, nj, cgrtyp, cgtypxy, ig1, ig2, ig3, ig4)
    use app
    implicit none

    external cigaxg, llfxy, pgsmabt, messags

    integer ni, nj, ig1, ig2, ig3, ig4, i, j
    real lat(ni, nj), lon(ni, nj), latg(ni, nj), long(ni, nj)
    real xlat1, xlon1, xlat2, xlon2

    character*1 cgrtyp, cgtypxy


    if (cgrtyp == 'Z') then
        do j = nj, 1, -1
            lat(1, j) = lat(j, 1)
        enddo

        do i = 1, ni
            do j = 1, nj
                latg(i, j) = lat(1, j)
                long(i, j) = lon(i, 1)
            enddo
        enddo

        call cigaxg(cgtypxy, xlat1, xlon1, xlat2, xlon2, ig1, ig2, ig3, ig4)
        call ez_gfllfxy(lon, lat, long, latg, ni*nj, xlat1, xlon1, xlat2, xlon2)
    else
        call app_log(APP_ERROR, 'conlalo: GRILLE not "E"')
        call pgsmabt
    endif
end
