!> coupe zonale ou meridionale d un champ
subroutine coupzm(iunit, cnom, cjcoup)
    use app
    use rmn_fst24
    use packing, only : npack
    use pgsm_mod, only : nwetike, etikent, message, tmpif1, typeent, ip3ent
    use accum, only : npas
    use cfldinf, only : cnomvar, ctypvar, cigtyp, cetiket
    use nivos, only : nivospr, nmo
    use heuress, only : nhur, nheures
    use param, only : dat, deet
    implicit none

    !> 
    integer, intent(in):: iunit
    !> Nomvar
    character(len = 4), intent(in) :: cnom
    !> Coupe zonale='zon' meridionale='mer'
    character(len = 8), intent(in) :: cjcoup

    external calcul, ecritur, gauss, pgsmabt, imprime, loupmir, louptra, loupin1, messags
    integer, external :: fstopc

    !            faire une coupe zonale ou meridionale sur un champ dont
    !            le type de grille est "g"-"a"-"l"-"b"-"c"
    !            calcul pour une coupe meridionale sur un champ gaussien
    !            "g" est fait a partir de poids calcule par gaussg
    !            la moyenne zonale(est-ouest) contient nj points
    !            la moyenne meridionale(nord-sud) contient ni points

#include "defin.cdk90"

    real coa(500), w(500), sia(500), rad(500), poids(500), sinm2(500), sinm1(500), sin2(500), champ(1000)
    integer i, datev
    integer ihr, iheur, iprs, npres
    integer ilath, j, iopc, ni

    type(fst_file), pointer :: file
    type(fst_query) :: query
    type(fst_record) :: record

    if (fentree == iunit) then
        file = inputFiles(1)
    else if (fsortie == iunit) then
        file = outputFile
    else
        call app_log(APP_ERROR, 'lrsmdes: Unrecognized file! Must be FENTREE or FSORTIE')
        call pgsmabt
    end if

    cnomvar = cnom

    do ihr = 1, nhur
        iheur = heures(ihr)
    enddo

    npres = max0(1, nmo-2)
    do iprs = 1, npres
        ! identifier numero du record
        call chk_userdate(datev)

        ! modification de hollerith a caractere
        if (etikent(1) /= -1) then
            write(cetiket, '(3A4)') (etikent(i), i = 1, nwetike)
        else
            cetiket = '            '
        endif

        if (typeent /= -1) then
            write(ctypvar, '(A2)') typeent
        else
            ctypvar = '  '
        endif

        cigtyp = ' '
        query = file%new_query(datev = datev, etiket = cetiket, ip1 = nivospr(iprs), ip2 = iheur, ip3 = ip3ent, typvar = ctypvar, nomvar = cnomvar)
        if (query%find_next(record)) then
            write(app_msg, *)  'coupzm: Record does not exist, check directive MOYENT/MOYSRT IUNIT=', iunit, ' NIVEAU=', nivospr(iprs), &
                ' HEURE=', iheur, 'NOM=', cnomvar
            call app_log(APP_ERROR, app_msg)
            return
        endif
        call query%free()

        if (record%nk > 1) then
            call app_log(APP_ERROR, 'coupzm: PGSM does not accept 3 dimension fields (NK>1)')
            call pgsmabt
        endif

        ! identifier parametres si type g-a-b-l-c
        dat = record%dateo
        deet = record%deet
        cigtyp = record%grtyp

        ! verifier si grille gaussienne ni doit etre pair
        if (cigtyp == 'G' .and. mod(record%ni, 2) /= 0)  then
            call messags(record%ni)
        endif

        ! nombre de longitude max = 1000
        if (record%ni > 1000) then
            call app_log(APP_ERROR, 'coupzm: Too many longitudes within fields (max=1000)')
            call pgsmabt
        endif

        ! verifier si dimension nj > 500
        if (record%ig1 == 0 .and. record%nj > 500) then
            call app_log(APP_ERROR, 'coupzm: Dimension NJ for MOYENT-MOYSRT too large (max=500)')
            call pgsmabt
        endif

        ! verifier type de grille
        if (cigtyp /= 'G' .and. cigtyp /= 'A' .and. cigtyp /= 'L' .and. cigtyp /= 'B' .and. cigtyp /= 'C') then
            write(app_msg, *) 'coupzm: Bad grid type: ', cigtyp, ', must be G - L - B - A - C (MOYENT/MOYSRT)'
            call app_log(APP_ERROR, app_msg)
            return
        endif

        allocate(tmpif1(record%ni, record%nj))

        if ( .not. message) then
            iopc = fstopc('TOLRNC', 'DEBUGS', .true. )
        endif
        call chk_userdate(datev)

        if (.not. file%read(record, data = c_loc(tmpif1(1, 1))))  then
            call app_log(APP_ERROR, 'coupzm: Field does not exist in MOYENT/MOYSRT')
            call pgsmabt
        endif
        call prefiltre(tmpif1, record%ni, record%nj, record%grtyp)

        if (printen)  then
            call imprime(cnomvar, tmpif1, record%ni, record%nj)
        endif

        ! initialiser les poids pour grille gaussienne meridionale
        if (cjcoup == 'MER' .and. cigtyp == 'G') then
            ilath = record%nj
            if (record%ig1 == 0) then
                ilath = record%nj / 2
            endif

            call gauss(ilath, coa, poids, sia, rad, w, sinm1, sinm2, sin2)

            ! sauve les poids dans coa pour renverser le champ si necessaire

            do j = 1, ilath
                coa(j) = poids(j)
            enddo

            ! si gaussienne globale  transfer hemis nord dans hemis sud (miroir)
            if (record%ig1 == 0)  then
                 call loupmir(poids(ilath), poids(ilath), ilath)
            endif

            ! hemisphere sud  orientation nord-sud renverse
            if ((record%ig1 == 2 .and. record%ig2 == 1) .or. (record%ig1 == 1 .and. record%ig2 == 0)) then
                call louptra(poids(ilath), coa, ilath)
            else
                call loupin1(poids(1), poids(1), nj)
            endif
        endif

        ! calcul moyenne zonale ou meridional
        call calcul(tmpif1, champ, record%ni, record%nj, poids, cjcoup, cigtyp)

        ! si type de grille 'b' on reduit ni = ni-1
        if (cigtyp == 'B') then
            ni = record%ni - 1
        else
            ni = record%ni
        endif

        ! ecrit champ zonal
        if (cjcoup == 'ZON') then
            ! ecrit champ  meridional
            call ecritur(champ, npack, dat, deet, npas, 1, record%nj, record%nk, record%ip1, record%ip2, record%ip3, &
                ctypvar, cnomvar, cetiket, cigtyp, record%ig1, record%ig2, record%ig3, record%ig4)
        else
            call ecritur(champ, npack, dat, deet, npas, ni, 1, 1, record%ip1, record%ip2, record%ip3, &
                ctypvar, cnomvar, cetiket, cigtyp, record%ig1, record%ig2, record%ig3, record%ig4)
        endif
        deallocate(tmpif1)
    enddo
end
