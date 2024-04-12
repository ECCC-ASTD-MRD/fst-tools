!> Lire la position(lat, lon) de chaque point d'une grille de type "Y" ou "Z"
subroutine gritp12(it, ip1, ip2, ip3)
    use app
    implicit none

    integer, intent(in) :: it
    integer, intent(in) :: ip1
    integer, intent(in) :: ip2
    integer, intent(in) :: ip3

    ! Y = LISTE DE LAT-LON(NI, NJ) OU X-Y(NI, NJ)
    ! Z = COLONNE DE LAT(NJ) OU RANGEE DE LONG(NI) OU COLONNE DE Y(NJ)   RANGEE DE X(NI)
    ! COORDONNEE X, Y SONT POLAIRE STEREOGRAPHIQUE

    external pgsmabt, imprime, ecritur

    integer, external :: ezqkdef
    integer, external :: ezgdef
    integer, external :: ezgxprm
    integer, external :: gdgaxes
    integer, external :: gdll
    integer, external :: chkenrpos

#include "llccmm.cdk90"
#include "indptr.cdk90"
#include "packin.cdk90"
#include "grilles.cdk90"
#include "lires.cdk90"
#include "voir.cdk90"
#include "tp12ig.cdk90"

    character(len = 12) :: etikx
    character(len = 4) :: nomx
    character(len = 2) :: grref

    integer :: i
    integer :: swa, lng, dltf, ubc, extra1, extra2, extra3

    integer :: ni, nj, nk
    integer :: dateo, deet, npas
    integer :: ier, iopc
    integer :: ig1, ig2, ig3, ig4, ig1ref, ig2ref, ig3ref, ig4ref
    integer :: ip1x, ip2x, ip3x

    character(len = 4) :: nomvarx, nomvary, nomu
    character(len = 12) :: etiketx, etikety, etiku
    character(len = 2) :: typvarx, typvary, typvaru
    integer :: nix, njx, niu, niy, njy, nju, nkx, nky, nku
    integer :: dateox, deetx, npasx, nbitsx, datypx
    integer :: dateou, deetu, npasu, nbitsu, datypu

    integer :: lip1, lip2, lip3

    logical :: grille_z, grille_u

    type(fst_file) :: inputFile
    type(fst_query) :: tic_query
    type(fst_query) :: tac_query
    type(fst_query) :: tictac_query
    type(fst_record) :: tic
    type(fst_record) :: tac
    type(fst_record) :: tictac

    if (it == gr_tape2) then
        ! iunit = lnkdiun(idx_ozsrt)
        inputFile = outputFile
    else
        ! iunit = lnkdiun(1)
        inputFile = inputFiles(1)
    endif

    !  VERIFICATION DES PARAMETRES DES 2 RECORDS
    !  SI LU SUR UNIT 1 ON ECRIT LES 2 RECORDS SUR UNIT 2

    nix = -1
    njx = -1
    nkx = -1
    niy = -1
    njy = -1
    nky = -1
    niu = -1
    nju = -1
    nku = -1
    grille_z = .false.
    grille_u = .false.

    if (it /= gr_stations) then
        ier = chkenrpos(lnkdiun(1), lnkdiun(idx_ozsrt), ip1, ip2, ip3)
        if (ier < 0) then
            call app_log(APP_ERROR, 'gritp12: Posisional records missing')
            call pgsmabt
        elseif (ier == 0) then
            grille_z = .true.
        else
            grille_u = .true.
        endif

        if (outputFileMode == 1) then
            if (grille_z) then
                cgrtyp = 'Z'
                ! ireclo = fstinf(iunit, nix, njx, nkx, -1, '            ', ip1, ip2, ip3, '  ', '>>  ')
                ! irecla = fstinf(iunit, niy, njy, nky, -1, '            ', ip1, ip2, ip3, '  ', '^^  ')
                tic_query = inputFile%new_query(ip1 = ip1, ip2 = ip2, ip3 = ip3, nomvar = '>>  ')
                tic_query%find_next(tic)
                call tic_query%free()
                nix = tic%ni
                njx = tic%nj
                nkx = tic%nk
                tac_query = inputFile%new_query(ip1 = ip1, ip2 = ip2, ip3 = ip3, nomvar = '^^  ')
                tac_query%find_next(tac)
                call tac_query%free()
                niy = tac%ni
                njy = tac%nj
                nky = tac%nk

                gdout = ezqkdef(tic%ni, tac%nj, cgrtyp, ip1, ip2, ip3, 0, inputFile%get_unit())
            endif

            if (grille_u) then
                cgrtyp = 'U'
                ! irecyy = fstinf(iunit, niu, nju, nku, -1, '            ', ip1, ip2, ip3, '  ', '^>  ')
                tictac_query = inputFile%new_query(ip1 = ip1, ip2 = ip2, ip3 = ip3, nomvar = '^>  ')
                tictac_query%find_next(tictac)
                call tictac_query%free()

                ! ier = fstprm(irecyy, dateou, deetu, npasu, niu, nju, nku, nbitsu, datypu, lip1, lip2, lip3, typvaru, nomu, etiku, grref, ig1ref, ig2ref, ig3ref, ig4ref, swa, lng, dltf, ubc, extra1, extra2, extra3)
                niu = tictac%ni
                nju = tictac%nj
                nku = tictac%nk
                gdout = ezqkdef(tictac%ni, tictac%nj, cgrtyp, tictac%ip1, tictac%ip2, tictac%ip3, 0, inputFile%get_unit())
                ier = ezgxprm(gdout, li, lj, cgrtyp, lg1, lg2, lg3, lg4, tictac%grtyp, tictac%ig1, tictac%ig2, tictac%ig3, tictac%ig4)
            endif

            li = nix
            lj = njy
            lg1 = ig1
            lg2 = ig2
            lg3 = ig3
            lg4 = ig4
        else
            ! ireclo = fstinf(iunit, nix, njx, nkx, -1, '            ', ip1, ip2, ip3, '  ', '>>  ')
            ! irecla = fstinf(iunit, niy, njy, nky, -1, '            ', ip1, ip2, ip3, '  ', '^^  ')
            tic_query = inputFile%new_query(ip1 = ip1, ip2 = ip2, ip3 = ip3, nomvar = '>>  ')
            tic_query%find_next(tic)
            call tic_query%free()
            nix = tic%ni
            njx = tic%nj
            nkx = tic%nk
            tac_query = inputFile%new_query(ip1 = ip1, ip2 = ip2, ip3 = ip3, nomvar = '^^  ')
            tac_query%find_next(tac)
            call tac_query%free()
            niy = tac%ni
            njy = tac%nj
            nky = tac%nk
            ! ier = fstprm(ireclo, dateox, deetx, npasx, nix, njx, nkx, nbitsx, datypx, lip1, lip2, lip3, typvarx, nomx, etikx, grref, ig1ref, ig2ref, ig3ref, ig4ref, swa, lng, dltf, ubc, extra1, extra2, extra3)
            li = nix
            lj = njy
            gdout = ezqkdef(li, lj, 'Z', tic%ip1, tic%ip2, tic%ip3, 0, 1)
         endif
         allocate(tmplon(li, lj))
         allocate(tmplat(li, lj))
         ier = gdll(gdout, tmplat, tmplon)
    else
        nix = ncoords
        niy = ncoords
        njx = 1
        njy = 1
        cgrtyp = 'Y'
        cgtypxy= 'L'
        nomvarx = '>>  '
        nomvary = '^^  '
        etikety=  'NORDSUD     '
        etiketx = 'ESTOUEST    '
        typvarx = 'C '
        typvary = 'C '
        ip1x = ip1
        ip2x = ip2
        ip3x = ip3
        deetx= 0
        npasx= 0
        dateox = 017901000
        li = ncoords
        lj = 1
        allocate(tmplon(li, lj))
        allocate(tmplat(li, lj))
        call cxgaig('L', ig1la, ig2la, ig3la, ig4la, 0., 0., 1.0, 1.0)
        call cxgaig('L', ig1lo, ig2lo, ig3lo, ig4lo, 0., 0., 1.0, 1.0)
        npack = -32

        do i = 1, li * lj
            print *, i, dateox, typvarx, li, lj
        enddo
    endif

    ! INITIALISATION DE DGRWXY POUR UTILISATION DANS  ROUTINE VDAUV
    if (cgtypxy == 'E') then
        allocate(tmplong(li, lj))
        allocate(tmplatg(li, lj))
        allocate(tmplon(li, lj))
        allocate(tmplat(li, lj))
    endif

    if (.not. message) iopc = fstopc('TOLRNC', 'DEBUGS', .true.)

    if (it == gr_stations) then
        call fillcoord(tmplat, tmplon)
        gdout = ezgdef(nix, njx, cgrtyp, cgtypxy, ig1la, ig2la, ig3la, ig4la, tmplon, tmplat)

        nj = 1
        nk = 1
        if (outputFileMode == 1) then
            call ecritur(tmplon, npack, dateox, deetx, npasx, ncoords, nj, nk, ip1x, ip2x, ip3x, typvarx, nomvarx, etiketx, cgtypxy, ig1lo, ig2lo, ig3lo, ig4lo)
            call ecritur(tmplat, npack, dateox, deetx, npasx, ncoords, nj, nk, ip1x, ip2x, ip3x, typvary, nomvary, etikety, cgtypxy, ig1la, ig2la, ig3la, ig4la)
            lg1 = ip1x
            lg2 = ip2x
            lg3 = ip3x
            lg4 = 0
        endif
    else
        ier = gdll(gdout, tmplat, tmplon)
    endif

    if (printen)  call imprime(nomvary, tmplat, niy, njy)
    if (printen)  call imprime(nomvarx, tmplon, nix, njx)

    ! CALCUL LATITUDES LONGITUDES  DU TYPE "Z" OU "Y"
    if (cgrtyp == 'Z') then
        ier = ezgxprm(gdout, li, lj, cgrtyp, lg1, lg2, lg3, lg4, cgtypxy, ig1ref, ig2ref, ig3ref, ig4ref)
        ier = gdll(gdout, tmplat, tmplon)
    endif

    if (printen)  call app_log(APP_INFO, 'gritp12: Print latlon after gdll call')
    if (printen)  call imprime(nomvarx, tmplat, niy, njy)
    if (printen)  call imprime(nomvary, tmplon, nix, njx)
end
