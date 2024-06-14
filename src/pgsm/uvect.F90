!> Interpolation des vecteurs u-v (horizontalement)
subroutine uvectur (cnom1, cnom2, cnom3, iheur, npar, itabuv)
    use, intrinsic :: iso_fortran_env
    use rmn_fst24
    use app
    use files, only : nRecords, inputFiles
    use packing, only : npack, npack_orig
    use pgsm_mod, only : vvent, wdvent
    use pgsm_mod, only : nwetike, etikent
    use pgsm_mod, only : ier
    use pgsm_mod, only : tmpif1, tmpif2, typeent, ip3ent
    use accum, only : npas
    use grilles, only : cgrtyp, gdin, gdout, lg1, lg2, lg3, lg4, li, lj, masque
    use pgsm_mod, only : message
    implicit none

    !> Nom du premier vecteur  ex:"uu", "us"...
    character(len = 4), intent(in) :: cnom1
    !> Nom du deuxieme vecteur ex:"vv", "vs"...
    character(len = 4), intent(in) :: cnom2
    !> Nom du champ a ecrire apres interpolation du vent ex:"uv"
    character(len = 4), intent(in) :: cnom3
    !> Heure de la variable
    integer, intent(in) :: iheur
    !> Nombre de locations dnas itabuv
    integer, intent(in) :: npar
    !> Table contenant les noms (niveau)
    integer, dimension(npar), intent(in) :: itabuv

#include "enrege.cdk90"
#include "gdz.cdk90"
#include "tp12ig.cdk90"

    external ecritur, cigaxg, pgsmabt, imprime, incdat, messags
    external cxgaig
    integer, external :: fstopc
    integer ezqkdef, ezwdint, ezuvint, ezdefset, ezsint
    integer, external :: gdwdfuv, gdll

    character(*), parameter :: missing_rec_fmt = "('uvectur: No record in file NOM=', a2)"

    character(len = 2) :: ctypvar
    character(len = 12) :: cetiket, cetike
    integer i, j
    integer jp1, jp2, jp3, ni, nj
    real, allocatable, dimension(:, :) :: latgdin, longdin
    integer :: ilop, iprs, iopc
    integer :: infon, datdv
    integer :: cdatyp
    real :: datev
    integer :: entier_ou_reel

    real(kind = real64) :: delta_t

    real, dimension(:, :), pointer :: uuout, vvout
    integer, dimension(:, :), pointer :: itmpif1, itmpif2, itmpif3, itmpif4
    real, dimension(:, :), pointer :: tmpif3, tmpif4

    type(fst_query) :: query
    type(fst_record), dimension(nRecords) :: records
    type(fst_record) :: vv

    call chk_userdate(datev)

    do iprs = 1, npar
        ! trouver record pour u, v  ou us, vs .....

        ! modification de hollerith a caractere
        if (etikent(1)  /=  -1) then
            write(cetiket, '(3A4)') (etikent(i), i=1, nwetike)
        else
            cetiket = '            '
        endif

        if (typeent  /=  -1) then
            write(ctypvar, '(A2)') typeent
        else
            ctypvar = '  '
        endif

        query = inputFiles(1)%new_query(datev = datev, etiket = cetiket, ip1 = itabuv(iprs), ip2 = iheur, ip3 = ip3ent, &
            typevar = ctypvar, nomvar = cnom1)
        infon = query%find_all(records)
        call query%free()
        if (infon == 0) then
            write(app_msg, missing_rec_fmt) cnom1
            call app_log(APP_ERROR, app_msg)
            cycle
        endif

        do ilop = 1, ifon
            if (records(i)%nk > 1) then
                call app_log(APP_ERROR, 'uvectur: PGSM does not accept 3 dimension fields (NK>1)')
                call pgsmabt
            endif
        end do

        do ilop = 1, infon
            entier_ou_reel = 1
            npack_orig = -records(ilop)%nbits
            npas = records(ilop)%npas

            ! verifier si grille gaussienne ni doit etre pair
            if (records(ilop)%grtyp == 'G' .and. mod(records(ilop)%ni, 2) /= 0) call messags(ni)
            ! calcul la date pour le record de la variable nom2
            delta_t = records(ilop)%deet * records(ilop)%npas / 3600.0
            call incdatr(datdv, records(ilop)%dateo, delta_t)
            vv_query = inputFiles(1)%new_query(datev = datdv, etiket = records(ilop)%etiket, &
                ip1 = records(ilop)%ip1, ip2 = records(ilop)%ip2, ip3 = records(ilop)%ip3, typevar = ctypvar, nomvar = cnom2 &
                ig1 = records(ilop)%ig1, ig2 = records(ilop)%ig2, ig3 = records(ilop)%ig3, ig4 = records(ilop)%ig4, grtyp = records(ilop)%grtyp)
            if (.not. vv_query % fine_next(vv)) then
                write(app_msg, missing_rec_fmt) cnom2
                call app_log(APP_ERROR, app_msg)
                call pgsmabt
            endif
            call vv_query%free()

            if (vv%nk > 1) then
                call app_log(APP_ERROR, 'uvectur: PGSM does not accept 3 dimension fields (NK>1)')
                call pgsmabt
            endif

            ! Switch pour champs masques
            if (masque == 1) then
                if (vv%typvar(1:1) == '@') then
                    cycle ! Les masques sont traites dans uvecteur_masque
                else if (vv%typvar(2:2) == '@') then
                    call uvecteur_masque(records(ilop), vv)
                    cycle
                endif
            endif

            if (records(ilop)%cdatyp == 2 .or. records(ilop)%cdatyp == 130 .or. records(ilop)%cdatyp == 4 .or. records(ilop)%cdatyp == 132) then
                allocate(itmpif3(li, lj))
                allocate(itmpif4(li, lj))
                entier_ou_reel = 2
            endif

            allocate(tmpif3(li, lj))
            allocate(tmpif4(li, lj))

            ! lire champ nom1
            if (.not. message) iopc = fstopc('TOLRNC', 'DEBUGS', .true.)
            records(ilop)%read()
            if (entier_ou_reel == 2) then
                call records(ilop)%get_data_array(itmpif1)
                call prefiltre(itmpif1, records(ilop)%ni, records(ilop)%nj, records(ilop)%grtyp)
            else
                call records(ilop)%get_data_array(tmpif1)
                call prefiltre(tmpif1, records(ilop)%ni, records(ilop)%nj, records(ilop)%grtyp)
            endif

            if (printen) call imprime(cnom1, tmpif1, ni, nj)
            if (.not. message) iopc = fstopc('TOLRNC', 'DEBUGS', .true.)
            vv%read()
            if (entier_ou_reel == 2) then
                call vv%get_data_array(itmpif2)
                call prefiltre(itmpif2, vv%ni, vv%nj, vv%grtyp)
            else
                call vv%get_data_array(tmpif2)
                call prefiltre(tmpif2, vv%ni, vv%nj, vv%grtyp)
            endif
            if (records(ilop)%grtyp == 'G'.and. mod(ni, 2) /= 0) call messags(ni)

            if (printen) call imprime(cnom2, tmpif2, ni, nj)

            ! si vvent = .true. on calcule la vitesse du vent
            if (entier_ou_reel == 2) then
                call cvtrfi(tmpif1, itmpif1, ni, nj)
                call cvtrfi(tmpif2, itmpif2, ni, nj)
            endif

            gdin = ezqkdef(records(ilop)%ni, records(ilop)%nj, &
                records(ilop)%grtyp, records(ilop)%ig1, records(ilop)%ig2, records(ilop)%ig3, records(ilop)%ig4, inputFiles(1)%get_unit())

            if (vvent) then
                if (gdout == gdin) then
                    if (ctypvar == '@@') then
                        uuout => tmpif1
                        vvout => tmpif2
                    else
                        if (wdvent) then
                            allocate(latgdin(ni, nj), longdin(ni, nj))
                            ier = gdll(gdin, latgdin, longdin)
                            ier = gdwdfuv(gdin, tmpif3, tmpif4, tmpif1, tmpif2, latgdin, longdin, records(ilop)%ni * records(ilop)%nj)
                            cgrtyp = records(ilop)%grtyp
                            lg1 = records(ilop)%ig1
                            lg2 = records(ilop)%ig2
                            lg3 = records(ilop)%ig3
                            lg4 = records(ilop)%ig4
                            uuout => tmpif3
                            vvout => tmpif4
                        else
                            uuout => tmpif1
                            vvout => tmpif2
                            uuout = sqrt(uuout * uuout + vvout * vvout)
                        endif
                    endif
                else
                    ier = ezdefset(gdout, gdin)
                    if (ctypvar == '@@') then
                        ier = ezsint(tmpif3, tmpif1)
                        ier = ezsint(tmpif4, tmpif2)
                    else
                        ier = ezwdint(tmpif3, tmpif4, tmpif1, tmpif2)
                    endif
                    uuout => tmpif3
                    vvout => tmpif4
                endif

                if (entier_ou_reel == 2) then
                    call cvtifr(itmpif3, uuout, li, lj)
                    call iecritur(itmpif3, npack, records(ilop)%dateo, records(ilop)%deet, records(ilop)%npas, &
                    li, lj, records(ilop)%nk, records(ilop)%jp1, records(ilop)%jp2, records(ilop)%jp3, &
                    ctypvar, cnom3, cetike, cgrtyp, lg1, lg2, lg3, lg4)
                else
                    call ecritur(uuout, npack, records(ilop)%dateo, records(ilop)%deet, records(ilop)%npas, &
                    li, lj, records(ilop)%nk, records(ilop)%jp1, records(ilop)%jp2, records(ilop)%jp3, &
                    ctypvar, cnom3, cetike, cgrtyp, lg1, lg2, lg3, lg4)
                endif

                if (wdvent) then
                    do j = 1, lj
                        do i = 1, li
                            if (tmpif4(i, j)<0.0) then
                                tmpif4(i, j) = tmpif4(i, j) + 360.0
                            endif
                        enddo
                    enddo
                    vvout => tmpif4
                    if (entier_ou_reel == 2) then
                        call cvtifr(itmpif4, vvout, li, lj)
                        call iecritur(itmpif4, npack, records(ilop)%dateo, records(ilop)%deet, records(ilop)%npas, &
                        li, lj, records(ilop)%nk, records(ilop)%jp1, records(ilop)%jp2, records(ilop)%jp3, &
                            ctypvar, 'WD  ', cetike, cgrtyp, lg1, lg2, lg3, lg4)
                    else
                        call ecritur(vvout, npack, records(ilop)%dateo, records(ilop)%deet, records(ilop)%npas, &
                        li, lj, records(ilop)%nk, records(ilop)%jp1, records(ilop)%jp2, records(ilop)%jp3, &
                            ctypvar, 'WD  ', cetike, cgrtyp, lg1, lg2, lg3, lg4)
                    endif
                endif ! wdvent
            else ! vvent
                ! on ne fait pas d'interpolation si igtyp=grtyp  ig1=lg1  ig2=lg2 ig3=lg3  ig4=lg4
                if (records(ilop)%grtyp /= cgrtyp .or. &
                    records(ilop)%ig1 /= lg1 .or. records(ilop)%ig2 /= lg2 .or. records(ilop)%ig3 /= lg3 .or. records(ilop)%ig4 /= lg4 .or. &
                    records(ilop)%ni /= li .or. records(ilop)%nj /= lj) then
                    ! interpolation u, v vecteur a vitesse et direction du vent
                    ier = ezdefset(gdout, gdin)

                    ! cas special pour typvar = @@
                    if (ctypvar == '@@') then
                        ier = ezsint(tmpif3, tmpif1)
                        ier = ezsint(tmpif4, tmpif2)
                    else
                        ier = ezuvint(tmpif3, tmpif4, tmpif1, tmpif2)
                    endif

                    uuout => tmpif3
                    vvout => tmpif4

                    ! apres interpolation horizontale passer de vitesse et direction
                    ! aux composantes u et v

                    ! si type de grille "x",    u-v interpolation n\s - e\o
                else
                    deallocate(tmpif3)
                    deallocate(tmpif4)
                    uuout => tmpif1
                    vvout => tmpif2
                    if (message) then
                        call app_log(APP_WARNING, 'uvectur: No horizontal interpolation')
                    endif
                endif

                ! ecrire vecteur u
                if (entier_ou_reel == 2) then
                    call cvtifr(itmpif3, uuout, li, lj)
                    call iecritur(itmpif3, npack, records(ilop)%dateo, records(ilop)%deet, records(ilop)%npas, &
                        li, lj, records(ilop)%nk, records(ilop)%jp1, records(ilop)%jp2, records(ilop)%jp3, ctypvar, cnom1, cetike, cgrtyp, &
                        lg1, lg2, lg3, lg4)
                else
                    call ecritur(uuout, npack, records(ilop)%dateo, records(ilop)%deet, records(ilop)%npas, &
                        li, lj, records(ilop)%nk, records(ilop)%jp1, records(ilop)%jp2, records(ilop)%jp3, ctypvar, cnom1, cetike, cgrtyp, &
                        lg1, lg2, lg3, lg4)
                endif

                ! ecrire vecteur v
                if (entier_ou_reel == 2) then
                    call cvtifr(itmpif4, vvout, li, lj)
                    call iecritur(itmpif4, npack, records(ilop)%dateo, records(ilop)%deet, records(ilop)%npas, &
                        li, lj, records(ilop)%nk, records(ilop)%jp1, records(ilop)%jp2, records(ilop)%jp3, ctypvar, cnom2, cetike, cgrtyp, &
                        lg1, lg2, lg3, lg4)
                else
                    call ecritur(vvout, npack, records(ilop)%dateo, records(ilop)%deet, records(ilop)%npas, &
                        li, lj, records(ilop)%nk, records(ilop)%jp1, records(ilop)%jp2, records(ilop)%jp3, ctypvar, cnom2, cetike, cgrtyp, &
                        lg1, lg2, lg3, lg4)
                endif
            endif ! fin du calcul des composantes

            call records(ilop)%free()
            call vv%free()

            if (associated(tmpif1)) deallocate(tmpif1)
            if (associated(tmpif2)) deallocate(tmpif2)
            if (associated(tmpif3)) deallocate(tmpif3)
            if (associated(tmpif4)) deallocate(tmpif4)

            if (allocated(latgdin)) deallocate(latgdin)
            if (allocated(longdin)) deallocate(longdin)

            if (entier_ou_reel == 2) then
                if (associated(itmpif3)) deallocate(itmpif3)
                if (associated(itmpif4)) deallocate(itmpif4)
            endif
        enddo ! ilop = 1, infon
    enddo ! fin boucle iprs
    vvent = .false.
end
