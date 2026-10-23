!> Horizontaly interpolate vectors
subroutine uvectur(vec_comp1_name, vec_comp2_name, vec_len_name, vec_dir_name, iheur, nip1s, ip1s)
    use, intrinsic :: iso_fortran_env, only: real64
    use app
    implicit none

    !> First vector component nomvar
    character(len = 4), intent(in) :: vec_comp1_name
    !> Second vector component nomvar
    character(len = 4), intent(in) :: vec_comp2_name
    !> Vector length nomvar
    character(len = 4), intent(in) :: vec_len_name
    !> Vector direction nomvar
    character(len = 4), intent(in) :: vec_dir_name
    !> Forecast hour
    integer, intent(in) :: iheur
    !> Number of levels
    integer, intent(in) :: nip1s
    !> Levels
    integer, intent(in) :: ip1s(nip1s)

#include "defin.cdk90"
#include "llccmm.cdk90"
#include "accum.cdk90"
#include "lires.cdk90"
#include "pairs.cdk90"
#include "lnkflds.cdk90"
#include "dates.cdk90"
#include "grilles.cdk90"
#include "voir.cdk90"
#include "packin.cdk90"

    external :: ecritur, pgsmabt, imprime, messags
    integer, external :: fstinf, fstinl, ipgsmlic, pgsmlic, ipgsmlir, pgsmlir, fstprm, fstopc
    integer, external :: ezqkdef, ezwdint, ezuvint, ezdefset, ezsint, gdwdfuv, gdll

    character(len = *), parameter :: missing_rec_fmt = "('uvectur: No record in file NOM=', a4)"

    character(len = 12) :: cetiket, cetike
    character(len = 4) :: cnomvar
    character(len = 1) :: cigtyp
    character(len = 2) :: ctypvar
    integer :: i, j
    integer :: jp1, jp2, jp3, ni, nj, nk
    integer, dimension(:), allocatable :: listniv
    real, allocatable, dimension(:, :) :: latgdin, longdin
    integer :: deet, ig1, ig2, ig3, ig4
    integer :: ilop, ip1_idx, irec_uu, irec_vv, iopc
    integer :: numu, numv, infon, dat, datdv
    integer :: cnbits, cdatyp, cswa, clng, cdltf, cubc, extra1, extra2, extra3, total_keys
    integer :: datev
    integer :: iunit
    logical :: is_integer
    real(kind = real64) :: delta_t
    real, dimension(:, :), pointer :: uuout, vvout

    iunit = lnkdiun(1)
    nk = 1

    call pgsm_get_nfstkeys(total_keys)
    allocate(listniv(total_keys))

    call chk_userdate(datev)

    do ip1_idx = 1, nip1s
        ! modification de hollerith a caractere
        if (etikent(1) /= -1) then
            write(cetiket, '(3A4)') (etikent(i), i=1, nwetike)
        else
            cetiket = '            '
        endif

        if (typeent /= -1) then
            write(ctypvar, '(A2)') typeent
        else
            ctypvar = '  '
        endif

        ! trouver record pour u, v  ou us, vs .....
        ier = fstinl(iunit, ni, nj, nk, datev, cetiket, ip1s(ip1_idx), iheur, ip3ent, ctypvar, vec_comp1_name, listniv, infon, total_keys)
        if (ier < 0 .or. infon == 0) then
            write(app_msg, missing_rec_fmt) vec_comp1_name
            call app_log(APP_ERROR, app_msg)
            cycle
        endif

        if (nk > 1) then
            call app_log(APP_ERROR, 'uvectur: PGSM does not accept 3 dimension fields (NK>1)')
            call pgsmabt
        endif

        do ilop = 1, infon
            is_integer = .false.
            irec_uu = listniv(ilop)

            ! identifier parametres champ nom1
            cetike = '            '
            ier = fstprm(irec_uu, dat, deet, npas, ni, nj, nk, cnbits, cdatyp, jp1, jp2, jp3, ctypvar, cnomvar, cetike, cigtyp, ig1, ig2, ig3, ig4, cswa, clng, cdltf, cubc, extra1, extra2, extra3)
            npack_orig = -cnbits
            if (ier < 0) then
                call app_log(APP_ERROR, 'uvectur: FSTPRM failed')
            endif

            ! verifier si grille gaussienne ni doit etre pair
            if (cigtyp == 'G' .and. mod(ni, 2) /= 0) call messags(ni)

            ! calcul la date pour le record de la variable vec_comp2_name
            delta_t = deet * npas / 3600.0
            call incdatr(datdv, dat, delta_t)
            irec_vv = fstinf(iunit, ni, nj, nk, datdv, cetike, jp1, jp2, jp3, ctypvar, vec_comp2_name)
            if (irec_vv < 0) then
                write(app_msg, missing_rec_fmt) vec_comp2_name
                call app_log(APP_ERROR, app_msg)
                call pgsmabt
            endif

            if (nk > 1) then
                call app_log(APP_ERROR, 'uvectur: PGSM does not accept 3 dimension fields (NK>1)')
                call pgsmabt
            endif

            ! Switch pour champs masques
            if (masque == 1) then
                if (ctypvar(1:1) == '@') then
                    cycle ! Les masques sont traites dans uvecteur_masque
                else if (ctypvar(2:2) == '@') then
                    call uvecteur_masque(irec_uu, irec_vv)
                    cycle
                endif
            endif

            ! allouer memoire
            if (cdatyp == 2 .or. cdatyp == 130 .or. cdatyp == 4 .or. cdatyp == 132) then
                allocate(itmpif1(ni, nj))
                allocate(itmpif2(ni, nj))
                allocate(itmpif3(li, lj))
                allocate(itmpif4(li, lj))
                is_integer = .true.
            endif

            allocate(tmpif1(ni, nj))
            allocate(tmpif2(ni, nj))
            allocate(tmpif3(li, lj))
            allocate(tmpif4(li, lj))

            ! lire champ nom1
            if ( .not. message) iopc= fstopc('TOLRNC', 'DEBUGS', .true. )
            if (is_integer) then
                numu = ipgsmlir(itmpif1, 1, ni, nj, nk, datdv, cetike, jp1, jp2, jp3, ctypvar, vec_comp1_name, cigtyp)
            else
                numu = pgsmlir(tmpif1, 1, ni, nj, nk, datdv, cetike, jp1, jp2, jp3, ctypvar, vec_comp1_name, cigtyp)
            endif

            if (printen) call imprime(vec_comp1_name, tmpif1, ni, nj)
            if ( .not. message) iopc= fstopc('TOLRNC', 'DEBUGS', .true. )
            if (is_integer) then
                numv = ipgsmlic(itmpif2, 1, ni, nj, nk, datdv, cetike, jp1, jp2, jp3, ctypvar, vec_comp2_name, ig1, ig2, ig3, ig4, cigtyp)
            else
                numv = pgsmlic(tmpif2, 1, ni, nj, nk, datdv, cetike, jp1, jp2, jp3, ctypvar, vec_comp2_name, ig1, ig2, ig3, ig4, cigtyp)
            endif
            if (cigtyp == 'G' .and. mod(ni, 2) /= 0) call messags(ni)
            if (printen) call imprime(vec_comp2_name, tmpif2, ni, nj)

            if (is_integer) then
                call cvtrfi(tmpif1, itmpif1, ni, nj)
                call cvtrfi(tmpif2, itmpif2, ni, nj)
            endif

            gdin = ezqkdef(ni, nj, cigtyp, ig1, ig2, ig3, ig4, iunit)

            if (vvent) then
                if (gdout == gdin) then
                    ! Output grid is the same as the input to one
                    if (ctypvar == '@@') then
                        uuout => tmpif1
                        vvout => tmpif2
                    else
                        if (wdvent) then
                            allocate(latgdin(ni, nj), longdin(ni, nj))
                            ier = gdll(gdin, latgdin, longdin)
                            ier = gdwdfuv(gdin, tmpif3, tmpif4, tmpif1, tmpif2, latgdin, longdin, ni * nj)
                            cgrtyp = cigtyp
                            lg1 = ig1
                            lg2 = ig2
                            lg3 = ig3
                            lg4 = ig4
                            uuout => tmpif3
                            vvout => tmpif4
                        else
                            uuout => tmpif1
                            vvout => tmpif2
                            uuout = sqrt(uuout * uuout + vvout * vvout)
                        endif
                    endif
                else
                    !> Input and output grids are different
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

                if (is_integer) then
                    call cvtifr(itmpif3, uuout, li, lj)
                    call iecritur(itmpif3, npack, dat, deet, npas, li, lj, nk, jp1, jp2, jp3, &
                        ctypvar, vec_len_name, cetike, cgrtyp, lg1, lg2, lg3, lg4)
                else
                    call ecritur(uuout, npack, dat, deet, npas, li, lj, nk, jp1, jp2, jp3, &
                        ctypvar, vec_len_name, cetike, cgrtyp, lg1, lg2, lg3, lg4)
                endif

                if (wdvent) then
                    do j = 1, lj
                        do i = 1, li
                            if (tmpif4(i, j) < 0.0) then
                                tmpif4(i, j) = tmpif4(i, j) + 360.0
                            endif
                        enddo
                    enddo
                    vvout => tmpif4
                    if (is_integer) then
                        call cvtifr(itmpif4, vvout, li, lj)
                        call iecritur(itmpif4, npack, dat, deet, npas, li, lj, nk, jp1, jp2, jp3, &
                            ctypvar, vec_dir_name, cetike, cgrtyp, lg1, lg2, lg3, lg4)
                    else
                        call ecritur(vvout, npack, dat, deet, npas, li, lj, nk, jp1, jp2, jp3, &
                            ctypvar, vec_dir_name, cetike, cgrtyp, lg1, lg2, lg3, lg4)
                    endif
                endif
            else
                ! on ne fait pas d'interpolation si igtyp = grtyp, ig1 = lg1, ig2 = lg2, ig3 = lg3, ig4 = lg4
                if (cigtyp /= cgrtyp .or. ig1 /= lg1 .or. ig2 /= lg2 .or. ig3 /= lg3 .or. ig4 /= lg4 .or. li /= ni .or. lj /= nj) then
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

                    ! apres interpolation horizontale passer de vitesse et direction aux composantes u et v
                    ! si type de grille "x", u-v interpolation n\s - e\o
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
                if (is_integer) then
                    call cvtifr(itmpif3, uuout, li, lj)
                    call iecritur(itmpif3, npack, dat, deet, npas, li, lj, nk, jp1, jp2, jp3, ctypvar, vec_comp1_name, cetike, cgrtyp, lg1, lg2, lg3, lg4)
                else
                    call ecritur(uuout, npack, dat, deet, npas, li, lj, nk, jp1, jp2, jp3, ctypvar, vec_comp1_name, cetike, cgrtyp, lg1, lg2, lg3, lg4)
                endif

                ! ecrire vecteur v
                if (is_integer) then
                    call cvtifr(itmpif4, vvout, li, lj)
                    call iecritur(itmpif4, npack, dat, deet, npas, li, lj, nk, jp1, jp2, jp3, ctypvar, vec_comp2_name, cetike, cgrtyp, lg1, lg2, lg3, lg4)
                else
                    call ecritur(vvout, npack, dat, deet, npas, li, lj, nk, jp1, jp2, jp3, ctypvar, vec_comp2_name, cetike, cgrtyp, lg1, lg2, lg3, lg4)
                endif

                ! fin du calcul des composantes
            endif

            if (associated(tmpif3)) deallocate(tmpif3)
            if (associated(tmpif4)) deallocate(tmpif4)
            if (associated(tmpif1)) deallocate(tmpif1)
            if (associated(tmpif2)) deallocate(tmpif2)

            if (allocated(latgdin)) deallocate(latgdin)
            if (allocated(longdin)) deallocate(longdin)

            if (is_integer) then
                if (associated(itmpif1)) deallocate(itmpif1)
                if (associated(itmpif2)) deallocate(itmpif2)
                if (associated(itmpif3)) deallocate(itmpif3)
                if (associated(itmpif4)) deallocate(itmpif4)
            endif
        enddo
    enddo ! ip1_idx = 1, nip1s
    vvent = .false. 
    deallocate(listniv)
end
