!> Interpoler des vecteurs u-v (horizontalement)
subroutine uvecteur_masque(uu_record, vv_record)
    use app
    use rmn_fst24
    use packing, only : npack
    use pgsm_mod, only : vvent, wdvent, message
    use accum, only : npas
    use grilles, only : cgrtyp, gdin, gdout, lg1, lg2, lg3, lg4, li, lj, masque
    use symetry, only : symetri
    implicit none

    type(fst_record) :: uu_record
    type(fst_record) :: vv_record

    external ecritur, imprime

    integer :: ezqkdef, ezuvint, ezuvint_mdm, ezwdint, ezdefset, get_mask

#include "defin.cdk90"

    character(len = 12):: cetiket
    character(len = 2) :: ctypvar
    character(len = 1) :: cigtyp

    integer :: i, j
    integer :: ier

    logical :: sym

    character(len = 2), parameter :: ctypvar_mask = '@@'

    real, dimension(:, :), allocatable, target :: tmp_uuin, tmp_uuout, tmp_vvin, tmp_vvout
    integer, dimension(:, :), allocatable, target :: mask_in, mask_out

    real, dimension(:, :), pointer :: uu_in, vv_in, uu_out, vv_out
    integer, dimension(:, :), pointer :: tmpmsk

    type(fst_record) :: mask_record
    type(fst_record) :: mask_out_record

    allocate(tmp_uuout(li, lj), tmp_vvout(li, lj))

    ier = uu_record%read()
    call uu_record%get_data_array(tmp_uuin)
    call prefiltre(tmp_uuim, uu_record%ni, uu_record%nj, uu_record%grtyp)
    ier = vv_record%read()
    call vv_record%get_data_array(tmp_vvin)
    call prefiltre(tmp_vvim, vv_record%ni, vv_record%nj, vv_record%grtyp)
    npas = vv_record%npas

    if (printen) call imprime(uu_record%nomvar, tmp_uuin, uu_record%ni, uu_record%nj)
    if (vv_record%ig1 /= 0) sym = symetri(uu_record%nomvar)

    ier = get_mask(mask_record, uu_record)
    if (ier < 0) then
        !  interpolation ordinaire sans masque
        gdin = ezqkdef(vv_record%ni, vv_record%nj, vv_record%grtyp, vv_record%ig1, vv_record%ig2, vv_record%ig3, vv_record%ig4, 1)
        ier = ezdefset(gdout, gdin)

        ! si vvent = .true. on calcule la vitesse du vent
        if (vvent) then
            ier = ezwdint(tmp_uuout, tmp_vvout, tmp_uuin, tmp_vvout)
            uu_out => tmp_uuout

            call ecritur(uu_out, npack, vv_record%dateo, vv_record%deet, vv_record%npas, li, lj, vv_record%nk, &
                vv_record%ip1, vv_record%ip2, vv_record%ip3, vv_record%typvar, 'UV  ', vv_record%etiket, cgrtyp, lg1, lg2, lg3, lg4)

            if (wdvent) then
                do j = 1, lj
                    do i = 1, li
                        if (tmp_vvout(i, j) < 0.0) then
                            tmp_vvout(i, j) = tmp_vvout(i, j) + 360.0
                        endif
                    enddo
                enddo
                vv_out => tmp_vvout
                call ecritur(vv_out, npack, vv_record%dateo, vv_record%deet, vv_record%npas, li, lj, vv_record%nk, &
                    vv_record%ip1, vv_record%ip2, vv_record%ip3, vv_record%typvar, 'WD  ', vv_record%etiket, cgrtyp, lg1, lg2, lg3, lg4)
            endif
        else
            if (vv_record%grtyp /= cgrtyp .or. vv_record%ig1 /= lg1 .or. vv_record%ig2 /= lg2 .or. vv_record%ig3 /= lg3 .or. vv_record%ig4 /= lg4 &
                .or. vv_record%ni /= li .or. vv_record%nj /= lj) then
                ier = ezuvint(tmp_uuout, tmp_vvout, tmp_uuin, tmp_vvout)
                uu_out => tmp_uuout
                vv_out => tmp_vvout
            else
                deallocate(tmp_uuout)
                deallocate(tmp_vvout)
                uu_out => tmp_uuin
                vv_out => tmp_vvout
                if (message) then
                    call app_log(APP_WARNING, 'uvecteur_masque: No horizontal interpolation')
                endif
            endif
            call ecritur(uu_out, npack, vv_record%dateo, vv_record%deet, vv_record%npas, li, lj, vv_record%nk, &
                vv_record%ip1, vv_record%ip2, vv_record%ip3, vv_record%typvar, uu_record%nomvar, vv_record%etiket, cgrtyp, lg1, lg2, lg3, lg4)
            call ecritur(vv_out, npack, vv_record%dateo, vv_record%deet, vv_record%npas, li, lj, vv_record%nk, &
                vv_record%ip1, vv_record%ip2, vv_record%ip3, vv_record%typvar, vv_record%nomvar, vv_record%etiket, cgrtyp, lg1, lg2, lg3, lg4)
        endif
    else
        ! Masque present
        allocate(mask_in(vv_record%ni, vv_record%nj), mask_out(li, lj))

        if (vv_record%grtyp /= cgrtyp .or. vv_record%grtyp == 'Z' .or. &
            vv_record%ig1 /= lg1 .or. vv_record%ig2 /= lg2 .or. vv_record%ig3 /= lg3 .or. vv_record%ig4 /= lg4 .or. &
            vv_record%ni /= li .or. vv_record%nj /= lj) then
            gdin = ezqkdef(vv_record%ni, vv_record%nj, vv_record%grtyp, vv_record%ig1, vv_record%ig2, vv_record%ig3, vv_record%ig4, 1)
            ier = ezdefset(gdout, gdin)
            ier = mask_record%read()
            call mask_record%get_data_array(mask_in)
            ier = ezuvint_mdm(tmp_uuout, tmp_vvout, mask_out, tmp_uuin, tmp_vvin, mask_in)
            uu_out => tmp_uuout
            vv_out => tmp_vvout
            tmpmsk => mask_out
        else
            uu_out => tmp_uuin
            vv_out => tmp_vvin
            tmpmsk => mask_in
            if (message) then
                write(app_msg, 662) uu_record%nomvar
662           format(2x, 'uvecteur_masque: No horizontal interpolation CHAMP=', a4)
                call app_log(APP_WARNING, app_msg)
            endif
        endif

        call ecritur(uu_out, npack, vv_record%dateo, vv_record%deet, vv_record%npas, li, lj, vv_record%nk, &
            vv_record%ip1, vv_record%ip2, vv_record%ip3, vv_record%typvar, uu_record%nomvar, vv_record%etiket, cgrtyp, lg1, lg2, lg3, lg4)
        call ecritur(vv_out, npack, vv_record%dateo, vv_record%deet, vv_record%npas, li, lj, vv_record%nk, &
            vv_record%ip1, vv_record%ip2, vv_record%ip3, vv_record%typvar, vv_record%nomvar, vv_record%etiket, cgrtyp, lg1, lg2, lg3, lg4)

        ! On regarde si le masque existe dans le fichier de sortie
        mask_out_query = outputFile%new_query(datev = mask_record%datev, etiket = mask_record%etiket, &
            ip1 = mask_record%ip1, ip2 = mask_record%ip2, ip3 = mask_record%ip3, typvar = mask_record%typvar, nomvar = mask_record%nomvar)
        ier = mask_out_query%find_next(mask_out_record)
        call mask_out_query%free()

        if (ier < 0) then
            call iecritur(tmpmsk, -mask_record%nbits, mask_record%dateo, mask_record%deet, mask_record%npas, li, lj, vv_record%nk, &
                mask_record%ip1, mask_record%ip2, mask_record%ip3, mask_record%typvar, mask_record%nomvar, mask_record%etiket, &
                cgrtyp, lg1, lg2, lg3, lg4)
            if (allocated(mask_out)) then
                deallocate(mask_out, mask_in)
            endif
        endif
        mask_out_record%free()
        deallocate(tmp_uuout, tmp_uuin, tmp_vvout, tmp_vvin)
    endif

    uu_record%free()
    vv_record%free()
end
