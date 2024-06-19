!> interpolation horizontale d un champ defini par l usager
subroutine scalair_msk(irec, records, done, nRecords)
    use app
    use rmn_fst24
    use files, only : inputFiles
    use packing, only : npack
    use accum, only : npas
    use pgsm_mod, only: tmplat, tmpif1, tmpif2, message
    use grilles, only : cgrtyp, gdin, gdout, lg1, lg2, lg3, lg4, li, lj
    use symetry, only : symetri
    implicit none

    integer, intent(in) :: irec
    integer, intent(in) :: nRecords
    type(fst_record), dimension(nRecords), intent(inout) :: records
    logical, dimension(nRecords), intent(inout) :: done

    external ecritur, tourbillon_relatif, imprime
    external cvtifr
    integer, external :: ezgdef_fmem
    integer, external :: ezqkdef
    integer, external :: ezsint
    integer, external :: ezsint_mdm
    integer, external :: ezdefset
    integer, external :: get_mask

    integer :: i
    logical, save :: unefoys = .true.
    logical :: sym

    real, dimension(:, :), allocatable, target ::  fld, fld_out
    integer, dimension(:, :), allocatable, target :: mask, mask_out, mask_zones

    real, dimension(:, :), pointer :: tmpout
    integer, dimension(:, :), pointer :: tmpmsk

    logical mask_present, mask_done

    type(fst_record) :: mask_record

    character(len = *), parameter :: msg_fmt = "(2x, 'scalair_msk: No horizonal interpolation CHAMP=', a2)"

    mask_present = .false.
    mask_done = .false.

    allocate(fld(records(irec)%ni, records(irec)%nj), fld_out(records(irec)%li, records(irec)%lj))

    ier = get_mask(mask_record, records(irec))
    if (ier >= 0) then
        mask_present = .true.
        allocate(mask(ni, nj), mask_out(li, lj))
    else
        mask_present = .false.
    endif

    records(irec)%read()
    call records(irec)%get_data_array(fld)
    call prefiltre(fld, records(irec)%ni, records(irec)%nj, records(irec)%grtyp)
    npas = records(irec)%npas

    if (records(irec)%datyp == 2 .or. records(irec)%datyp == 4) then
        call cvtifr(fld, fld, records(irec)%ni, records(irec)%nj)
    endif

    if (printen) call imprime(records(irec)%nomvar, tmpif1, records(irec)%ni, records(irec)%nj)
    if (records(irec)%ig1 /= 0) sym = symetri(records(irec)%nomvar)

    if (.not. mask_present) then
        ! on ne fait pas d'interpolation si igtyp=grtyp  ig1=lg1  ig2=lg2 ig3=lg3  ig4=lg4
        if (records(irec)%grtyp /= cgrtyp .or. records(irec)%grtyp == 'Z' .or. &
            records(irec)%ig1 /= lg1 .or. records(irec)%ig2 /= lg2 .or. records(irec)%ig3 /= lg3 .or. records(irec)%ig4 /= lg4 .or. &
            records(irec)%ni /= li .or. records(irec)%nj /= lj) then

            gdin = ezqkdef(records(irec)%ni, records(irec)%nj, records(irec)%grtyp, &
                records(irec)%ig1, records(irec)%ig2, records(irec)%ig3, records(irec)%ig4, inputFiles(1)%get_unit())
            ier = ezdefset(gdout, gdin)
            ier = ezsint(fld_out, fld)
            tmpout => fld_out
        else
            tmpout => fld
            if (message) then
                write(app_msg, msg_fmt) records(irec)%nomvar
                call app_log(APP_INFO, app_msg)
            endif
        endif

        if (records(irec)%nomvar == 'QR') then
            app_msg = 'nomvar = ' // trim(records(irec)%nomvar) // &
                ' is supposed to be converted to relative vorticity, but that code has been broken for years!' // &
                new_line('a') // 'Please let service rpn know about it by filing a bug report at:' // &
                new_line('a') // 'https://gitlab.science.gc.ca/RPN-SI/Support/'
            call app_log(APP_FATAL, app_msg)
            call tourbillon_relatif(tmpif2, li, lj, tmplat)
        endif
        call ecritur(tmpout, records(irec)%npack, records(irec)%dateo, records(irec)%deet, records(irec)%npas, li, lj, records(irec)%nk, &
            records(irec)%ip1, records(irec)%ip2, records(irec)%ip3, records(irec)%typvar, records(irec)%nomvar, records(irec)%etiket, &
            cgrtyp, lg1, lg2, lg3, lg4)
        deallocate(fld_out)
    else
        if (records(irec)%grtyp /= cgrtyp .or. records(irec)%grtyp == 'Z' .or. &
            records(irec)%ig1 /= lg1 .or. records(irec)%ig2 /= lg2 .or. records(irec)%ig3 /= lg3 .or. records(irec)%ig4 /= lg4 .or. &
            records(irec)%ni /= li .or. records(irec)%nj /= lj) then

            gdin = ezqkdef(records(irec)%ni, records(irec)%nj, records(irec)%grtyp, &
                records(irec)%ig1, records(irec)%ig2, records(irec)%ig3, records(irec)%ig4, inputFiles(1)%get_unit())
            ier = ezdefset(gdout, gdin)
            mask_record%read()
            call mask_record%get_data_array(mask)
            ier = ezsint_mdm(fld_out, mask_out, fld, mask)
            tmpout => fld_out
            tmpmsk => mask_out
        else
            tmpout => fld
            tmpmsk => mask
            if (message) then
                write(app_msg, msg_fmt) records(irec)%nomvar
                call app_log(APP_INFO, app_msg)
            endif
        endif
        ! ecrire sur fichier approprie(std, ms, seq)
        if (records(irec)%nomvar == 'QR') then
            app_msg = 'nomvar = ' // trim(records(irec)%nomvar) // &
                ' is supposed to be converted to relative vorticity, but that code has been broken for years!' // &
                new_line('a') // 'Please let service rpn know about it by filing a bug report at:' // &
                new_line('a') // 'https://gitlab.science.gc.ca/RPN-SI/Support/'
            call app_log(APP_FATAL, app_msg)
            call tourbillon_relatif(tmpif2, li, lj, tmplat)
        endif
        call ecritur(tmpout, records(irec)%npack, records(irec)%dateo, records(irec)%deet, records(irec)%npas, li, lj, records(irec)%nk, &
            records(irec)%ip1, records(irec)%ip2, records(irec)%ip3, records(irec)%typvar, records(irec)%nomvar, records(irec)%etiket, &
            cgrtyp, lg1, lg2, lg3, lg4)
        mask_done = .false.
        do i = 1, nRecords
            if (mask_record%is_same(records(i))) then
                if (done(i)) then
                    mask_done = .true.
                endif
                exit
            endif
        enddo
        if (mask_present .and. .not. mask_done) then
            if (unefoys) then
                allocate(mask_zones(li, lj))
                call ezget_mask_zones(mask_zones, mask)
                call iecritur(mask_zones, -16, mask_record%dateo, mask_record%deet, mask_record%npas, li, lj, 1, &
                    mask_record%ip1, mask_record%ip2, mask_record%ip3, '@Z', mask_record%nomvar, mask_record%etiket, &
                    cgrtyp, lg1, lg2, lg3, lg4)
                unefoys = .false.
                deallocate(mask_zones)
            endif
            call iecritur(tmpmsk, -mask_record%nbits, mask_record%dateo, mask_record%deet, mask_record%npas, li, lj, 1, &
                    mask_record%ip1, mask_record%ip2, mask_record%ip3, '@@', mask_record%nomvar, mask_record%etiket, &
                    cgrtyp, lg1, lg2, lg3, lg4)
            do i = 1, nRecords
                if (mask_record%is_same(records(i))) then
                    done(i) = .true.
                    exit
                endif
            enddo
            if (allocated(mask_out)) then
                deallocate(mask_out)
            endif
        endif
        deallocate(fld_out)

    endif
    deallocate(fld)
end