! interpolation horizontale d un champ defini par l usager
subroutine scalair(cnom, iheur, nniv, niveaux)
    use app
    use rmn_fst24
    use files, only : nRecords, inputFiles
    use packing, only : npack, npack_orig
    use pgsm_mod, only : nwetike, etikent, tmpif1, tmpif2, message, typeent, ip3ent, ier, mtdone, printen, tmplat
    use accum, only : npas, unefois
    use grilles, only : cgrtyp, gdin, gdout, li, lj, lg1, lg2, lg3, lg4, masque
    use symetry, only : symetri
    use pairs, only : npair, paire
    implicit none

    !> nom du champ 2 caracteres gz.tt.dd.......
    character(len = 4), intent(inout) :: cnom
    !> heure de la variable sur fichier d entre
    integer, intent(in) :: iheur
    !> nombre de niveaux
    integer, intent(in) :: nniv
    !> table contenant nniv niveaux
    integer, dimension(nniv), intent(in) :: niveaux

    external :: ecritur, tourbillon_relatif, imprime, pgsmabt
    external :: cvtrfi, cvtifr
    integer, external :: ezqkdef, ezsint, ezdefset, ezgetopt

    ! interpolation horizontale des scalaires gz, tt, dd, ww, qq, es.
    ! d'une grille a une autre pour nniv niveaux
    ! ecrire resultat sur fichier standard, ms, sequentiel

    character(len = 4) :: cnomvar
    character(len = 4) :: cnomx
    character(len = 12) :: cetiket
    character(len = 2) :: ctypvar

    character(len = 8) :: extrap_option
    integer :: datev, i, nunv
    integer :: iprs, irec, nrecs
    logical :: sym
    integer, dimension(:, :), allocatable, target :: ifld_in, ifld_out

    real, dimension(:, :), pointer :: tmpout

    integer, dimension(:), allocatable :: liste
    logical, dimension(:), allocatable :: done_fields

    type(fst_query) :: query
    type(fst_record), dimension(nRecords) :: records

    nunv = 0

    allocate(liste(nRecords))
    allocate(done_fields(nRecords))
    done_fields = .false.
    cnomx = cnom
    if (cnom == 'QR') cnom = 'QQ'

    do iprs = 1, nniv
        ! conversion de l etiquette d'entree en caracteres
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

        if (cnom == 'MT') then
            if (.not. mtdone) then
                query = inputFiles(1)%new_query(nomvar = cnom)
                nrecs = query%find_all(records)
                call query%free()

                if (nrecs == 0) then
                    if (message) then
                        call app_log(APP_WARNING, 'scalair: Mountain record not found')
                    endif
                endif
                mtdone = .true.
            endif
        else
            call chk_userdate(datev)

            query = inputFiles(1)%new_query(datev = datev, etiket = cetiket, ip1 = niveaux(iprs), ip2 = iheur, ip3 = ip3ent, &
                typvar = ctypvar, nomvar = cnom)
            nrecs = query%find_all(records)
            call query%free()

            if (nrecs == 0) then
                if (message) then
                    call app_log(APP_WARNING, 'scalair: Record does not exist in input file')
                endif
                exit
            endif
        endif

        do irec = 1, nrecs
            if (records(irec)%nk > 1) then
                call app_log(APP_ERROR, 'scalair: PGSM does not accept 3 dimension fields (NK>1)')
                call pgsmabt
            endif

            !  On verifie d'abord que le champ n'a pas ete traite, car les masques associes sont traites ailleurs.
            if (done_fields(irec)) cycle
            npack_orig = -records(irec)%pack_bits
            npas = records(irec)%npas

            if (records(irec)%nomvar(1:2) == '>>' .or. records(irec)%nomvar(1:2) == '^^' .or. records(irec)%nomvar(1:2) == 'HY' .or. &
                records(irec)%nomvar == '^>' .or. records(irec)%nomvar == '!!') cycle

            if (records(irec)%typvar(2:2) == '@'.and. masque == 1) then
                if (ctypvar(1:1) /= '@') then
                    call scalair_msk(irec, records, done_fields, nRecords)
                endif
                cycle
            endif

            extrap_option = '        '
            ier = ezgetopt('extrap_degree', extrap_option)
            if (extrap_option(1:5) == 'abort') then
                gdin = ezqkdef(records(irec)%ni, records(irec)%nj, records(irec)%grtyp, &
                    records(irec)%ig1, records(irec)%ig2, records(irec)%ig3, records(irec)%ig4, inputFiles(1)%get_unit())
                call chk_extrap(gdout, gdin, li, lj, records(irec)%ni, records(irec)%nj)
            endif

            !     si la directive de champ emploi tout  champ(tout, tout) on doit faire
            !     attention pour les vecteurs u-v car l'interpolation serait scalaire
            !     on verifie et les interpolations pour les vecteurs ne sont pas faites

            do i = 1, npair
                if (cnom == '    ') then
                    if ((paire(i)(9:10) == records(irec)%nomvar(1:2)) .or. (paire(i)(13:14) == records(irec)%nomvar(1:2))) then
                        nunv = nunv + 1
                        goto 99999
                    endif
                endif
            enddo

            allocate(tmpif1(records(irec)%ni, records(irec)%nj))

            if (records(irec)%data_type == 2 .or. records(irec)%data_type == 4 .or. &
                records(irec)%data_type == 130 .or. records(irec)%data_type == 132) then

                allocate(ifld_in(records(irec)%ni, records(irec)%nj))
                allocate(ifld_out(li, lj))
                if (.not. records(irec)%read(c_loc(ifld_in))) then
                    call app_log(APP_ERROR, 'scalair: Failed to read field')
                    call pgsmabt
                end if
                call cvtrfi(tmpif1, ifld_in, records(irec)%ni, records(irec)%nj)
            else
                if (.not. records(irec)%read(c_loc(tmpif1))) then
                    call app_log(APP_ERROR, 'scalair: Failed to read field')
                    call pgsmabt
                end if
                call prefiltre(tmpif1, records(irec)%ni, records(irec)%nj, records(irec)%grtyp)
            endif

            if (printen) call imprime(records(irec)%nomvar, tmpif1, records(irec)%ni, records(irec)%nj)
            if (records(irec)%grtyp == 'A' .or. records(irec)%grtyp == 'B' .or. records(irec)%grtyp == 'G') then
                if (records(irec)%ig1 /= 0) sym = symetri(records(irec)%nomvar)
            endif

            if (records(irec)%grtyp /= cgrtyp .or. records(irec)%ni /= li .or. records(irec)%nj /= lj .or. &
                records(irec)%ig1 /= lg1 .or. records(irec)%ig2 /= lg2 .or. records(irec)%ig3 /= lg3 .or. records(irec)%ig4 /= lg4) then
                ! interpolation
                allocate(tmpif2(li, lj))
                gdin = ezqkdef(records(irec)%ni, records(irec)%nj, records(irec)%grtyp, &
                    records(irec)%ig1, records(irec)%ig2, records(irec)%ig3, records(irec)%ig4, inputFiles(1)%get_unit())
                ier = ezdefset(gdout, gdin)
                ier = ezsint(tmpif2, tmpif1)
                tmpout => tmpif2
            else
                tmpout => tmpif1
                if (message) then
                    write(app_msg, "(2x, 'AUCUNE INTERPOLATION HORIZONTALE CHAMP=', a4)") records(irec)%nomvar
                    call app_log(APP_WARNING, app_msg)
                endif
            endif

            ! ecrire sur fichier approprie(std, ms, seq)
            if (cnomx == 'QR') then
                call tourbillon_relatif(tmpif2, li, lj, tmplat)
                cnomvar = 'QR'
            else
                cnomvar = records(irec)%nomvar
            endif

            if (records(irec)%data_type == 2 .or. records(irec)%data_type == 4 .or. &
                records(irec)%data_type == 130 .or. records(irec)%data_type == 132) then

                call cvtifr(ifld_out, tmpout, li, lj)
                call iecritur(ifld_out, npack, records(irec)%dateo, records(irec)%deet, records(irec)%npas, li, lj, records(irec)%nk, &
                    records(irec)%ip1, records(irec)%ip2, records(irec)%ip3, records(irec)%typvar, cnomvar, records(irec)%etiket, &
                    cgrtyp, lg1, lg2, lg3, lg4)
                deallocate(ifld_in, ifld_out)
            else
                call ecritur(tmpout, npack, records(irec)%dateo, records(irec)%deet, records(irec)%npas, li, lj, records(irec)%nk, &
                    records(irec)%ip1, records(irec)%ip2, records(irec)%ip3, records(irec)%typvar, cnomvar, records(irec)%etiket, &
                    cgrtyp, lg1, lg2, lg3, lg4)
                if (associated(tmpif2)) then
                    deallocate(tmpif2)
                endif
                deallocate(tmpif1)
            endif

99999       continue

            if (unefois) goto 23000
        enddo
    enddo ! iprs = 1, nniv
23000 continue

    if (nunv > 0) then
        call app_log(APP_WARNING, 'scalair: No interpolation on variable pair CHAMP(TOUT, TOUT), you have to use a variable name ex: CHAMP(UU, TOUT)')
        call app_log(APP_WARNING, 'scalair: Vector interpolation will be scalar')
    endif
    deallocate(liste)
end


subroutine cvtrfi(rfld, ifld, ni, nj)
    implicit none

    !> Size of the array
    integer, intent(in) :: ni
    !> Size of the array
    integer, intent(in) :: nj
    !> Input array (intger)
    integer, dimension(ni, nj), intent(in) :: ifld
    !> Output array (real)
    real, dimension(ni, nj), intent(out) :: rfld

    integer :: i, j

    do j = 1, nj
        do i = 1, ni
            rfld(i, j) = real(ifld(i, j))
        enddo
    enddo
end

!> Convert real array to integer array
subroutine cvtifr(ifld, rfld, ni, nj)
    implicit none

    !> Size of the array
    integer, intent(in) :: ni
    !> Size of the array
    integer, intent(in) :: nj
    !> Input array (real)
    real, dimension(ni, nj), intent(in) :: rfld
    !> Output array (integer)
    integer, dimension(ni, nj), intent(out) :: ifld

    integer :: i, j

    do j = 1, nj
        do i = 1, ni
            ifld(i, j) = nint(rfld(i, j))
        enddo
    enddo
end
