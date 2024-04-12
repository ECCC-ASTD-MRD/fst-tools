integer function chkenrpos(linputFile, loutputFile, ip1, ip2, ip3)
    use files
    implicit none

#include "indptr.cdk90"

    type(fst_file), intent(in) :: linputFile
    type(fst_file), intent(in) :: loutputFile
    integer, intent(in) :: ip1
    integer, intent(in) :: ip2
    integer, intent(in) :: ip3

    real, dimension (:), allocatable :: ax, ay

    integer :: lip1, lip2, lip3

    integer, external :: fstinf
    integer, external :: fstluk
    integer, external :: fstecr
    integer, external :: fstprm

    integer :: ni1, nj1, nk1, ni2, nj2, nk2
    integer :: ier, ier2, yy_key, ax_key, ay_key
    logical :: yinyang_grid

    character(len = 2) :: typvarx, typvary, grref
    character(len = 12) :: etikx, etiky
    character(len = 4) :: nomx, nomy
    integer :: dateo, deet, npas, nbits, datyp, ig1ref, ig2ref, ig3ref, ig4ref, swa, lng, dltf, ubc, extra1, extra2, extra3

    type(fst_query) :: tic_query
    type(fst_query) :: tac_query
    type(fst_query) :: tictac_query
    type(fst_record) :: tic
    type(fst_record) :: tac
    type(fst_record) :: tictac

    integer :: tic_found = 0
    integer :: tac_found = 0
    integer :: yy_found = 0

    yinyang_grid = .false.
    chkenrpos = -1

    if (outputFileMode == 5) then
        chkenrpos = 0
        return
    endif

    if (outputFileMode == 1) then
        ! ax_key = fstinf(luout, ni1, nj1, nk1, -1, '            ', ip1, ip2, ip3, '  ', '>>  ')
        ! ay_key = fstinf(luout, ni1, nj1, nk1, -1, '            ', ip1, ip2, ip3, '  ', '^^  ')
        tic_query = outputFile%new_query(ip1 = ip1, ip2 = ip2, ip3 = ip3, nomvar = '>>  ')
        tic_found = tic_query%find_count()
        call tic_query%free()
        tac_query = outputFile%new_query(ip1 = ip1, ip2 = ip2, ip3 = ip3, nomvar = '^^  ')
        tac_found = tac_query%find_count()
        call tac_query%free()

        if (tic_found > 0 .and. tac_found > 0) then
            chkenrpos = 0
        endif

        ! yy_key = fstinf(luout, ni1, nj1, nk1, -1, '            ', ip1, ip2, ip3, '  ', '^>  ')
        yy_query = outputFile%new_query(ip1 = ip1, ip2 = ip2, ip3 = ip3, nomvar = '^>  ')
        yy_found = yy_query%find_count()
        call tictac_query%free()
        if (yy_found > 0) then
            chkenrpos = 1
        endif

        if (chkenrpos == 1 .and. tac_count >= 0) then
            if (yy_key > ax_key .or. yy_key > ay_key) then
                chkenrpos = 0
            endif
        endif

        if (chkenrpos >= 0) return
    endif

    ! ax_key = fstinf(luin, ni1, nj1, nk1, -1, '            ', ip1, ip2, ip3, '  ', '>>  ')
    ! ay_key = fstinf(luin, ni1, nj1, nk1, -1, '            ', ip1, ip2, ip3, '  ', '^^  ')
    ! yy_key = fstinf(luin, ni1, nj1, nk1, -1, '            ', ip1, ip2, ip3, '  ', '^>  ')
    tic_query = inputFiles(1)%new_query(ip1 = ip1, ip2 = ip2, ip3 = ip3, nomvar = '>>  ')
    tac_query = inputFiles(1)%new_query(ip1 = ip1, ip2 = ip2, ip3 = ip3, nomvar = '^^  ')
    yy_query = inputFiles(1)%new_query(ip1 = ip1, ip2 = ip2, ip3 = ip3, nomvar = '^>  ')

    if (ax_key < 0 .or. ay_key < 0) then
        chkenrpos = -1
    else
        chkenrpos = 0
    endif

    if (yy_key >= 0) then
        chkenrpos = 1
        yinyang_grid = .true.
    endif

    if (chkenrpos == 1 .and. ax_key >= 0) then
        if (yy_key > ax_key.or.yy_key > ay_key) then
            chkenrpos = 0
            yinyang_grid = .false.
        endif
    endif
    if (chkenrpos == -1) return

    if (yinyang_grid) then
        ier = fstprm(yy_key, dateo, deet, npas, ni1, nj1, nk1, nbits, datyp, lip1, lip2, lip3, typvarx, nomx, etikx, grref, ig1ref, ig2ref, ig3ref, ig4ref, swa, lng, dltf, ubc, extra1, extra2, extra3)

        ! allocate(ax(ni1 * nj1 * nk1))
        ! ier = fstluk(ax, yy_key, ni1, nj1, nk1)

        call ecritur(ax, -nbits, dateo, deet, npas, ni1, nj1, nk1, lip1, lip2, lip3, typvarx, nomx, etikx, grref, ig1ref, ig2ref, ig3ref, ig4ref)

        ! deallocate(ax)
    else
        ier = fstprm(ax_key, dateo, deet, npas, ni1, nj1, nk1, nbits, datyp, lip1, lip2, lip3, typvarx, nomx, etikx, grref, ig1ref, ig2ref, ig3ref, ig4ref, swa, lng, dltf, ubc, extra1, extra2, extra3)
        ier = fstprm(ay_key, dateo, deet, npas, ni2, nj2, nk2, nbits, datyp, lip1, lip2, lip3, typvary, nomy, etiky, grref, ig1ref, ig2ref, ig3ref, ig4ref, swa, lng, dltf, ubc, extra1, extra2, extra3)

        ! allocate(ax(ni1 * nj1 * nk1))
        ! allocate(ay(ni2 * nj2 * nk2))

        ! ier = fstluk(ax, ax_key, ni1, nj1, nk1)
        ! ier = fstluk(ay, ay_key, ni2, nj2, nk2)

        call ecritur(ax, -nbits, dateo, deet, npas, ni1, nj1, nk1, lip1, lip2, lip3, typvarx, nomx, etikx, grref, ig1ref, ig2ref, ig3ref, ig4ref)

        call ecritur(ay, -nbits, dateo, deet, npas, ni2, nj2, nk2, lip1, lip2, lip3, typvary, nomy, etiky, grref, ig1ref, ig2ref, ig3ref, ig4ref)

        ! deallocate(ax)
        ! deallocate(ay)
    endif
end
