!> Miroir de la directive champ pour fichiers sequentiels
#include "defin.cdk90"
subroutine champ_seq (listn, listip1, waitOrGo)
    !> Appel via directive champ_seq(listn, listip1, waitOrGo)

    use app
    use files
    implicit none

    !> Liste de nomvar
    integer, dimension(*), intent(in) :: listn
    !> Liste de niveau
    integer, dimension(*), intent(in) :: listip1
    !> Commutateur d'accumulation de directives
    integer, intent(in) :: waitOrGo

    integer, external :: pgsmluk
    integer, external ::  pgsmlir
    external ecritur, symetri
    external argdims, pgsmabt, grille2
    integer ezqkdef, ezsint, ezdefset

#include "voir.cdk90"
#include "llccmm.cdk90"
#include "accum.cdk90"
#include "champs.cdk90"
#include "indptr.cdk90"
#include "grilles.cdk90"
#include "lires.cdk90"
#include "ecrires.cdk90"
#include "dates.cdk90"
#include "packin.cdk90"
#include "dummys.cdk90"
#include "gdz.cdk90"
#include "champseq.cdk90"
#include "heures.cdk90"
#include "styles.cdk90"

    character*12 etiket
    character*4 nomvar
    character*2 typvar
    character*1 cigtyp

    integer ig1, ig2, ig3, ig4, irec, iunit
    integer deet
    integer ip1, ip2, ip3, i, j, k, date
    integer ni, nj
    integer argdims
    logical symetri, heureok, processed
    character*8 string
    real :: ptr
    real :: fbidon
    type(fst_record) :: record
    type(fst_query) :: query


    iunit = 1

    if (npar /= 3) then
        if (message) then
            call app_log(APP_ERROR, 'champ_seq: Directive CHAMP_SEQ should have 3 arguments')
        endif
        return
    endif

    if (.not. associated(tmplat)) then
        if (message) then
            call app_log(APP_WARNING, 'champ_seq: Grid not defined, will use PS(2805)')
        endif
        call grille2(3, 51, 55, 26., 28., 381000., 350., 1)
    endif

    !   trouver nombre d'arguments dans une liste (ip1, ip2, ip3)
    ntitems = ntitems + 1
    if (ntitems > nmaxlist1) then
        call app_log(APP_ERROR, 'champ_seq: Limit of 16 directives CHAMP_SEQ has been passed')
        call pgsmabt
    endif

    if (argdims(1) > nmaxlist2) then
        call app_log(APP_ERROR, 'champ_seq: Limit of 16 variable names has been passed')
        call pgsmabt
    endif

    if (argdims(2) > nmaxlist2) then
        call app_log(APP_ERROR, 'champ_seq: Limit of 16 vertical levels has been passed')
        call pgsmabt
    endif

    nitems1(ntitems) = argdims(1)
    nitems2(ntitems) = argdims(2)
    do i = 1, argdims(1)
        write(listnom(ntitems, i), '(A2)') listn(i)
    enddo

    do i = 1, argdims(2)
        listniv(ntitems, i) = listip1(i)
    enddo

    if (listniv(ntitems, 1) > 1000000 .and. listniv(ntitems, 2)  < 0) then
        do i = 1, argdims(2), 2
            ptr = transfer(listniv(ntitems, i), ptr)
            call convip_plus(listniv(ntitems, i / 2 + 1), ptr, -1 * listniv(ntitems, (i + 1)) - 1000, ip1style, string, .false.)
         enddo
         nitems2(ntitems) = argdims(2) / 2
    endif

    if (waitOrGo == WAIT) return

    ! ier = fstrwd(lnkdiun(1))
    ier = inputFiles(1)%rewind()
    if (ier  >  1) then
        call app_log(APP_ERROR, 'champ_seq: Rewinding file has failed!')
        call pgsmabt
    endif
    ! irec = fstsel(1, ni, nj, nk, -1, '        ', -1, -1, -1, ' ', '  ')

    ! irec = fstsui(1, ni, nj, nk)
    query = inputFiles(1)%new_query()
    !> \todo Figure out if inputFiles(1)%set_search_criteria() is necessary?

    do while (query%find_next(record))
        processed = .false.
        ! ier = fstprm(irec, date, deet, npas, ni, nj, nk, nbits, datyp, ip1, ip2, ip3, typvar, nomvar, etiket, cigtyp, ig1, ig2, ig3, ig4, swa, lng, dltf, ubc, extra1, extra2, extra3)
        ! Pas ncessaire avec fst24

        if (record%nk > 1) then
            call app_log(APP_ERROR, 'champ_seq: PGSM does not allow for 3 dimension fields (NK > 1)')
            call pgsmabt
        endif

        heureok = .false.
        if (heures(1) == -1) then
            heureok = .true.
        else
            do k = 1, nhur
                if (record%ip2 == heures(k)) then
                    heureok = .true.
                endif
            enddo
        endif

        if (heureok .and. .not. processed) then
            do i = 1, ntitems
                if (.not. processed) then
                    do j = 1, nitems1(i)
                        if (listnom(i, j) == record%nomvar .or. listnom(i, j)  == ' ' .and. .not. processed) then
                            do k = 1, nitems2(i)
                                if (listniv(i, k) == ip1 .or. listniv(i, k) == -1 .and. .not. processed) then
                                    allocate(tmpif1(ni, nj))
                                    allocate(tmpif2(li, lj))
                                    ! \todo Is grtyp input only in pgsmluk?
                                    ier = pgsmluk(tmpif1, irec, record%ni, record%nj, record%nk, record%nomvar, record%grtyp)

                                    gdin = ezqkdef(ni, nj, cigtyp, ig1, ig2, ig3, ig4, iunit, fbidon, fbidon)
                                    ier = ezdefset(gdout, gdin)
                                    ier = ezsint(tmpif2, tmpif1)

                                    call ecritur(tmpif2, npack, date, deet, npas, li, lj, 1, ip1, ip2, ip3, typvar, nomvar, etiket, cgrtyp, lg1, lg2, lg3, lg4)

                                    deallocate(tmpif2)
                                    deallocate(tmpif1)
                                    processed = .true.
                                endif
                            enddo
                        endif
                    enddo
                endif
            enddo
        endif
        ! irec = fstsui(1, ni, nj, nk)
    enddo
    call query%free()

    ! l'interpolation est terminee - On a passé a travers le fichier
    do i = 1, ntitems
        do j = 1, nitems2(i)
            listnom(i, j) = '  '
            listniv(i, j) = -1
        enddo
        nitems2(i) = 0
    enddo
    ntitems = 0
end
