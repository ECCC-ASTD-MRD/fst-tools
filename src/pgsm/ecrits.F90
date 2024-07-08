!> Ecrire sur fichier standard, ms, sequentiel
subroutine ecrits(nom, npac, idat, ip1, ip2, ip3, type, etiqet, igtyp, imprim, ig1srt, ig2srt, ig3srt, ig4srt)
    use app
    use rmn_fst24
    use files, only: outputFile, outputFileMode
    use pgsm_mod, only: ier, iwrit, tmpif0, message
    use accum, only : nni, nnj, nnk, idatt, ideet, npas, jpp1, jpp2, jpp3, igg1, igg2, igg3, igg4, cnumv, ctypv, cetik, cigty, icnt
    use ecrires, only : compression_level, printsr
    use chck, only : ichck, necrt
    implicit none

    !> Field name
    integer, intent(in) :: nom
    !> Data compaction
    integer, intent(in) :: npac
    !> Origin date (CMC stamp)
    integer, intent(inout) :: idat
    !> Level
    integer, intent(in) :: ip1(2)
    !> Forecast hour
    integer, intent(inout) :: ip2
    !> User defined identifier
    integer, intent(inout) :: ip3
    !> Field type
    integer, intent(in) :: type
    !> Label
    integer, intent(in) :: etiqet(*)
    !> Grid type
    integer, intent(in) :: igtyp
    !> Print field information if true
    integer, intent(in) :: imprim
    !> First grid parameter
    integer, intent(in) :: ig1srt
    !> Second grid parameter
    integer, intent(in) :: ig2srt
    !> Third grid parameter
    integer, intent(in) :: ig3srt
    !> Forth grid parameter
    integer, intent(in) :: ig4srt

#include "blancs.cdk90"

    external conver, pgsmabt, imprims, putfld
    integer, external :: fstopc, fstcvt
    integer, external :: argdims

    character(*), parameter :: fmt_in = "('ecrits: ENTRE     ', a2, 2x, i10, 3x, i5, 3x, i2, 3x, i3, 4x, a1, 4x, a10, 3x, a1)"
    character(*), parameter :: fmt_out = "('ecrits: SORTIE    ', a2, 2x, i10, 3x, i5, 3x, i2, 3x, i3, 4x, a1, 4x, a10, 3x, a1)"
    character(*), parameter :: fmt_wrt = "(2x, 'ecritur:  Record written ', 2(a2, '- '), 3(i5, '- '), 'size ', 2(i5, '- '), 'file SEQUENTIEL')"

    type(fst_record) :: record
    logical :: success

    character(len = 12) :: cetiqet
    character(len = 4) :: cnomvar
    character(len = 2) :: ctypvar
    character(len = 1) :: cigtyp

    integer :: iopc
    integer :: npkc
    integer :: cdatyp
    integer :: ig1s, ig2s, ig3s, ig4s
    integer :: rewrit
    integer :: letiket(3)

    integer :: lip1
    real :: ptr
    character(len = 8) :: junk

    cnomvar = '    '
    ctypvar = '  '
    cetiqet = '            '
    cigtyp  = ' '
    letiket(1) = etiqet(1)
    letiket(2) = blancs
    letiket(3) = blancs
    if (argdims(8) > 1) then
        letiket(2) = etiqet(2)
    endif
    if (argdims(8) > 2) then
        letiket(3) = etiqet(3)
    endif

    lip1 = ip1(1)
    if (argdims(4) > 1) then
        ptr = transfer(ip1(1), ptr)
        call convip_plus(lip1, ptr, -1 * ip1(2) - 1000, 2, junk, .false.)
    endif

    ier = fstcvt(nom, type, letiket, igtyp, cnomvar, ctypvar, cetiqet, cigtyp, .true.)

    if (nom == -1) cnomvar = cnumv
    if (type == -1) ctypvar = ctypv
    if (etiqet(1) == -1) cetiqet = cetik
    if (igtyp == -1) cigtyp = cigty

    npkc = npac
    if (npac == -1) npkc = -16
    if (idat == -1) idat = idatt
    if (lip1 == -1) lip1 = jpp1
    if (ip2 == -1) ip2 = jpp2
    if (ip3 == -1) ip3 = jpp3
    if (ip3 == 4095) ip3 = icnt

    if (necrt < 11) then
        ig4s = igg4
        ig3s = igg3
        ig2s = igg2
        ig1s = igg1
    endif

    if (necrt == 11) then
        ig4s = igg4
        ig3s = igg3
        ig2s = igg2
        ig1s = ig1srt
    endif

    if (necrt == 12)  then
        ig4s = igg4
        ig3s = igg3
        ig2s = ig2srt
        ig1s = ig1srt
    endif

    if (necrt == 13) then
        ig4s = igg4
        ig3s = ig3srt
        ig2s = ig2srt
        ig1s = ig1srt
    endif

    if (necrt == 14) then
        ig4s = ig4srt
        ig3s = ig3srt
        ig2s = ig2srt
        ig1s = ig1srt
    endif

    if (necrt > 9) then
        write(app_msg, fmt_in) cnumv, idatt, jpp1, jpp2, jpp3, ctypv, cetik, cigty
        call app_log(APP_INFO, app_msg)
        write(app_msg, fmt_out) cnomvar, idat, lip1, ip2, ip3, ctypvar, cetiqet, cigtyp
        call app_log(APP_INFO, app_msg)
    endif

    if (ichck == 0)  then
        call app_log(APP_ERROR, 'ecrits: Directive LIREN or LIRSR must be called before ECRITS')
        call pgsmabt
    endif

    ! SI LE NOM EXISTE DANS LA TABLE BATIE PAR L USAGER ALORS LE CHAMP EST MODIFIE
    ! EX: ACUMULA(NNI, NNJ) =  AMAX1(BAS, AMIN1(HAUT, (ACUMULA(NNI, NNJ) + ECART)*FACT))
    call conver(tmpif0, nni, nnj, cnomvar)

    if (printsr) then
        call imprims(cnomvar, tmpif0, nni, nnj)
    endif

    if (outputFileMode == 1) then
        if (compression_level == 0) then
            cdatyp = 1
        else
            if (npac <= -16) then
                cdatyp = 134
            else if (npac == -32) then
                cdatyp = 133
            else
                cdatyp = 1
            endif
        endif

        npkc = abs(npac)

        if (iwrit == +1) then
            rewrit = FST_YES
        else
            rewrit = FST_NO
        endif
        if (.not. message) iopc = fstopc('TOLRNC', 'DEBUGS', .true.)
        record%data = c_loc(tmpif0)
        record%ni = nni
        record%nj = nnj
        record%nk = nnk
        record%data_type = cdatyp
        record%data_bits = 32
        record%pack_bits = npkc
        record%dateo = idat
        record%deet = ideet
        record%npas = npas
        record%ip1 = lip1
        record%ip2 = ip2
        record%ip3 = ip3
        record%typvar = ctypvar
        record%nomvar = cnomvar
        record%etiket = cetiqet
        record%grtyp = cigtyp
        record%ig1 = ig1s
        record%ig2 = ig2s
        record%ig3 = ig3s
        record%ig4 = ig4s
        success = outputFile%write(record, rewrite = rewrit)
    else if (outputFileMode == 3) then
        !> \todo Handle non-fst files
        ! write(iun) (tmpif0(i), i=1, nni * nnj)
        if (message) then
            write(app_msg, fmt_wrt) ctypvar, cnomvar, lip1, ip2, ip3, nni, nnj
            call app_log(APP_INFO, app_msg)
        endif
    else
        if (message) then
            call app_log(APP_ERROR, 'ecrits: Unknown file')
        endif
    endif

    deallocate(tmpif0)
end
