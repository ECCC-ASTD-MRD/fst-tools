subroutine copy2outputFile(nomvar)
    use app
    use rmn_fst24
    use files, only : inputFiles, outputFile
    implicit none

    character(len = 4), intent(in) :: nomvar

    integer, external :: fstopl
    integer :: dummy

    type(fst_query) :: inQuery
    type(fst_query) :: outQuery
    type(fst_record) :: inRec
    type(fst_record) :: outRec

    dummy = fstopl('REDUCTION32', .false., .false.)

    inQuery = inputFiles(1)%new_query(nomvar = nomvar)
    do while (inQuery%find_next(inRec))
        outQuery = outputFile%new_query(dateo = inRec%dateo, etiket = inRec%etiket, &
            ip1 = inRec%ip1, ip2 = inRec%ip2, ip3 = inRec%ip3, typvar = inRec%typvar, nomvar = inRec%nomvar)
        if (.not. outQuery%find_next(outRec)) then
            if (.not. inputFiles(1)%read(inRec)) then
                call app_log(APP_ERROR, 'chk_toctoc: Failed to read toctoc!')
                call pgsmabt
            end if
            ! outRec%data = c_loc(hydata)
            ! outRec%ni = inRec%ni
            ! outRec%nj = inRec%nj
            ! outRec%nk = inRec%nk
            ! outRec%data_type = inRec%data_type
            ! outRec%data_bits = inRec%data_bits
            ! outRec%pack_bits = inRec%pack_bits
            ! outRec%dateo = inRec%dateo
            ! outRec%deet = inRec%deet
            ! outRec%npas = inRec%npas
            ! outRec%ip1 = inRec%ip1
            ! outRec%ip2 = inRec%ip2
            ! outRec%ip3 = inRec%ip3
            ! outRec%typvar = inRec%typvar
            ! outRec%nomvar = inRec%nomvar
            ! outRec%etiket = inRec%etiket
            ! outRec%grtyp = inRec%grtyp
            ! outRec%ig1 = inRec%ig1
            ! outRec%ig2 = inRec%ig2
            ! outRec%ig3 = inRec%ig3
            ! outRec%ig4 = inRec%ig4
            if (.not. outputFile%write(inRec)) then
                call app_log(APP_ERROR, 'chk_toctoc: Failed to write toctoc!')
                call pgsmabt
            end if
            call inRec%free()
        endif
        call outQuery%free()
    enddo
    call inQuery%free()
end
