!> Identification du fichier..ouvrir fichier...reserver memoire
subroutine sorti(modx, norecs, jwrit)
    use app
    use rmn_fst24
    use files, only : inputFiles, inputFilePaths, outputFile, outputFilePath, outputFileMode, nInput, nRecords
    use pgsm_mod, only: iwrit, voire, message, iset, nsort, ier
    IMPLICIT NONE

    !> Type de fichier de sortie
    integer, intent(in) :: modx
    !> Nombre d'enregistrements dans le fichier
    integer, intent(in) :: norecs
    integer, intent(in) :: jwrit

    integer, external :: fnom
    integer, external :: exfin
    integer, external :: pgsmof

    !  IN      JWRIT     -1=REECRIRE SUR FICHIER OU ECRIRE A LA FIN(MS)                   SORTI(MS, 500, A)
    !                    -1=ECRIRE SUR FICHIER UN RECORD SANS DETRUIRE UN RECORD PAREIL   SORTI(STD, 500, A)
    !                    +1=REECRIRE SUR FICHIER FATAL SI RECORD PAS LA.                  SORTI(MS, 500, R)
    !                    +1=REMPLACE UN RECORD SI DEJA EXISTANT DETRUIT                   SORTI(STD, 500, R)

    integer :: i
    logical, save :: dejalue = .false.

    if (dejalue) then
        if (message) then
            call app_log(APP_WARNING, 'sorti: Second call to directive SORTI ignored')
            return
        endif
    endif
    dejalue = .true.

    outputFileMode = modx
    iset = 0

    if (outputFileMode == 1)  then
        if (norecs == 1) then
            !ier = fnom(lnkdiun(idx_ozsrt), lfn(idx_ozsrt), 'STD+SEQ+FTN', 0)
        else if (norecs == 0) then
            !ier = fnom(lnkdiun(idx_ozsrt), lfn(idx_ozsrt), 'STD+SEQ+REMOTE', 0)
        else
            !ier = fnom(lnkdiun(idx_ozsrt), lfn(idx_ozsrt), 'STD+RND+REMOTE', 0)

            if (nsort == 2) then
                iwrit =+ 1
            else if (nsort == 3) then
                iwrit = jwrit
            else
                call app_log(APP_ERROR, 'sorti: Wrong call to directive SORTI std file')
                call pgsmabt
            endif
        endif
        if (ier < 0) then
            call app_log(APP_WARNING, 'sorti: Problem openning output file')
            call pgsmabt
        endif
    else if (outputFileMode == 2) then
        call app_log(APP_ERROR, 'sorti: "MS" files are not supported in this version of PGSM')
        call pgsmabt
    else if (outputFileMode == 3 .or. outputFileMode == 4) then
        !ier = fnom(lnkdiun(idx_ozsrt), lfn(idx_ozsrt), 'SEQ+FTN+UNF', 0)

        if (nsort /= 1)  then
            call app_log(APP_ERROR, 'sorti: Wrong call to directive SORTI seq file')
            call pgsmabt
        endif
    else if (outputFileMode == 5) then
        if (jwrit == -1) then
            ! ier = pgsmof(lnkdiun(idx_ozsrt), lfn(idx_ozsrt))
        else
            ! ier = pgsmof(lnkdiun(idx_ozsrt), lfn(idx_ozsrt))
            !ier = fnom(lnkdiun(idx_ozsrt), lfn(idx_ozsrt), 'SEQ+FMT+R/W', 0)
        endif
    else
        call app_log(APP_ERROR, 'sorti: Unknown file type, wrong directive or mode different from STD, MS, SEQ')
        return
    endif

    if (outputFileMode == 1)  then
        if (.not. outputFile%open(outputFilePath, '')) then
            call app_log(APP_ERROR, 'sorti: Problem openning output file')
            call pgsmabt
        endif
    endif

    ! OUVRIR FICHIER D'ENTREE STANDARD
    nRecords = 0
    do i = 1, nInput
        if (.not. inputFiles(i)%open(inputFilePaths(i), options = 'R/O')) then
            write(app_msg, *) 'sorti: File ', inputFilePaths(i), ' is not standard random'
            call app_log(APP_ERROR, app_msg)
            call pgsmabt
        endif
        nRecords = nRecords + inputFiles(i)%get_num_records()
    enddo
    if (nInput > 1) then
        if (.not. fst24_link(inputFiles)) then
            call app_log(APP_ERROR, 'sorti: Failed to link files')
            call pgsmabt
        end if
    end if

    if (voire) then
        if (message) then
            call inputFiles(1)%print_summary()
        endif
    endif
end subroutine sorti
