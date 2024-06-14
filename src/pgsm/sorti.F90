!> Identification du fichier..ouvrir fichier...reserver memoire
subroutine sorti(modx, norecs, jwrit)
    use app
    use files
    use pgsm_mod, only: userdate, iwrit, voire, message
    IMPLICIT NONE

    !> Type de fichier de sortie
    INTEGER, INTENT(IN) :: modx
    !> Nombre d'enregistrements dans le fichier
    INTEGER, INTENT(IN) :: norecs
    INTEGER, INTENT(IN) :: jwrit

    INTEGER, EXTERNAL :: fnom
    INTEGER, EXTERNAL :: exfin
    INTEGER, EXTERNAL :: pgsmof

    !  IN      JWRIT     -1=REECRIRE SUR FICHIER OU ECRIRE A LA FIN(MS) SORTI(MS, 500, A)
    !                    -1=ECRIRE SUR FICHIER UN RECORD SANS DETRUIRE UN
    !                       RECORD PAREIL   SORTI(STD, 500, A)
    !                    +1=REECRIRE SUR FICHIER FATAL SI RECORD PAS LA.   SORTI(MS, 500, R)
    !                    +1=REMPLACE UN RECORD SI DEJA EXISTANT DETRUIT   SORTI(STD, 500, R)

#include "enrege.cdk90"

    integer :: i
    common /relu/ dejalue
    logical dejalue
    data dejalue /.false./

    if (dejalue) then
        if (message) then
            call app_log(APP_WARNING, 'sorti: Second call to directive SORTI ignored')
            return
        endif
    endif
    dejalue = .true.

    outputFileMode = modx
    ! LE FICHIER D ENTRE NE PEUT ETRE FICHIER STANDARD SEQUENTIEL
    iset = 0

    !  RESERVER MEMOIR POUR FICHIER D ENTRE
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
            ier = pgsmof(lnkdiun(idx_ozsrt), lfn(idx_ozsrt))
        else
            ier = pgsmof(lnkdiun(idx_ozsrt), lfn(idx_ozsrt))
            !ier = fnom(lnkdiun(idx_ozsrt), lfn(idx_ozsrt), 'SEQ+FMT+R/W', 0)
        endif
    else
        call app_log(APP_ERROR, 'sorti: Unknown file type, wrong directive or mode different from STD, MS, SEQ')
        return
    endif
    if (ier < 0) then
        call app_log(APP_ERROR, 'sorti: Problem openning output file')
        call pgsmabt
    endif

    if (outputFileMode == 2) then
        call app_log(APP_ERROR, 'sorti: "MS" files are not supported anymore on CYBER-910-920')
        call pgsmabt
    endif

    if (outputFileMode == 1)  then
        ier = output%open(outputFilePath, '')

        if (ier < 0) then
            call app_log(APP_ERROR, 'sorti: Problem openning output file')
            call pgsmabt
        endif
    endif

    ! OUVRIR FICHIER D'ENTREE STANDARD
    nRecords = 0
    do i = 1, nInput
        ier = inputFiles(i)%open(lfn(i), '')
        if (ier < 0) then
            write(app_msg, *) 'sorti: File , lfn(i), is not standard random'
            call app_log(APP_ERROR, app_msg)
            call pgsmabt
        endif
        nRecords = nRecords + inputFiles(i)%get_num_records()
    enddo
    if (nInput > 1) then
        fst24_link(inputFiles)
    end if

    if (voire) then
        if (message) then
            do i = 1, nInput
                inputFiles(1)%print_summary
            enddo
        endif
    endif
end subroutine sorti
