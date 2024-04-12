!> IDENTIFICATION DU FICHIER..OUVRIR FICHIER...RESERVER MEMOIRE
subroutine sorti(modx, norecs, jwrit)
    use app
    use files
    IMPLICIT NONE

    !> Type de fichier de sortie
    INTEGER, INTENT(IN) :: modx
    !> Nombre d'enregistrements dans le fichier
    INTEGER, INTENT(IN) :: norecs
    INTEGER, INTENT(IN) :: jwrit

    INTEGER, EXTERNAL :: fnom
    INTEGER, EXTERNAL :: fstnbr
    INTEGER, EXTERNAL :: fsteof
    INTEGER, EXTERNAL :: fstouv
    INTEGER, EXTERNAL :: fstvoi
    INTEGER, EXTERNAL :: fstapp
    INTEGER, EXTERNAL :: exfin
    INTEGER, EXTERNAL :: pgsmof

    !  IN      JWRIT     -1=REECRIRE SUR FICHIER OU ECRIRE A LA FIN(MS) SORTI(MS, 500, A)
    !                    -1=ECRIRE SUR FICHIER UN RECORD SANS DETRUIRE UN
    !                       RECORD PAREIL   SORTI(STD, 500, A)
    !                    +1=REECRIRE SUR FICHIER FATAL SI RECORD PAS LA.   SORTI(MS, 500, R)
    !                    +1=REMPLACE UN RECORD SI DEJA EXISTANT DETRUIT   SORTI(STD, 500, R)
    !
    !MESSAGES
    !         DEUXIEME APPEL A LA DIRECTIVE SORTIE APPEL IGNORE
    !         MAUVAIS APPEL A DIRECTIVE SORTIE FICHIER STD
    !         MAUVAISE DIRECTIVE (SORTIE) FICHIER MS
    !         DIRECTIVE ENREG=0, INITIALISER A 1
    !         MAUVAIS APPEL A SORTIE FICHIER SEQ
    !         TYPE DE FICHIER INCONNU
    !         MAUVAISE DIRECTIVE MODE DIFFERENT DE (STD, MS, SEQ)

#include "llccmm.cdk90"
#include "accum.cdk90"
#include "charac.cdk90"
#include "dates.cdk90"
#include "indptr.cdk90"
#include "enrege.cdk90"
#include "voir.cdk90"

    integer :: i
    common /relu/ dejalue
    logical dejalue
    data dejalue /.false./

    !   DATE DE LA RUN OPERATIONNELLE UTILISEE PAR L'USAGER DIRECTIVE OPDAT=OUI
    if (lfn(idx_date) /= 'NON') then
        if (seldat) userdate = jdate
        if (seldat) then
            write(app_msg, *)'sorti: Origin date = valid date = ', userdate
            call app_log(APP_INFO, app_msg)
        endif
    endif

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
        ! if (norecs == 1)  then
        !    ier = fstouv(lnkdiun(idx_ozsrt), 'SEQ+FTN')
        ! else if (norecs == 0) then
        !     ier = fstouv(lnkdiun(idx_ozsrt), 'SEQ')
        !     if (jwrit == -1) then
        !         ier = fstapp(lnkdiun(idx_ozsrt), ' ')
        !         ier = fsteof(lnkdiun(idx_ozsrt))
        !     endif
        ! else
        !     ier = fstouv(lnkdiun(idx_ozsrt), 'RND')
        ! endif

        if (ier < 0) then
            call app_log(APP_ERROR, 'sorti: Problem openning output file')
            call pgsmabt
        endif
    endif

    ! OUVRIR FICHIER D'ENTREE STANDARD
    ! if (inputMode == SEQUENTIEL) then
    !     ier = fstouv(lnkdiun(1), 'SEQ')
    ! else
        nRecords = 0
        do i = 1, nInput
            ! ier = fstouv(lnkdiun(i), 'RND+R/O+OLD')
            ! nRecords = nRecords + fstnbr(lnkdiun(i))

            ier = inputFiles(i)%open(lfn(i), '')
            if (ier < 0) then
                write(app_msg, *) 'sorti: File , lfn(i), is not standard random'
                call app_log(APP_ERROR, app_msg)
                call pgsmabt
            endif
            nRecords = nRecords + inputFiles(i)%get_num_records()
        enddo
        fst24_link(inputFiles)

    !    call fstlnk(lnkdiun, nInput)
    ! endif

      if (voire) then
         if (message) then
            do i = 1, nInput
               ier = fstvoi(lnkdiun(i), 'RND')
            enddo
         endif
      endif
end subroutine sorti
