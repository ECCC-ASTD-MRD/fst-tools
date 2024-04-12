MODULE files
    USE rmn_fst24

    IMPLICIT NONE

    INTEGER, PARAMETER :: sequentiel = 1
    INTEGER, PARAMETER :: random = 2

    SAVE
    ! integer :: lnkdiun(990)
    TYPE(fst_file), DIMENSION(990) :: inputFiles
    ! integer :: idx_ozsrt
    TYPE(fst_file) :: outputFile

    !> Output file mode/kind
    !> 1 = Fichier standard
    !> 2 = Fichier direct (READMS)
    !> 3 = Fichier sequentiel
    !> 4 = Fichier sequentiel avec paramètres de fstecr
    !> 5 = Fichier sequentiel ascii (sortie(formatee))
    integer :: outputFileMode

    CHARACTER(len = 4096) :: outputFilePath

    INTEGER :: nInput
    INTEGER :: inputMode
    INTEGER :: nRecords = 0
END MODULE files