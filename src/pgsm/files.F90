MODULE files
    USE rmn_fst24

    IMPLICIT NONE

    INTEGER, PARAMETER :: sequentiel = 1
    INTEGER, PARAMETER :: random = 2

    SAVE
    ! integer :: lnkdiun(990)
    TYPE(fst_file), DIMENSION(990) :: inputFiles
    TYPE(fst_file) :: outputFile

    CHARACTER(len = 4096) :: outputFilePath

    INTEGER :: nInput
    INTEGER :: inputMode
    INTEGER :: nRecords = 0

    ! integer :: idx_ozsrt, isll_input
END MODULE files