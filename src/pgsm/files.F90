module files
    use pgsm_mod, only : opt_len
    use rmn_fst24, only : fst_file

    implicit none

    integer, parameter :: fentree = 1
    integer, parameter :: fsortie = 2

    integer, parameter :: sequentiel = 1
    integer, parameter :: random = 2

    character(len = opt_len), allocatable, save :: inputFilePaths(:)
    type(fst_file), dimension(990), save :: inputFiles
    type(fst_file), save :: outputFile

    !> Output file mode/kind
    !> 1 = Fichier standard
    !> 2 = Fichier direct (READMS)
    !> 3 = Fichier sequentiel
    !> 4 = Fichier sequentiel avec paramètres de fstecr
    !> 5 = Fichier sequentiel ascii (sortie(formatee))
    integer, save :: outputFileMode

    character(len = 4096), save :: outputFilePath

    integer, save :: nInput
    integer, save :: inputMode
    integer, save :: nRecords = 0
end module files