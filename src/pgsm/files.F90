module files
    use rmn_fst24, only : fst_file

    implicit none

    integer, parameter :: fentree = 1
    integer, parameter :: fsortie = 2

    integer, parameter :: sequentiel = 1
    integer, parameter :: random = 2

    save
    type(fst_file), dimension(990) :: inputFiles
    type(fst_file) :: outputFile

    !> Output file mode/kind
    !> 1 = Fichier standard
    !> 2 = Fichier direct (READMS)
    !> 3 = Fichier sequentiel
    !> 4 = Fichier sequentiel avec paramètres de fstecr
    !> 5 = Fichier sequentiel ascii (sortie(formatee))
    integer :: outputFileMode

    character(len = 4096) :: outputFilePath

    integer :: nInput
    integer :: inputMode
    integer :: nRecords = 0
end module files