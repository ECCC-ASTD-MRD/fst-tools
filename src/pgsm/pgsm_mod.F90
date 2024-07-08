module pgsm_mod
    implicit none

    !> Length of the reserved space for options parsed by ccard (includes file names)
    integer, parameter :: opt_len = 512

    !> Validate date
    integer, save :: date2 = -1
    integer, save :: date3 = -1
    integer, save :: userdate = -1
    logical, save :: validate_date = .false.

    logical, save :: vvent = .false.
    !> \bug This was not previously initialized
    logical, save :: wdvent = .false.
    logical, save :: mtdone = .false.

    !> Display the fields in the input file
    logical, save :: voire = .false.
    !> Display the fields in the output file
    logical, save :: voirs = .false.
    !> 
    logical, save :: pose = .false.
    !> Verbose mode
    logical, save :: message = .true.
    !> Print the data read from the input files
    logical, save :: printen = .false.

    !> mot de 10 caracteres max de 20 seulement les dix premiers seront utulises defaut -1
    integer, save :: etikent(3) = [-1, -1, -1]
    !> \bug This was not previously initialized
    integer, save :: nwetike = 0

    !> Rewrite mode. Initialized by sorti
    integer, save  :: iwrit = 0
    !> Pointeur temporaire
    integer, save  :: iset = -2
    !> Number of argument of the sorti directive
    !> 1 = sequential file, 2 = standard file, 3 = ms file
    integer, save  :: nsort = 0
    integer, save  :: nlalo = 0

    !> type de variable initialiser a -1 dans pgsm peut-etre initialiser avec directive
    integer :: typeent = -1
    !> Arguments de la routine lire initialiser a -1 dans pgsm peut etre initialiser avec directive
    integer :: ip3ent = -1

    !> Marqueur d'erreur
    integer, save :: ier = 0

    integer, save :: ip1style = 2
    integer, save :: dateform = 1

    real, save, dimension(:, :), pointer :: tmpif0
    real, save, dimension(:, :), pointer :: tmpif1
    real, save, dimension(:, :), pointer :: tmpif2
    real, save, dimension(:, :), pointer :: tmplat
    real, save, dimension(:, :), pointer :: tmplon
    real, save, dimension(:, :), pointer :: tmplatg
    real, save, dimension(:, :), pointer :: tmplong
end module pgsm_mod
