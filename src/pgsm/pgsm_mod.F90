module pgsm_mod
    !> Validate date
    integer, save :: date2
    integer, save :: date3
    integer, save :: userdate
    logical, save :: validate_date

    logical, save :: vvent
    logical, save :: wdvent
    logical, save :: mtdone

    !> Display the fields in the input file
    logical, save :: voire
    !> Display the fields in the output file
    logical, save :: voirs
    !> 
    logical, save :: pose
    !> Verbose mode
    logical, save :: message
    !> Print the data read from the input files
    logical, save :: printen

    !> mot de 10 caracteres max de 20 seulement les dix premiers seront utulises defaut -1
    integer, save :: etikent(3)
    integer, save :: nwetike

    !> Rewrite mode. Initialized by sorti
    integer, save  :: iwrit
    !> Pointeur temporaire
    integer, save  :: iset
    !> Number of argument of the sorti directive
    !> 1 = sequential file, 2 = standard file, 3 = ms file
    integer, save  :: nsort
    integer, save  :: nlalo

    !> type de variable initialiser a -1 dans pgsm peut-etre initialiser avec directive
    integer :: typeent
    !> Arguments de la routine lire initialiser a -1 dans pgsm peut etre initialiser avec directive
    integer :: ip3ent

    !> Marqueur d'erreur
    integer, save :: ier

    integer, save :: ip1style
    integer, save :: dateform

    real, save, dimension(:, :), pointer :: tmpif0
    real, save, dimension(:, :), pointer :: tmpif1
    real, save, dimension(:, :), pointer :: tmpif2
    real, save, dimension(:, :), pointer :: tmplat
    real, save, dimension(:, :), pointer :: tmplon
    real, save, dimension(:, :), pointer :: tmplatg
    real, save, dimension(:, :), pointer :: tmplong

contains

    subroutine init()
        date2 = -1
        date3 = -1
        userdate = -1
        validate_date = .false.

        vvent = .false.
        mtdone = .false.

        voire = .false.
        voirs = .false.
        pose = .false.
        message = .true.
        printen = .false.

        etikent(1) = -1
        etikent(2) = -1

        iwrit = 0
        nsort = 0
        nlalo = 0

        typeent = -1
        ip3ent = -1
    end subroutine init
end module pgsm_mod
