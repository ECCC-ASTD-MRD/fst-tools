module champs
    implicit none

    integer, parameter :: mxchamps = 31

    !> table contenant des niveaux ou des heures
    integer, save :: champpr(mxchamps)
    !> valeur initialiser par readlx sauve dans nchmp
    integer, save :: nchamp
    !> nombre d'arguments pour directives champ
    integer, save :: nchmp

    integer, save :: npar

contains

    subroutine init()
        implicit none

        nchamp = 1
        nchmp = 1
    end subroutine init
end module champs
