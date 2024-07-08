module champs
    implicit none

    integer, parameter :: mxchamps = 31

    !> table contenant des niveaux ou des heures
    integer, save :: champpr(mxchamps)
    !> valeur initialiser par readlx sauve dans nchmp
    integer, save :: nchamp = 1
    !> nombre d'arguments pour directives champ
    integer, save :: nchmp = 1

    integer, save :: npar
end module champs
