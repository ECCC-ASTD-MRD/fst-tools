module chck
    implicit none

    integer, save :: ichck
    integer, save :: nlire
    integer, save :: najou
    integer, save :: nenle
    integer, save :: nmoys
    integer, save :: necrt
    integer, save :: nmod
    integer, save :: nraci
    integer, save :: npfo
    integer, save :: multp

contains

    subroutine init()
        ichck = 0
    end subroutine init
end module chck
