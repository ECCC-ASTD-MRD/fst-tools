module pairs
    implicit none

    integer, parameter :: mxpairs = 40

    integer, save :: npair
    integer, save :: npairuv

    character(len = 24), save :: paire(mxpairs)

contains

    subroutine init()
        implicit none

        paire(1) = 'VENT    UU  VV  UV      '
        paire(2) = 'UV      UU  VV  ??      '
        paire(3) = 'VENTUVS US  VS  UV      '
        paire(4) = 'UVS     US  VS  ??      '
        paire(5) = 'WDUV    UU  VV  UV  WD  '
        paire(6) = 'WDUD    UD  VD  UV  WD  '
        paire(7) = '!#@$!#@$>>  ^^  >>  ^^  '

        npair = 7
        npairuv = 4
    end subroutine init
end module pairs
