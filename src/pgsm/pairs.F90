module pairs
    implicit none

    integer, parameter :: mxpairs = 40

    integer, save :: npair = 7
    integer, save :: npairuv = 4

    character(len = 24), save :: paire(mxpairs) = [ &
        'VENT    UU  VV  UV      ', &
        'UV      UU  VV  ??      ', &
        'VENTUVS US  VS  UV      ', &
        'UVS     US  VS  ??      ', &
        'WDUV    UU  VV  UV  WD  ', &
        'WDUD    UD  VD  UV  WD  ', &
        '!#@$!#@$>>  ^^  >>  ^^  ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ', &
        '                        ' &
    ]
end module pairs
