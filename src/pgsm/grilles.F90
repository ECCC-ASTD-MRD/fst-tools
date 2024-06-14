module grilles
    integer, parameter :: gr_a         = 1
    integer, parameter :: gr_latlon    = 2
    integer, parameter :: gr_ps        = 3
    integer, parameter :: gr_tape4     = 4
    integer, parameter :: gr_g         = 5
    integer, parameter :: gr_b         = 6
    integer, parameter :: gr_tape1     = 7
    integer, parameter :: gr_tape2     = 8
    integer, parameter :: gr_xylis     = 9
    integer, parameter :: gr_xydir    = 10
    integer, parameter :: gr_lldir    = 11
    integer, parameter :: gr_lllist   = 12
    integer, parameter :: gr_grib     = 10
    integer, parameter :: gr_stereo   = 13
    integer, parameter :: gr_comme    = 12
    integer, parameter :: gr_stations = 14
    integer, parameter :: gr_gem      = 15
    integer, parameter :: gr_gef      = 15
    integer, parameter :: nmaxcoords  = 100000

    !> Number of East-West points in the output file
    integer, save :: li
    !> Number of North-South points in the output file
    integer, save :: lj

    !> First output grid descriptor
    integer, save :: lg1
    !> Second output grid descriptor
    integer, save :: lg2
    !> Third output grid descriptor
    integer, save :: lg3
    !> Fourth out the grid descriptor
    integer, save :: lg4

    ! -ps - lg1 position j du pole
    !       lg2 position i du pole
    !       lg3 dgrw*100
    !       lg4 d60 hetometre(0-36000)
    ! -lat, lon - lg1 dlat*100
    !            lg2 dlon*100
    !            lg3 (90-lat)*100 0=pole sud
    !            lg4 (lon*100) (0-36000) longitude coin
    ! -gaussien - lg1 = 1 hemisphere nord
    !             lg1 = 2 hemisphere sud
    !             lg1 - 0 globale

    !> pointeur dans lcm table de latitudes
    integer, save :: ixlat
    !> pointeur dans lcm table de longitudes
    integer, save :: ixlon
    !> pointeur dans lcd table de latitudes  tournees (grille E)
    integer, save :: ixlatg
    !> pointeur dans lcd table de longitudes tournees (grille E)
    integer, save :: ixlong
    !> Third parameter (output) of qlxinx -> Number of directives? Number of parameters?
    integer, save :: ngr
    integer, save :: ncoords
    integer, save :: gdin
    integer, save :: gdout
    integer, save :: masque

    !> \bug Never initialized!
    real, save :: dgrwxy

    !> 
    real, save :: coordll(nmaxcoords, 2)

    !> Output grid type
    character(len = 1), save :: cgrtyp
    character(len = 1), save :: cgtypxy

contains
    subroutine init()
        ncoords = 0
        ixlat = 0
        ixlon = 0
        ngr = 0
    end subroutine init
end module grilles
