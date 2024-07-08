!> Choisir le degre de l'interpolation
subroutine qqqintx(ordre)
    use app
    implicit none

    !> Interpolation order
    integer, intent(in) :: ordre

    integer, external :: ezsetopt
    integer :: ier

    select case (ordre)
        case (100)
            ier = ezsetopt('interp_degree', 'nearest')
        case (1)
            ier = ezsetopt('interp_degree', 'linear')
        case (3)
            ier = ezsetopt('interp_degree', 'cubic')
        case (4)
            ier = ezsetopt('interp_degree', 'average')
        case (5)
            ier = ezsetopt('interp_degree', 'sph_average')
        case default
            call app_log(APP_WARNING,'qqqintx:Wrong interpolation value, should be 0, 1 or 3, will default to 3')
            ier = ezsetopt('interp_degree','cubic')
    end select
end
