module heuress
    implicit none

    integer, parameter :: mxheure = 40

    ! table contenant les heures via directive
    integer, save :: heures(mxheure)
    ! cause de readlx nheure est sauve dans nhur
    ! nhur est desormais employe
    integer, save :: nhur
    ! nombre d'arguments dans routine heure via directive (readlx) nombre d'heures dans routine heure table heures
    integer, save :: nheure

contains

    subroutine init()
        integer :: i
        nhur = 1
        nheure = 0
        do i = 1, mxheure
            heures(i) = -2
        end do
    end subroutine
end module heuress
