module heuress
    implicit none

    integer, parameter :: mxheure = 40

    ! table contenant les heures via directive
    integer, save :: heures(mxheure) = -2
    ! cause de readlx nheure est sauve dans nhur
    ! nhur est desormais employe
    integer, save :: nhur = 1
    ! nombre d'arguments dans routine heure via directive (readlx) nombre d'heures dans routine heure table heures
    integer, save :: nheure = 0
end module heuress
