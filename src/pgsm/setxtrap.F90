subroutine setxtrap(val)
    use app
    implicit none

    integer, intent(in) :: val

    integer, parameter :: voisin  = 100
    integer, parameter :: maximum =   4
    integer, parameter :: minimum =   5
    integer, parameter :: valeur  =   6
    integer, parameter :: abort   =  13

    integer :: ival
    real    :: rval
    integer :: ier, ezsetval, ezsetopt

    character(len = 8) :: ezopt

    equivalence (ival, rval)

    ival = val

#include "defin.cdk90"

    if (val .ne. voisin .and. val .ne. minimum .and. val .ne. maximum .and. val .ne. abort .and. val .ne. oui) then
        ier = ezsetval('extrap_value', rval)
        ier = ezsetopt('extrap_degree', 'value')
    else
        if (val .eq. 100) then
            ezopt = 'NEAREST'
        else if (val .eq. 1) then
            ezopt = 'LINEAR'
        else if (val .eq. 3)  then
            ezopt = 'CUBIC'
        else if (val .eq. minimum) then
            ezopt = 'MINIMUM'
        else if (val .eq. maximum) then
            ezopt = 'MAXIMUM'
        else
            ezopt = 'ABORT'
        endif
        ier = ezsetopt('extrap_degree', ezopt)
    endif
end
