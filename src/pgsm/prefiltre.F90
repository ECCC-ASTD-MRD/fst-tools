subroutine prefiltre(fld, ni, nj, grtyp)
    use app
    implicit none

    integer, intent(in) :: ni, nj
    real :: fld(ni, nj)
    character(len = 1), intent(in) :: grtyp

#include "qqqfilt.cdk90"

    if (fltoggle(1)) then
        if (grtyp == 'Y') then
            call app_log(APP_WARNING, 'prefiltre: Cannot filter fields on Y grids')
        else
            call app_log(APP_INFO, 'prefiltre: Fields filtered on read')
            call filtre (fld, NI, NJ, fltntimes(1), fltlist(1, 1), fltwgtlng(1))
        endif
    endif
end
