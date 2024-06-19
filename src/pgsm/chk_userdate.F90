!> \bug The result of this function is undefined if the user-provided value in the userdate is anything other than "oui" or "non"
!> Furthermore it relies on the values in date2 and date3 which are never set to anything other than -1
subroutine chk_userdate(datev)
    use app
    use pgsm_mod, only: userdate, date2, date3
    implicit none

    integer, intent(out) :: datev

#include "defin.cdk90"

    if (userdate == -1) then
        datev = -1
        return
    endif

    if (userdate == oui) then
        if (date3 == -1) then
            datev = date2
        else
            call newdate(datev, date2, date3, 3)
        endif
    else if (userdate == non) then
        date2 = -1
        date3 = -1
        datev = -1
    else
        write(app_msg, *) 'chk_userdate: Suspicious date value :', userdate
        call app_log(APP_WARNING, app_msg)
    endif
end subroutine chk_userdate