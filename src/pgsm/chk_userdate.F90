   subroutine chk_userdate(datev)
   use app
   implicit none

#include "defin.cdk90"
#include "dates.cdk90"

   integer datev

   if (userdate == -1) then
      datev = -1
      return
   endif

   if (userdate .eq. oui) then
      if (date3.eq.-1) then
         datev=date2
      else
         call newdate(datev, date2, date3, 3)
      endif
   else if (userdate == non) then
      date2 = -1
      date3 = -1
      datev = -1
   else
      write(app_msg,*) 'chk_userdate: Suspicious date value :', userdate
      call app_log(APP_WARNING,app_msg)
   endif

   return
   end subroutine chk_userdate