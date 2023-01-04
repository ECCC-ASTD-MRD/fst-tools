   subroutine prefiltre(fld,ni,nj,nomvar,grtyp)
      use app
      implicit none
      
      integer ni,nj
      real fld(ni,nj)
      character*4 nomvar
      character*1 grtyp

#include "qqqfilt.cdk90"

      if (fltoggle(1)) then
        if (grtyp.eq.'Y') then
            call app_log(APP_WARNING,'prefiltre: Cannot filter fields on Y grids')
        else
            call app_log(APP_INFO,'prefiltre: Fields filtered on read')
!          call statfld4 (fld,nomvar,0,'AVANFFLT',ni,nj,1,ni,nj,1,0,0,0)
          call filtre (fld, NI, NJ, 0, fltntimes(1), fltlist(1,1), fltwgtlng(1))
!          call statfld4 (fld,nomvar,1,'APRESFLT',ni,nj,1,ni,nj,1,0,0,0)
        endif
      endif
      
      return
      end
