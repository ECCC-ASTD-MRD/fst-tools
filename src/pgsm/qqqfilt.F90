   subroutine qqqfilt(inout,weightlst,ntimes,verbose)
      use app
      implicit none
      
      integer inout,weightlst(*),ntimes,verbose
      
#include "qqqfilt.cdk90"

      integer i,j,l,sum
      integer nb_elem,lng_liste,istart, iend

      external argdims
      integer argdims

      data(fltoggle(i),i= 1,2)  /.false.,.false./
      data(fltntimes(i), i=1,2) /0,0/
      data(fltverb(i), i=1,2)   /0,0/


      if (inout.ne.1.and.inout.ne.2) then
        call app_log(APP_ERROR,'qqqfilt: You have to use LECTURE or ECRITURE directives')
        return
      endif

      if (argdims(2).gt.9) then
        call app_log(APP_ERROR,'qqqfilt: Weight list too long, maximum is 9')
        return
      endif

      
      fltoggle(inout)=.true.
      if (argdims(2).le.1) then
         fltoggle(inout)=.false.
         return
      endif
      
      fltwgtlng(inout)=argdims(2)
      fltntimes(inout)=ntimes
      fltverb(inout)=verbose

      if (mod(fltwgtlng(inout),2).ne.1) then
        call app_log(APP_ERROR,'qqqfilt: Weight list has to be odd')
        return
      endif

      do i=1, fltwgtlng(inout)
        fltlist(i,inout)=weightlst(i)
      enddo


      return
      end
