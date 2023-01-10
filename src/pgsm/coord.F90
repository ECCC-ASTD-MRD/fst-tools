      subroutine coord(lescoords,mode)
      use app
      implicit none
      
      real lescoords(*)
      integer mode

#include "grilles.cdk90"

      external argdims
      integer argdims
      
      integer i,localncoords


      localncoords = argdims(1)
      if (mod(localncoords,2).ne.0) then
         call app_log(APP_ERROR,'coord: Number of coordinates odd')
         call pgsmabt
      endif

      if (mode.eq.0) then
         ncoords = 0
      endif
      
      
      do i=1,localncoords,2
         if (i.lt.nmaxcoords) then
            coordll(ncoords+i/2+1,1) = lescoords(i)
            coordll(ncoords+i/2+1,2) = lescoords(i+1)
         else
            write(app_msg,*) 'coord: Too many points, Max=', nmaxcoords
            call app_log(APP_WARNING,app_msg)
         endif
      enddo
      
      ncoords = ncoords+localncoords/2

      return
      end
         
