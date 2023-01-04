      subroutine fillcoord(lat,lon)
   implicit none
#include "grilles.cdk90"
      real lat(*),lon(*)
      
      integer i

      do i=1,ncoords
         lat(i) = coordll(i,1)
         lon(i) = coordll(i,2)
         enddo
      return
      end
