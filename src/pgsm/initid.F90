      subroutine initid
   implicit none
#include "idents.cdk90"

      integer i

      qposition = 0
      qcsepar = 'T'
      qcform = 'f12.5'
      qnitems = 0
      do i=1,16
         qitems(i) = 0
      enddo

      return
      end
