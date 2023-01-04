   subroutine qqqform(theform)
      implicit none

      integer theform(4)
#include "idents.cdk90"
      external argdims
      integer argdims

      integer i,j,longueur

      character*80 chaine

      longueur = argdims(1)

      qcform(1:16) = ' '

      do i=1,longueur
         j = 4*(i-1)+1
         write(qcform(j:j+3),'(a4)') theform(i)
      enddo
      
      return
      end
