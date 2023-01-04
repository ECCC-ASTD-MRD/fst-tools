   subroutine initseq
      use app
      implicit none
      
#include "champseq.cdk90"
      
!     
!     Initialisation des listes utilisees par champ_seq
!     
      integer  i,j 

      do i=1,nmaxlist1
         do j=1,nmaxlist2
            listnom(i,j) = '    '
            listniv(i,j) = -1
         enddo
         nitems1(i)=0
         nitems2(i)=0
      enddo
      ntitems = 0
      
      return
      end
      
