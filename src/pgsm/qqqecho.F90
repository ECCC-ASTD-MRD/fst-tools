   subroutine qqqecho(chaine)
      use app
      implicit none

      external argdims
      integer argdims

      integer chaine(20)
      integer i,j,longueur,iun

      character*80 message
      character*16 form

      longueur = argdims(1)

      message(1:80) = ' '

      do i=1,longueur
         j = 4*(i-1)+1
         write(message(j:j+3),'(a4)') chaine(i)
      enddo
      
      iun = 2
      call pgsmecho(iun, message,longueur*4)

      return
      end
