   subroutine setxtrap(val)
      use app
      implicit none
      
#include "defin.cdk90"

      integer val

      integer voisin, abort, valeur, maximum, minimum
      parameter (voisin  =   100)
      parameter (maximum =   4)
      parameter (minimum =   5)
      parameter (valeur  =   6)
      parameter (abort   =  13)

      integer n, i, j
      integer ival
      real    rval
      integer ier, ezsetval, ezsetopt

      character*8 op,v

      equivalence (ival, rval)

      ival = val

      op = 'EXTRAP'

      if (val .ne. voisin .and. val .ne. minimum .and. val .ne. maximum .and. val .ne. abort .and. val .ne. oui) then
         v = 'VALEUR'
         ier = ezsetval('extrap_value', rval)
         ier = ezsetopt('extrap_degree', 'value')
      else
         if (val .eq. 100) then
            ier = ezsetopt('extrap_degree', 'NEAREST')
            v = '0'
         else if (val .eq. 1) then
            ier =  ezsetopt('extrap_degree', 'LINEAR')
            v = '1'
         else if (val .eq. 3)  then
            ier =  ezsetopt('extrap_degree', 'CUBIC')
            v = '3'
         else if (val .eq. minimum) then
            v = 'MINIMUM'
            ier =  ezsetopt('extrap_degree', v)
         else if (val .eq. maximum) then
            v = 'MAXIMUM'
            ier =  ezsetopt('extrap_degree', v)
         else
            v = 'ABORT'
            ier =  ezsetopt('extrap_degree', v)
         endif
      endif


      return
      end

