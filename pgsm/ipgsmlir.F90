!**S/P IPGSMLIR
!
      integer function ipgsmlir(ifld,iun,ni,nj,nk,datev,etiket,ip1,ip2,ip3,typvar,nomvar,grtyp)
#include "impnone.cdk90"
      integer iun,ni,nj,nk,ip1,ip2,ip3,datev,ig1,ig2,ig3,ig4
      integer ifld(ni,nj,nk)
      character(len=12) etiket
      character(len=4) nomvar
      character(len=2) typvar
      character(len=1) grtyp
      external fstlir
      integer fstlir


      integer ier,i

      ier = fstlir(ifld,iun,ni,nj,nk,datev,etiket,ip1,ip2,ip3,typvar,nomvar)

      if (ier.lt.0) then
        ipgsmlir=ier
        return
      endif

      call prefiltre(ifld,ni,nj,nomvar,grtyp)
      ipgsmlir=ier
      return
      end

