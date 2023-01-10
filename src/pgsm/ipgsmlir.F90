!**S/P IPGSMLIR
!     
      integer function ipgsmlir(ifld,iun,ni,nj,nk,datev,etiket,ip1,ip2,ip3,typvar,nomvar,grtyp)
   implicit none
      integer iun,ni,nj,nk,ip1,ip2,ip3,datev,ig1,ig2,ig3,ig4
      integer ifld(ni,nj,nk)
      character*12 etiket
      character*4 nomvar
      character*2 typvar
      character*1 grtyp
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
      
