!**S/P IPGSMLIC
!     
      integer function ipgsmlic(ifld,iun,ni,nj,nk,datev,etiket,ip1,ip2,ip3,typvar,nomvar,ig1,ig2,ig3,ig4,grtyp)
   implicit none
      integer iun,ni,nj,nk,ip1,ip2,ip3,datev,ig1,ig2,ig3,ig4
      integer ifld(ni,nj,nk)
      character*12 etiket
      character*4 nomvar
      character*2 typvar
      character*1 grtyp
      external fstlic
      integer fstlic


      integer ier,i
      
      ier = fstlic(ifld,iun,ni,nj,nk,datev,etiket,ip1,ip2,ip3,typvar,nomvar,ig1,ig2,ig3,ig4,grtyp)

      if (ier.lt.0) then
        ipgsmlic=ier
        return
      endif

      call prefiltre(ifld,ni,nj,nomvar,grtyp)
      ipgsmlic=ier
      return 
      end
      
