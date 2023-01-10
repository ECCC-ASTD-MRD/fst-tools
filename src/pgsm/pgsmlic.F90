!**S/P PGSMLIC
!     
   integer function pgsmlic(fld,iun,ni,nj,nk,datev,etiket,      ip1,ip2,ip3,typvar,nomvar,ig1,ig2,ig3,ig4,grtyp)
      use app
      implicit none

      integer iun,ni,nj,nk,ip1,ip2,ip3,datev,ig1,ig2,ig3,ig4
      real fld(ni,nj,nk)
      character*12 etiket
      character*4 nomvar
      character*2 typvar
      character*1 grtyp
      external fstlic
      integer fstlic
      integer ier,i
      
      ier = fstlic(fld,iun,ni,nj,nk,datev,etiket,ip1,ip2,ip3,typvar,nomvar,ig1,ig2,ig3,ig4,grtyp)

      if (ier.lt.0) then
        pgsmlic=ier
        return
      endif

      call prefiltre(fld,ni,nj,nomvar,grtyp)
      pgsmlic=ier
      return 
      end
      
