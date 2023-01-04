!**S/P PGSMLIR
!     
   integer function pgsmlir(fld,iun,ni,nj,nk,datev,etiket,      ip1,ip2,ip3,typvar,nomvar,grtyp)
      use app
      implicit none
      
      integer iun,ni,nj,nk,ip1,ip2,ip3,datev,ig1,ig2,ig3,ig4
      real fld(ni,nj,nk)
      character*12 etiket
      character*4 nomvar
      character*2 typvar
      character*1 grtyp
      external fstlir
      integer fstlir


      integer ier,i
      
      ier = fstlir(fld,iun,ni,nj,nk,datev,etiket,ip1,ip2,ip3,typvar,nomvar)

      if (ier.lt.0) then
        pgsmlir=ier
        return
      endif

      call prefiltre(fld,ni,nj,nomvar,grtyp)
      pgsmlir=ier
      return 
      end
      
