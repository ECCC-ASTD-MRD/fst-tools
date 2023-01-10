!**S/P PGSMLUK
!
   integer function pgsmluk(fld,key,ni,nj,nk,nomvar,grtyp)
      use app
      implicit none
      
      integer key,ni,nj,nk
      real, dimension(ni,nj) :: fld
      character*4 nomvar
      character*1 grtyp
      external fstluk
      integer fstluk


      integer ier,i,j

      ier = fstluk(fld,key,ni,nj,nk)

!          do j=1,nj
!            do i=1,ni
!              print *, i, j, fld(i,j)
!            enddo
!         enddo

         if (ier.lt.0) then
        pgsmluk=ier
        return
      endif

      call prefiltre(fld,ni,nj,nomvar,grtyp)
      pgsmluk=ier
      return
      end

