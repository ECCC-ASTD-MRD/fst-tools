!**S/P IPGSMLUK
!
      integer function ipgsmluk(ifld,key,ni,nj,nk,nomvar,grtyp)
   implicit none
      integer key,ni,nj,nk
      integer, dimension(ni,nj) :: ifld
      character*4 nomvar
      character*1 grtyp
      external fstluk
      integer fstluk


      integer ier,i,j

      ier = fstluk(ifld,key,ni,nj,nk)

!          do j=1,nj
!            do i=1,ni
!              print *, i, j, ifld(i,j)
!            enddo
!         enddo

         if (ier.lt.0) then
        ipgsmluk=ier
        return
      endif

!      call prefiltre(fld,ni,nj,nomvar,grtyp)
      ipgsmluk=ier
      return
      end

