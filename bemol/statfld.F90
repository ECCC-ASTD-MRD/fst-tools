!**s/r statfld - calcule la moyenne, la variance, le minimum et 
!                le maximum d un champs et imprime le resultat.
!
      subroutine statfld (F_field, F_nv_S, F_no, F_from_S, &
                         minx,maxx,miny,maxy,lnk, &
                         F_i0,F_j0,F_k0,F_in,F_jn,F_kn)
!
      implicit none
! 
      integer minx,maxx,miny,maxy,lnk,F_i0,F_j0,F_k0,F_in,F_jn,F_kn,F_no
      character*(*) F_nv_S , F_from_S
      real F_field(minx:maxx,miny:maxy,lnk)
!
!author
!     M. Desgagne
!
!revision
! v2_00 - Desgagne M.       - initial MPI version (from MC2)
!
!object
!     calcule et imprime: la moyenne    (moy)
!                         la variance   (var)
!                         le minimum et le maximum du champ f   
! 
!arguments
!  Name        I/O                 Description
!----------------------------------------------------------------
! F_field       I         Field to be operated on
! F_nv_S        I         User provided string to define F_field
! F_no          I         Usually the timestep #
! F_from_S      I         Usually the name of the calling subroutine
! F_i0,F_j0     I         Global lower-left indexes of the sub-domain
!                            on which to perform statistics
! F_in,F_jn     I         Global upper-right indexes of the sub-domain
!                            on which to perform statistics
! F_k0,F_kn     I         Range of levels on which to perform statistics
!----------------------------------------------------------------
!
!implicits
!
      integer i,j,k,err,gnstatdp,itot,jtot,ktot,lun_out
      real sum,moy,var,min,max
      real*8 sumd,moyd,vard,mind,maxd,buf1
      pointer (pabuf1, buf1(minx:maxx,miny:maxy,lnk))
      integer imin,jmin,kmin,imax,jmax,kmax
      data gnstatdp /0/
!
!--------------------------------------------------------------------
      lun_out=6
!
! ** On calcule les points totales sur chaque axes
!
      itot= F_in-F_i0+1
      jtot= F_jn-F_j0+1
      ktot= F_kn-F_k0+1

      if (gnstatdp.lt.1) then
!
! ** On calcule la moyenne.
!
      sum = 0.0

      do k=F_k0,F_kn
         do j=F_j0,F_jn
            do i=F_i0,F_in
               sum = sum + F_field(i,j,k)
            end do
         end do
      end do
      
      moy = sum / float(itot*jtot*ktot)
!
! ** On calcule la variance
!
      sum = 0.0

      do k=F_k0,F_kn
         do j=F_j0,F_jn
            do i=F_i0,F_in
               sum = sum + ((F_field(i,j,k) - moy)*(F_field(i,j,k) - moy))
            end do
         end do
      end do

      var = sqrt (sum / float(itot*jtot*ktot))
!
! ** On identifie le minimum et le maximum.
!
      imin = F_i0
      jmin = F_j0
      kmin = F_k0
      imax = F_in
      jmax = F_jn
      kmax = F_kn
      max = F_field(F_i0,F_j0,F_k0)
      min = F_field(F_i0,F_j0,F_k0)
!
      do k=F_k0,F_kn
         do j=F_j0,F_jn
            do i=F_i0,F_in
               if (F_field(i,j,k) .gt. max) then
                  max  = F_field(i,j,k)
                  imax = i
                  jmax = j
                  kmax = k
               endif
               if (F_field(i,j,k) .lt. min) then
                  min  = F_field(i,j,k)
                  imin = i
                  jmin = j
                  kmin = k
               endif
            end do
         end do
      end do
!       
! ** On imprime
! 
      write(Lun_out,98) F_no,F_nv_S,moy,var,imin,jmin,kmin,min, &
                 imax,jmax,kmax,max,F_from_S 
 98   format (i4,a4,' Mean:',e14.7,' Var:',e14.7,' Min:[(',i3,',',i3,',',i3,')', &
             e14.7,']',' Max:[(',i3,',',i3,',',i3,')', e14.7,']',a6)
!
      else
!
! ** On calcule la moyenne en double precision.
!
      call hpalloc (pabuf1,(maxx-minx+1)*(maxy-miny+1)*lnk*2,err,1)
      sumd = 0.0
      do k=F_k0,F_kn
         do j=F_j0,F_jn
            do i=F_i0,F_in
               buf1(i,j,k) = F_field(i,j,k)
               sumd = sumd + buf1(i,j,k)
            end do
         end do
      end do
      moyd = sumd / float(itot*jtot*ktot)
!
! ** On calcule la variance en double precision.
!
      sumd = 0.0
      do k=F_k0,F_kn
         do j=F_j0,F_jn
            do i=F_i0,F_in
               sumd = sumd + ((buf1(i,j,k) - moyd)*(buf1(i,j,k) - moyd))
            end do
         end do
      end do
      vard = sqrt (sumd / float(itot*jtot*ktot))
!
! ** On identifie le minimum et le maximum en double precision.
!
      imin = F_i0
      jmin = F_j0
      kmin = F_k0
      imax = F_in
      jmax = F_jn
      kmax = F_kn
      maxd = buf1(F_i0,F_j0,F_k0)
      mind = buf1(F_i0,F_j0,F_k0)
!
      do k=F_k0,F_kn
         do j=F_j0,F_jn
            do i=F_i0,F_in
               if (buf1(i,j,k) .gt. maxd) then
                  maxd  = buf1(i,j,k)
                  imax  = i
                  jmax  = j
                  kmax  = k
               endif
               if (buf1(i,j,k) .lt. mind) then
                  mind  = buf1(i,j,k)
                  imin  = i
                  jmin  = j
                  kmin   = k
               endif
            end do
         end do
      end do
!       
! ** On imprime
! 
      write(Lun_out,99) F_no,F_nv_S,moyd,vard,imin,jmin,kmin,mind, imax,jmax,kmax,maxd,F_from_S 
 99   format (i4,a4,' Mean:',e22.14,' Var:',e22.14,/,' Min:[(',i3,',',i3,',',i3,')',e22.14,']',' Max:[(',i3,',',i3,',',i3,')',e22.14,']',a6)
      call hpdeallc (pabuf1,err,1)
!
      endif
!
!----------------------------------------------------------------
      return
      end 
