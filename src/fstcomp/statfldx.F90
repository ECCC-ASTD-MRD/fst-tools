! **s/r statfldx - calcule la moyenne, la variance, le rminimum et 
!                 le maximum d'un champs et imprime le resultat.
! 
      subroutine statfldx(record) 
      use rmn_fst24

      implicit none
! 
! OBJECT
!      calcule et imprime: la moyenne    (moy)
!                          la variance   (var)
!                          le minimum et le maximum
!      du champ f   
!  
!      arguments:
!          - record   - identification du champ
! 
! METHOD
! 
! EXTERNALS
! 
! AUTHOR   Michel Desgagne                   Nov   1992
! 
! Revision
!  001     M. Lepine, Mars  2003  -  appel a convip pour afficher les niveaux
!  002     Y. Chartier, Aout 2014 -  coordonnees i,j a 4 chiffres, detection de Nan
!  003     M. Lepine, Oct 2014    -  version adaptee (real*8), a partir de statfld4, pour utilisation avec fstcomp
! 
! HISTORY
! 
! *
      type(fst_record ) :: record  

      integer i,j,k
      real(kind=real32) :: sum,moy,var,rmin,rmax
      integer imin,jmin,kmin,imax,jmax,kmax,kind,dat2,dat3
      character(len=15) :: Level
      REAL      rlevel

      real(kind=real32), dimension(:,:,:), pointer :: data
      integer(kind=int64) :: size

      size = record%ni*record%nj*record%nk
      call record % get_data_array(data) 

!      logical isnan
! --------------------------------------------------------------------
! 
!  ** On calcule la moyenne.
! 
      sum = 0.0
      do 1 k=1,record%nk
         do 1 j=1,record%nj
            do 1 i=1,record%ni
         sum = sum + data(i,j,k)
 1    continue
      moy = sum / float(size)
! 
!  ** On calcule la variance
! 
      sum = 0.0
      do 2 k=1,record%nk
         do 2 j=1,record%nj
            do 2 i=1,record%ni
               sum = sum + ((data(i,j,k) - moy)*(data(i,j,k) - moy))
 2    continue
      var = sqrt (sum / float(size))
! 
!  ** On identifie le minimum et le maximum.
! 
      imin = 1
      jmin = 1
      kmin = 1
      imax = 1
      jmax = 1
      kmax = 1
      rmax = data(1,1,1)
      rmin = data(1,1,1)
! 
      do 3 k=1,record%nk
         do 3 j=1,record%nj
            do 3 i=1,record%ni
               if (data(i,j,k) .gt. rmax) then
                  rmax  = data(i,j,k)
                  imax = i
                  jmax = j
                  kmax = k
               endif
               if (data(i,j,k) .lt. rmin) then
                  rmin  = data(i,j,k)
                  imin = i
                  jmin = j
                  kmin = k
               endif
 3    continue
! 
      CALL convip_plus(record%ip1,rlevel,kind,-1,level,.true.)
!        
!  ** On imprime
!  
      write(6,10) record%nomvar,record%etiket,level,record%ip2,record%ip3, &
           moy,var,imin,jmin+(kmin-1)*record%nj,rmin, &
           imax,jmax+(kmax-1)*record%nj,rmax
!  10   format (' ',a4,1x,a2,1x,a15,' (',i9,') ',i9,1x,i9,1x,i9,1x,a12,1x,
 10   format ('  <',a4,'>',1x,a12,a15,1x,i8,1x,i8,1x, &
          ' Mean:',e15.8,' Stdev:',e15.8, &
          '  Min:[(',i4,',',i4,'):', &
          e11.4,']',' Max:[(',i4,',',i4,'):', &
          e11.4,']')
          
! On essaie de detecter la presence de Nan
#ifdef AIX 
      if (moy /= moy) then
         print *, '**** NaN detected'
#endif
#ifdef AMD64
      if (isnan(moy)) then
         print *, '**** NaN detected'
      endif
#endif
         do k=1,record%nk
            do j=1,record%nj
               do i=1,record%ni
#ifdef AIX
		  if (f(i,j,k) /= f(i,j,k)) then
		     write (6,20) i,j,k
		  endif
#endif
#ifdef AMD64
		  if (isnan(f(i,j,k))) then
		     write (6,20) i,j,k
		  endif
#endif
               enddo
            enddo
         enddo

 20 format(' ','**** NaN at grid point(', i4.4,',',i4.4,',',i3.3,')')
! 
! ----------------------------------------------------------------
      return
      end 
