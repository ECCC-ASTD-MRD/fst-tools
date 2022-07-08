!***s/r statfld4 - calcule la moyenne, la variance, le rminimum et 
!*                le maximum d'un champs et imprime le resultat.
!*
      subroutine statfld4(nomvar, typvar, ip1, ip2, ip3, date, etiket, &
                          f, ni, nj, nk, istart, iend, jstart, jend) 
      implicit none
      character*4 nomvar
      character*2 typvar
      integer ip1, ip2, ip3, date
      character*12 etiket
!* 
      integer ni, nj, nk
      integer istart, jstart, iend, jend, nis, njs
      real f(ni, nj, nk) 
!*
!*OBJECT
!*     calcule et imprime: la moyenne    (moy)
!*                         la variance   (var)
!*                         le minimum et le maximum
!*     du champ f   
!* 
!*     arguments:
!*         - f       - champ sur lequel on veut faire des statistiques
!*         - n       - dimensions du champ f
!*         - champ   - identification du champ
!*         - no      - compteur 
!*         - from    - identification du module d'ou on fait l'appel 
!*
!*METHOD
!*
!*EXTERNALS
!*
!*AUTHOR   Michel Desgagne                   Nov   1992
!*
!*Revision
!* 001     M. Lepine, Mars  2003 -  appel a convip pour afficher les niveaux
!*
!*HISTORY
!*
!**
      integer i, j, k
      real*8 sum, moy, var, sum2, stdev
      real rmin, rmax
      integer imin, jmin, kmin, imax, jmax, kmax, kind, dat2, dat3
      CHARACTER*15 Level
      REAL      rlevel
      integer   np

      character*12 etiket2


! ** On calcule la moyenne.
! ** On identifie le minimum et le maximum.
! ** On calcule la variance 
!
      sum = 0.0
      sum2 = 0.0
      var = 0.0
      stdev = 0.0
      np = 0

      imin = istart
      jmin = jstart
      kmin = 1
      imax = istart
      jmax = jstart
      kmax = 1
      rmin = f(imin, jmin, kmin)
      rmax = f(imax, jmax, kmax)


      do 1 k = 1, nk
      do 1 j = jstart, jend
      do 1 i = istart, iend
         sum  = sum + f(i, j, k)
         sum2 = sum2 + f(i, j, k)*f(i, j, k)
         np = np + 1

         if (f(i, j, k) .gt. rmax) then
            rmax = f(i, j, k)
            imax = i
            jmax = j
            kmax = k
         endif
         if (f(i, j, k) .lt. rmin) then
            rmin = f(i, j, k)
            imin = i
            jmin = j
            kmin = k
         endif
 1    continue
      moy = sum / np

      if (np .gt. 1) then
         var = ((sum2 - 2*sum*moy + moy*moy) / float(np))
         stdev = sqrt (max(0.0, var))
      endif

!
      CALL convip_plus(ip1, rlevel, kind, -1, level, .true.)
!       

! ** On imprime la grille ou region
! 
      write(6, 10) nomvar, typvar, level, ip2, ip3, date, etiket, &
                   moy, stdev, imin, jmin + (kmin - 1)*nj, rmin, &
                   imax, jmax + (kmax - 1)*nj, rmax


 10   format (' ',a4,1x,a2,1x,a15,1x,i9,1x,i9,1x,i9,1x,a12,1x, &
              ' Mean:',e13.6,' StDev:', e13.6, &
              ' Min:[(',i5,',',i5,'):', e11.4,']', &
              ' Max:[(',i5,',',i5,'):', e11.4,']')
      
      end
