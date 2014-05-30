! **s/p fststatm
! 
   program fststatm
   implicit none
! 
! AUTHOR   Yves Chartier                      July 1993
!  
! REVISION
! 
! LANGUAGE:  fortran 77
! 
! OBJECT (fststatm)
! 
! FILES
!      tape1: TSF file
!      tape10-49: RPN standard files 
! 
! ARGUMENTS 
! 
! IMPLICIT     
! 
! MODULES
   external ccard,fstlnk      
   
   character*128 cle(40),def(40),val(40)
   data cle /40*'fst.'/
   data def /40*'scrap'/
   data val /40*'scrap'/
   
   
   integer fnom,ier,fstouv,fstopi,fstopc
   logical flag
   
   integer date,ip1,ip2,ip3
   character*12 etiket
   character*4 nomvar
   character*2 typvar
   
   integer i,ipos,nf,level
   integer ni,nj,nk
   
   integer lnkdiun(40)
   data lnkdiun /10, 11, 12, 13, 14, 15, 16, 17, 18, 19, &
         20, 21, 22, 23, 24, 25, 26, 27, 28, 29, &
         30, 31, 32, 33, 34, 35, 36, 37, 38, 39, &
         41, 41, 42, 43, 44, 45, 46, 47, 48, 49 /
      
   call ccard(cle,def,val, 40, ipos)
   
   level = 6
   flag = .false.
   ier = fstopc('MSGLVL', 'ERRORS', flag)
   
   nf = 1
 33   if (val(nf).ne.def(nf).and.nf.le.40) then
         nf = nf +1
         goto 33
      endif
      
   nf = nf -1
   do 34 i=1, nf
      ier = fnom(lnkdiun(i),val(i),'RND+OLD+R/O',0)
      if (ier.lt. 0) then
         print *, '************************************************'
         print *, ' probleme avec fichier ',val(i),' inexistant - ' 
         print *, '************************************************'
         stop
      endif
 34   continue
      
   do 35 i=1,nf
      ier = fstouv(lnkdiun(i), 'RND')
      if (ier.lt.0) then
         print *, '**********************************************'
         print *, '* le fichier #',val(i), 'n''est pas standard random'
         print *, '**********************************************'
         stop
      endif
 35   continue
      date = -1
      ip1  = -1
      ip2  = -1
      ip3  = -1
      etiket = '        '
      typvar = ' '
      nomvar = '  '

      call fstlnk(lnkdiun, nf)   
      call fststat(lnkdiun(1), ni, nj, nk,date, etiket,ip1,ip2,ip3,typvar,nomvar)

 11   format(A16)
      
      stop
      end


      
      
! **s/p fststat
! 
   subroutine fststat(iun, NI, NJ, NK, datev, etiket, ip1, ip2, ip3,typvar, nomvar)
   implicit none
   
   integer iun,ni,nj,nk,ip1,ip2,ip3,datev
   character*12 etiket
   character*2 typvar
   character*4 nomvar
      
! 
! AUTHOR   Yves Chartier                      July 1993
!  
! REVISION 001  M. Lepine - Mars 2002 - ajout du mode reduction 32 pour IEEE a 64 bits
! REVISION 002  M. Lepine - Juin 2003 - remplacement de memoirh
! 
! LANGUAGE:  fortran 77
! 
! OBJECT (fststat)
! 
! FILES
!      tape1: TSF file
!      tape10-49: RPN standard files 
! 
! ARGUMENTS 
! 
! IMPLICIT     
! 
! MODULES
      
   external fstinf,fstprm,fstluk,fstsui,fstopl
   integer ier,fstprm,fstinf,fstsui,fstluk,fstopl
   character*1 grtyp
   integer i
   
   integer key, date0, deet, npas, nbits, datyp 
   integer swa, lng, dltf, ubc
   integer ig1, ig2, ig3, ig4, extra1, extra2, extra3
   
   real rtemp
   integer itemp
   
   equivalence (rtemp,itemp)
   
   real, allocatable, dimension(:) :: buf
   
   character*12 etiket2
   key = fstinf(iun, ni, nj, nk,  datev, etiket, ip1,ip2,ip3,typvar,nomvar)
   
   ier = fstopl('REDUCTION32',.true.,.false.)
10 if (key.ge.0) then
      allocate(buf(ni*nj*nk))
      ier = fstprm(key, date0, deet, npas, ni, nj, nk, nbits, &
            datyp, ip1, ip2, ip3, typvar, nomvar, etiket2, grtyp, & 
            ig1, ig2, ig3, ig4, swa, lng, dltf, ubc, &
            extra1, extra2, extra3)
      ier = fstluk(buf,key,ni,nj,nk)
   
      if (datyp.eq.2.or.datyp.eq.4.or.datyp.eq.130.or.datyp.eq.132) then
         do i=1,ni*nj*nk
            rtemp = buf(i)
            buf(i)=real(itemp)
         end do
      endif
   
      call statfld4(nomvar,typvar,ip1,ip2,ip3,date0,etiket2,buf,ni,nj,nk)
   
      key = fstsui(iun,ni,nj,nk)
      deallocate(buf)
      goto 10
   endif
      
   return 
   end


      
      
! **s/r statfld4 - calcule la moyenne, la variance, le rminimum et 
!                 le maximum d'un champs et imprime le resultat.
! 
      subroutine statfld4(nomvar,typvar,ip1,ip2,ip3,date,etiket,f,ni,nj,nk) 
      implicit none
      character*4 nomvar
      character*2 typvar
      integer ip1,ip2,ip3,date
      character*12 etiket
!  
      integer ni,nj,nk
      real f(ni,nj,nk) 
! 
! OBJECT
!      calcule et imprime: la moyenne    (moy)
!                          la variance   (var)
!                          le minimum et le maximum
!      du champ f   
!  
!      arguments:
!          - f       - champ sur lequel on veut faire des statistiques
!          - n       - dimensions du champ f
!          - champ   - identification du champ
!          - no      - compteur 
!          - from    - identification du module d'ou on fait l'appel 
! 
! METHOD
! 
! EXTERNALS
! 
! AUTHOR   Michel Desgagne                   Nov   1992
! 
! Revision
!  001     M. Lepine, Mars  2003 -  appel a convip pour afficher les niveaux
! 
! HISTORY
! 
! *
      integer i,j,k
      real sum,moy,var,rmin,rmax
      integer imin,jmin,kmin,imax,jmax,kmax,kind,dat2,dat3
      CHARACTER*15 Level
      REAL      rlevel
! --------------------------------------------------------------------
! 
!  ** On calcule la moyenne.
! 
      sum = 0.0
      do 1 k=1,nk
         do 1 j=1,nj
            do 1 i=1,ni
         sum = sum + f(i,j,k)
 1    continue
      moy = sum / float(ni*nj*nk)
! 
!  ** On calcule la variance
! 
      sum = 0.0
      do 2 k=1,nk
         do 2 j=1,nj
            do 2 i=1,ni
               sum = sum + ((f(i,j,k) - moy)*(f(i,j,k) - moy))
 2    continue
      var = sqrt (sum / float(ni*nj*nk))
! 
!  ** On identifie le minimum et le maximum.
! 
      imin = 1
      jmin = 1
      kmin = 1
      imax = 1
      jmax = 1
      kmax = 1
      rmax = f(1,1,1)
      rmin = f(1,1,1)
! 
      do 3 k=1,nk
         do 3 j=1,nj
            do 3 i=1,ni
               if (f(i,j,k) .gt. rmax) then
                  rmax  = f(i,j,k)
                  imax = i
                  jmax = j
                  kmax = k
               endif
               if (f(i,j,k) .lt. rmin) then
                  rmin  = f(i,j,k)
                  imin = i
                  jmin = j
                  kmin = k
               endif
 3    continue
! 
      CALL convip(ip1,rlevel,kind,-1,level,.true.)
!       call newdate(date,dat2,dat3,-3);
!       print *,'Debug date=',date,dat2,dat3/100
!        
!  ** On imprime
!  
!       write(6,10) nomvar,typvar,level,ip1,ip2,ip3,date,etiket,
      write(6,10) nomvar,typvar,level,ip2,ip3,date,etiket, &
           moy,var,imin,jmin+(kmin-1)*nj,rmin, &
           imax,jmax+(kmax-1)*nj,rmax
!  10   format (' ',a4,1x,a2,1x,a15,' (',i9,') ',i4,1x,i3,1x,i9,1x,a12,1x,
 10   format (' ',a4,1x,a2,1x,a15,1x,i4,1x,i3,1x,i9,1x,a12,1x, &
          ' Mean:',e12.6,'  Var:',e12.6, &
          '  Min:[(',i3,',',i3,'):', &
          e10.4,']',' Max:[(',i3,',',i3,'):', &
          e10.4,']')
! 
! ----------------------------------------------------------------
      return
      end 
