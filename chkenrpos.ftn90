      integer function chkenrpos(luin, luout, ip1, ip2, ip3)
      implicit none

#include "indptr.cdk90"

      real, dimension (:), allocatable :: ax, ay

      integer luin, luout, ip1, ip2, ip3
      integer lip1, lip2, lip3

      integer fstinf, fstluk, fstecr, fstprm
      external fstinf, fstluk, fstecr, fstprm

      integer ni1, nj1, nk1, ni2, nj2, nk2
      integer ier,ier1, ier2, yy_key, ax_key, ay_key
      logical yinyang_grid

      character*2 typvarx, typvary, grtyp, grref
      character*12 etikx, etiky
      character*4 nomx, nomy
      character*24 chaine
      integer  cdatyp
      integer dateo, deet, npas, nbits,datyp, ig1, ig2, ig3,      ig1ref, ig2ref, ig3ref, ig4ref,       swa, lng, dltf, ubc, extra1, extra2, extra3, npak

      yinyang_grid = .false.
      chkenrpos = -1

      if (mode.eq.1) then
         ax_key = fstinf(luout, ni1, nj1, nk1, -1, '            ', ip1, ip2, ip3, '  ', '>>  ')
         ay_key = fstinf(luout, ni1, nj1, nk1, -1, '            ', ip1, ip2, ip3, '  ', '^^  ')

         if (ax_key.ge.0.and.ay_key.ge.0) then
            chkenrpos = 0
         endif

         yy_key = fstinf(luout, ni1, nj1, nk1, -1, '            ', ip1, ip2, ip3, '  ', '^>  ')
         if (yy_key.ge.0) then
            chkenrpos = 1
         endif
         
         if (chkenrpos == 1 .and. ax_key >= 0) then
            if (yy_key > ax_key.or.yy_key > ay_key) then
               chkenrpos = 0
            endif
         endif
         
         if (chkenrpos >= 0) then
            return   
         endif
      endif

      if (mode.eq.5) then
         chkenrpos = 0
         return
      endif

      ax_key = fstinf(luin, ni1, nj1, nk1, -1, '            ', ip1, ip2, ip3, '  ', '>>  ')
      ay_key = fstinf(luin, ni1, nj1, nk1, -1, '            ', ip1, ip2, ip3, '  ', '^^  ')
      yy_key = fstinf(luin, ni1, nj1, nk1, -1, '            ', ip1, ip2, ip3, '  ', '^>  ')

      if (ax_key.lt.0.or.ay_key.lt.0) then
         chkenrpos = -1
      else
         chkenrpos = 0
      endif
      
      if (yy_key.ge.0) then
         chkenrpos = 1
         yinyang_grid = .true.   
      endif
      
      if (chkenrpos == 1 .and. ax_key >= 0) then
         if (yy_key > ax_key.or.yy_key > ay_key) then
            chkenrpos = 0
            yinyang_grid = .false.
         endif
      endif
      if (chkenrpos == -1) then        
         return
      endif
      
      if (yinyang_grid) then
         ier = fstprm(yy_key, dateo, deet, npas, ni1, nj1, nk1, nbits,      datyp, lip1, lip2, lip3, typvarx, nomx, etikx,      grref, ig1ref, ig2ref, ig3ref, ig4ref,       swa, lng, dltf, ubc, extra1, extra2, extra3)

         allocate(ax(ni1*nj1*nk1))

         ier = fstluk(ax, yy_key, ni1, nj1, nk1)

         call ecritur(ax,-nbits,dateo,deet,npas,ni1,nj1,nk1,      lip1,lip2,lip3,       typvarx,nomx,etikx,grref,ig1ref,ig2ref,ig3ref,ig4ref)

         deallocate(ax)
      else
         ier = fstprm(ax_key, dateo, deet, npas, ni1, nj1, nk1, nbits,      datyp, lip1, lip2, lip3, typvarx, nomx, etikx,      grref, ig1ref, ig2ref, ig3ref, ig4ref,       swa, lng, dltf, ubc, extra1, extra2, extra3)
         ier = fstprm(ay_key, dateo, deet, npas, ni2, nj2, nk2, nbits,      datyp, lip1, lip2, lip3, typvary, nomy, etiky,      grref, ig1ref, ig2ref, ig3ref, ig4ref,       swa, lng, dltf, ubc, extra1, extra2, extra3)

         allocate(ax(ni1*nj1*nk1))
         allocate(ay(ni2*nj2*nk2))

         ier = fstluk(ax, ax_key, ni1, nj1, nk1)
         ier = fstluk(ay, ay_key, ni2, nj2, nk2)

         call ecritur(ax,-nbits,dateo,deet,npas,ni1,nj1,nk1,      lip1,lip2,lip3,       typvarx,nomx,etikx,grref,ig1ref,ig2ref,ig3ref,ig4ref)

         call ecritur(ay,-nbits,dateo,deet,npas,ni2,nj2,nk2,      lip1,lip2,lip3,       typvary,nomy,etiky,grref,ig1ref,ig2ref,ig3ref,ig4ref)

         deallocate(ax)
         deallocate(ay)
      endif
      
      return
      end
