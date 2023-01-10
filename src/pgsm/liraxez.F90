      subroutine liraxez(iun, ni, nj, nk, ig1, ig2, ig3, ig4)
      use app
   implicit none

      integer iun, ni, nj, nk, ig1, ig2, ig3, ig4
      
#include "gdz.cdk90"
      
      data niz, njz, nkz 	/0, 0, 0/
      data ig1ref, ig2ref, ig3ref, ig4ref /-1, -1, -1, -1/
      data ig1z,   ig2z,   ig3z,   ig4z   /-1, -1, -1, -1/
      data grref          /'Z'/
      
      integer  fstinf, fstprm, fstluk
      external fstinf, fstprm, fstluk, pgsmabt,messags
      
      character *12 citiky,citikx
      character *4 cnmvar1,cnmvar2
      character *2 ctpvry,ctpvrx
      character *1 cgtypx,cgtypy
      
      integer irecy,irecx
      integer idatt,idett,npas,niy,njy,nky,jjp1,jjp2,jjp3
      integer ig1y,ig2y,ig3y,ig4y,nix,njx,nkx
      integer ig1x,ig2x,ig3x,ig4x
      integer numy,numx,ier,iopc
      integer cnbits,cdatyp,extra3,extra2,extra1,cubc,cdltf,clng,cswa
!     
        
      if (ni.eq.niz.and.nj.eq.njz.and.nk.eq.nkz.and.ig1.eq.ig1z.and.      ig2.eq.ig2z.and.ig3.eq.ig3z.and.ig4.eq.ig4z) then
         return
      endif

      if (associated(axex)) then
	 deallocate(axex)
      endif
      
      if (associated(axey)) then
	 deallocate(axey)
      endif

      irecx = fstinf(iun, nix, njx, nkx, -1,' ',       ig1, ig2, ig3,'  ','>>  ')
      irecy = fstinf(iun, niy, njy, nky, -1,' ',       ig1 ,ig2, ig3,'  ','^^  ')
      
      if (irecy .lt. 0 .or.irecx .lt. 0) then
         call app_log(APP_ERROR,'liraxez: Record ^^ or >> missing')
         call pgsmabt
      endif
!     
      if (nky .gt. 1) then
         call app_log(APP_ERROR,'liraxez: PGSM does not allow 3 dimensions fields (NK>1, record ^^ or >>)')
         call pgsmabt
      endif
      
      ier = fstprm(irecy,idatt,idett,npas,niy,njy,nky, cnbits,cdatyp,      jjp1,jjp2,jjp3,ctpvry,cnmvar1,citiky,cgtypy,      ig1y,ig2y,ig3y,ig4y, cswa, clng, cdltf, cubc,      extra1, extra2, extra3)
      if (ier .lt. 0) call app_log(APP_ERROR,'liraxez: FSTPRM failed')
!     
!     verifier si grille gaussienne ni doit etre pair
!     
      if (cgtypy.eq.'G'.and.mod(niy,2).ne.0)  call messags(niy)
!     
      ier = fstprm(irecx,idatt,idett,npas,nix,njx,nkx, cnbits,cdatyp,      jjp1,jjp2,jjp3,ctpvrx,cnmvar2,citikx,cgtypx,      ig1x,ig2x,ig3x,ig4x, cswa, clng, cdltf, cubc,      extra1, extra2, extra3)
      
      
      if (ier .lt. 0) call app_log(APP_ERROR,'liraxez: FSTPRM failed')
!     
!     verifier si grille gaussienne ni doit etre pair
!     
      if (cgtypx.eq.'G'.and.mod(nix,2).ne.0)  call messags(nix)
!     
!     
      if (cgtypy.ne.cgtypx .or. ig1y.ne.ig1x .or. ig2y.ne.ig2x .or.       ig3y.ne.ig3x .or. ig4y .ne.ig4x) then
         call app_log(APP_ERROR,'liraxez: Check record "^^" or ">>", GRTYP,IG1,IG2,IG3,IG4 are different')
         call pgsmabt
      endif
      
!     lecture des axes
      
      allocate(axex(nix))
      allocate(axey(niy))
      
      ier = fstluk(axex, irecx, nix, njx, nkx)
      if (ier .lt. 0) then
         call app_log(APP_ERROR,'liraxez: Problem reading record >>')
         call pgsmabt
      endif
      
      ier = fstluk(axey, irecy, niy, njy, nky)
      if (ier .lt. 0) then
         call app_log(APP_ERROR,'liraxez: Problem reading record ^^')
         call pgsmabt
      endif
      
      niz = nix
      njz = njy
      nkz = nkx
      
      ig1z = ig1
      ig2z = ig2
      ig3z = ig3
      ig4z = ig4
      
      grref = cgtypx
      ig1ref = ig1x
      ig2ref = ig2x
      ig3ref = ig3x
      ig4ref = ig4x
      
      return 
      end
      
