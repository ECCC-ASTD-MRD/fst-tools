      program fstuncompress
      use app
      implicit none

#include "fst-tools_build_info.h"

      integer iun_in,iun_out,key1
      integer dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1,ip2,ip3
      integer ig1,ig2,ig3,ig4,swa,lng,dltf,ubc
      integer extra2,extra3,datev,ier,ipos,ni1,nj1,nk1
      integer fstinf,fstprm,fstinl,fstecr,fstouv,fnom,fstluk,fstsui,realordouble,fstfrm
      logical rewrite_flag
      external fstinf,fstprm,fstecr,ccard,fstinl,fstouv,fnom,fstluk,fstsui,fstfrm

      real, allocatable, dimension(:) :: buf
      character *12 etiket
      character *4 nomvar

      character *1 grtyp
      character*2 typvar


      character*512 cle(2)
      character*512 def(2),val(2)

      data cle /'fstin.', 'fstout.' /
      data def /'void',    'void' /
      data val /'void',    'void' /
      
      rewrite_flag = .false.
      ipos = 0
      call ccard(cle,def,val, 2, ipos)
 
      app_ptr=app_init(0,'fstcompress',FSTCOMPRESS_VERSION,'',BUILD_TIMESTAMP)
      call app_start()

      iun_in = 10
      iun_out = 11


      ier = fnom(iun_in,val(1),'STD+RND+R/O+OLD+REMOTE',0)
      if (ier.lt.0) then
        write(app_msg,*) 'INVALID INPUT FILE -- ERR. CODE : ', ier
        call app_log(APP_ERROR,app_msg)
        app_status=app_end(-1)
        call exit(13)

      endif
      ier = fstouv(iun_in,'RND')
      if (ier.lt.0) then
        write(app_msg,*) 'INPUT FILE NOT A VALID RPN STANDARD FILE -- ERR. CODE : ', ier
        call app_log(APP_ERROR,app_msg)
        app_status=app_end(-1)
        call exit(13)
      endif
      ier = fnom(iun_out, val(2), 'STD+RND+R/W+REMOTE', 0)
      if (ier.lt.0) then
        write(app_msg,*) 'INVALID OUTPUT FILE -- ERR. CODE : ', ier
        call app_log(APP_ERROR,app_msg)
        app_status=app_end(-1)
        call exit(13)
      endif
      ier = fstouv(iun_out,'RND')
      if (ier.lt.0) then
        write(app_msg,*) 'OUTPUT FILE NOT A VALID RPN STANDARD FILE -- ERR. CODE : ', ier
        call app_log(APP_ERROR,app_msg)
        app_status=app_end(-1)
        call exit(13)
      endif

      key1 = fstinf (iun_in,ni1,nj1,nk1,-1,' ',-1,-1,-1,' ',' ')
      do while (key1 >= 0)
        ier = fstprm(key1,DATEO,DEET, NPAS, NI, NJ, NK, NBITS, DATYP, IP1,&
                     IP2, IP3, TYPVAR, NOMVAR, ETIKET, GRTYP, IG1, IG2, IG3,&
                     IG4, SWA, LNG, DLTF, UBC, DATEV, EXTRA2, EXTRA3)
        if (nbits > 32) then
         realordouble = 2
        else
         realordouble = 1
        endif
        if (ier < 0) then
          app_status=app_end(-1)
          exit
        endif

        allocate(buf(ni*nj*nk*realordouble))
        ier = fstluk(buf,key1,ni,nj,nk)
        if (ier < 0) then
          app_status=app_end(-1)
          exit
        endif

        if (datyp > 128) then
            datyp = datyp - 128
        endif
        if (datyp == 6) then
          datyp = 1
        endif
        ier = FSTECR(buf, buf, -nbits, iun_out, dateo, deet, npas, ni, nj,&
                      nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2,&
                      ig3, ig4, datyp, rewrite_flag)
        if (ier < 0) then
          app_status=app_end(-1)
          exit
        endif
  key1 = fstsui(iun_in, ni1, nj1, nk1)
        deallocate(buf)
      enddo

!      ier = fstvoi(iun_out,'RND')
      ier = fstfrm(iun_in)
      ier = fstfrm(iun_out)

      app_status=app_end(-1)
      call qqexit(app_status)
      end
