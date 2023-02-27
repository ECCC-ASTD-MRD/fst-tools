      program fstcompress
      use app
      implicit none

#include "fst-tools_build_info.h"

      integer iun_in,iun_out,key1,iun_out2
      integer ier, ipos, ni1,nj1,nk1,fstvoi,fstfrm,n
      integer dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1,ip2,ip3
      integer ig1,ig2,ig3,ig4,swa,lng,dltf,ubc
      integer extra2,extra3,datev
      integer fstinf,fstprm,fstinl,fstecr,fstouv,fnom,fstluk,fstsui
      integer compression_level, custom_nbits
      logical rewrite_flag
      external fstinf,fstprm,fstecr,ccard,fstinl,fstouv,fnom,fstluk,fstsui
      integer armn_compress_getlevel
      integer real_or_double
      external armn_compress_getlevel

      real, allocatable, dimension(:) :: buf
      integer, allocatable, dimension(:) :: iibuf
      character *12 etiket
      character *4 nomvar
      character*2 typvar
      character *1 grtyp

      character*512 cle(4)
      character*512 def(4),val(4)

      data cle /'fstin:', 'fstout:', 'level', 'nbits'/
      data def /'void',    'void', 'env', '-1' /
      data val /'void',    'void', 'env', '-1' /

      rewrite_flag = .false.
      ipos = 0
      call ccard(cle,def,val, 4, ipos)

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
!       ier = fnom(iun_in,val(1),'STD+RND+R/O+OLD+REMOTE',0)
!       ier = fnom(iun_out, val(2), 'STD+RND+R/W+REMOTE', 0)
!       ier = fstouv(iun_in,'RND')
!       ier = fstouv(iun_out,'RND')

      if (val(3)(1:3).eq.'env') then
        compression_level = armn_compress_getlevel()
        if (compression_level == -1) then
            call app_log(APP_INFO,'FST_OPTIONS environment variable undefined - Compression level set to BEST by default')
            compression_level = 1
           call armn_compress_setlevel(compression_level)
        else if (compression_level == 0) then
         call app_log(APP_INFO,'Compression level set to FAST from FST_OPTIONS environment variable')
        else
         call app_log(APP_INFO,'Compression level set to BEST from FST_OPTIONS environment variable')
        endif
      else
        call up2low(val(3),val(3))
        if (val(3)(1:4).eq.'best') then
          compression_level = 1
          call app_log(APP_INFO,'Compression level set to BEST from calling arguments')
        else if (val(3)(1:4).eq.'fast') then
          compression_level = 0
          call app_log(APP_INFO,'Compression level set to FAST from calling arguments')
        else
          write(app_msg,*) 'Wrong compression level', val(3), 'Compression level set to "best"'
          call app_log(APP_INFO,app_msg)
          compression_level = 1
        endif
        call armn_compress_setlevel(compression_level)
      endif

      read(val(4), *) custom_nbits

      key1 = fstinf (iun_in,ni1,nj1,nk1,-1,' ',-1,-1,-1,' ',' ')

      do while (key1 >= 0)
        ier = fstprm(key1,DATEO,DEET, NPAS, NI, NJ, NK, NBITS, DATYP, IP1,&
                     IP2, IP3, TYPVAR, NOMVAR, ETIKET, GRTYP, IG1, IG2, IG3,&
                     IG4, SWA, LNG, DLTF, UBC, DATEV, EXTRA2, EXTRA3)
         if (nbits > 32) then
            real_or_double = 2
         else
            real_or_double = 1
         endif

        if (ier < 0) then
         app_status=app_end(-1)
         call exit(13)
        endif
        if (datyp.eq.2.or.datyp.eq.4.or.datyp.eq.130.or.datyp.eq.132) then
         allocate(iibuf(ni*nj*nk*real_or_double))
         ier = fstluk(iibuf,key1,ni,nj,nk)
        else
         allocate(buf(ni*nj*nk*real_or_double))
         ier = fstluk(buf,key1,ni,nj,nk)
        endif
        if (ier < 0) then
         app_status=app_end(-1)
         call exit(13)
        endif


        if (nomvar == '^^' .or. nomvar == '>>' .or. nomvar == 'HY') then
          ier = FSTECR(buf, buf, -nbits, iun_out, dateo, deet, npas, ni, nj,&
                        nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2,&
                        ig3, ig4, datyp, rewrite_flag)
        else if (nbits .le. 16) then
            if (custom_nbits .ne. -1) then
              nbits = custom_nbits
            endif
            if (datyp.eq.1.or.datyp.eq.6.or.datyp.eq.5) then
               ier = FSTECR(buf, buf, -nbits, iun_out, dateo, deet, npas, ni, nj,&
                            nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2,&
                            ig3, ig4, 134, rewrite_flag)
            else if (datyp.eq.129.or.datyp.eq.134) then
               ier = FSTECR(buf, buf, -nbits, iun_out, dateo, deet, npas, ni, nj,&
                            nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2,&
                            ig3, ig4, 134, rewrite_flag)
            else if (datyp.eq.2.or.datyp.eq.4) then
               ier = FSTECR(iibuf, iibuf, -nbits, iun_out, dateo, deet, npas, ni, nj,&
                            nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2,&
                            ig3, ig4, datyp+128, rewrite_flag)
            else if (datyp.eq.130.or.datyp.eq.132) then
               ier = FSTECR(iibuf, iibuf, -nbits, iun_out, dateo, deet, npas, ni, nj,&
                            nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2,&
                            ig3, ig4, datyp, rewrite_flag)
            endif
         else if (datyp.eq.5) then
            if (custom_nbits .ne. -1) then
              nbits = custom_nbits
            else if (nbits > 32) then
               ier = FSTECR(buf, buf, -nbits, iun_out, dateo, deet, npas, ni, nj,&
                            nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2,&
                            ig3, ig4, datyp, rewrite_flag)
            else
               ier = FSTECR(buf, buf, -nbits, iun_out, dateo, deet, npas, ni, nj,&
                            nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2,&
                            ig3, ig4, 133, rewrite_flag)
            endif
         else
            ier = FSTECR(buf, buf, -nbits, iun_out, dateo, deet, npas, ni, nj,&
                         nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2,&
                         ig3, ig4, datyp, rewrite_flag)

         endif
        if (ier < 0) then
         app_status=app_end(-1)
         call exit(13)
          exit
        endif
        key1 = fstsui(iun_in,ni,nj,nk)
        if (allocated(buf))  deallocate(buf)
        if (allocated(iibuf)) deallocate(iibuf)
      end do

      ier = fstvoi(iun_out,'RND')
      ier = fstfrm(iun_in)
      ier = fstfrm(iun_out)

      app_status=app_end(-1)
      call qqexit(app_status)
      end
