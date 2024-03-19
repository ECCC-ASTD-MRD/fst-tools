      program fstcompress
      use app
      use rmn_fst24

      implicit none

#include "fst-tools_build_info.h"

      integer i, ier, ipos, n
      integer compression_level, custom_nbits
      external ccard
      integer armn_compress_getlevel
      external armn_compress_getlevel

      logical :: success
      type(fst_file)    :: input, output
      type(fst_record)  :: record
      type(fst_query)   :: query

      character(len=512) cle(4), def(4), val(4)

      data cle /'fstin:', 'fstout:', 'level', 'nbits'/
      data def /'void',    'void', 'env', '-1' /
      data val /'void',    'void', 'env', '-1' /

      ipos = 0
      call ccard(cle,def,val, 4, ipos)

      ier = app_loglevel('INFO')
      app_ptr=app_init(0,'fstcompress',FSTCOMPRESS_VERSION,'',BUILD_TIMESTAMP)
      call app_start()

      success = input%open(trim(val(1)),'STD+RND+R/O+OLD+REMOTE')
      if (.not. success) then
         write(app_msg,*) 'INVALID INPUT FILE'
         call app_log(APP_ERROR,app_msg)
         app_status=app_end(-1)
         call qqexit(app_status)
      endif

      success = output%open(trim(val(2)),'STD+RND+R/W+REMOTE')
      if (.not. success) then
         write(app_msg,*) 'OUTPUT FILE NOT A VALID RPN STANDARD FILE'
         call app_log(APP_ERROR,app_msg)
         app_status=app_end(-1)
         call qqexit(app_status)
       endif

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

      query = input%new_query()

      do while(query%find_next(record))
         success=record%read()

         if (.not. success) then
            app_status=app_end(-1)
            call qqexit(app_status)
         endif
         
         if (record%nomvar == '^^' .or. record%nomvar == '>>' .or. record%nomvar == 'HY') then
         else if (record%npak .le. 16) then
            if (custom_nbits .ne. -1) then
              record%npak = -custom_nbits
            endif
            if (record%datyp.eq.1.or.record%datyp.eq.6.or.record%datyp.eq.5) then
               record%datyp=134
            else if (record%datyp.eq.129.or.record%datyp.eq.134) then
               record%datyp=134
            else if (record%datyp.eq.2.or.record%datyp.eq.4) then
               record%datyp=record%datyp+128
            endif
         else if (record%datyp.eq.5) then
            if (custom_nbits .ne. -1) then
               record%npak = -custom_nbits
            else if (record%npak > 32) then
            else
               record%datyp=133
            endif
         endif
         success=output%write(record)

         if (.not. success) then
            app_status=app_end(-1)
            call qqexit(app_status)
            exit
        endif

      end do

      success = output%flush()
      call output%print_summary()
      success = input%close()
      success = output%close()

      app_status=app_end(-1)
      call qqexit(app_status)

      end
