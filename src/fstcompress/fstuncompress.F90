      program fstuncompress
      use app
      use rmn_fst24

      implicit none

#include "fst-tools_build_info.h"

      integer ier,ipos

      logical :: success
      type(fst_file)    :: input, output
      type(fst_record)  :: record
      type(fst_query)   :: query

      character(len=512) :: cle(2), def(2), val(2)

      data cle /'fstin.', 'fstout.' /
      data def /'void',    'void' /
      data val /'void',    'void' /
      
      ipos = 0
      call ccard(cle,def,val, 2, ipos)
 
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

      query = input%new_query()

      do while(query%find_next(record))

         success=record%read()

         if (.not. success) then
           app_status=app_end(-1)
           call qqexit(app_status)
         endif

         if (record%datyp > 128) then
           record%datyp = record%datyp - 128
         endif
         if (record%datyp == 6) then
           record%datyp = 1
         endif

         success=output%write(record)

         if (.not. success) then
            app_status=app_end(-1)
            call qqexit(app_status)
            exit
         endif
     enddo

      success = output%flush()
      call output%print_summary()
      success = input%close()
      success = output%close()

      app_status=app_end(-1)
      call qqexit(app_status)
      end
