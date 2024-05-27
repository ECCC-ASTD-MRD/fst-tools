   program zcrop
   use app
   use rmn_fst24
   implicit none

#include "fst-tools_build_info.h"

   integer ::  ipos, i, mode, new_ni, new_nj, ip1, ip2, ip3

   integer, parameter :: non_defini = -1, copie = 0, entier = 1, reel = 2
   external ccard

   integer imin, imax, jmin, jmax
   real xmin, xmax, ymin, ymax

   logical :: success
   type(fst_file) :: src,dst
   type(fst_record)  :: ax,ay

   real, allocatable, dimension(:) :: new_ax, new_ay
   real, pointer,     dimension(:) :: old_ax, old_ay

   character(len=128) :: cle(13)
   character(len=128) :: def(13),val(13)

   data cle /'src.',  'dst.', 'ip1', 'ip2', 'ip3', 'imin', 'imax', 'jmin', 'jmax',  'xmin', 'xmax', 'ymin', 'ymax' /
   data def /'void',  'void', '-99', '-99', '-99',  '-99',  '-99',  '-99',  '-99',  '-99.0','-99.0','-99.0','-99.0'/
   data val /'void',  'void','-99', '-99', '-99',  '-99',  '-99',  '-99',  '-99',  '-99.0','-99.0','-99.0','-99.0'/

   app_ptr=app_init(0,'ZCROP',VERSION,'',BUILD_TIMESTAMP)
   call app_start()

   mode = non_defini
   ipos = 0
   call ccard(cle,def,val, 13, ipos)

   success=src%open(val(1), 'STD+RND+R/O+OLD')
   success=dst%open(val(2), 'STD+RND+R/W')

   success=src%read(ax,nomvar='>>  ')
   success=src%read(ay,nomvar='^^  ')

   read(val(03), *)  ip1
   read(val(04), *)  ip2
   read(val(05), *)  ip3
   read(val(06), *)  imin
   read(val(07), *)  imax
   read(val(08), *)  jmin
   read(val(09), *)  jmax
   read(val(10), *)  xmin
   read(val(11), *)  xmax
   read(val(12), *)  ymin
   read(val(13), *)  ymax

   ax%ip1 = ip1
   ax%ip2 = ip2
   ax%ip3 = ip3

   if (ax%ip1 == -99) then
      ax%ip1 = ay%ip1
   endif

   if (ax%ip2 == -99) then
      ax%ip2 = ay%ip2
   endif

   if (ax%ip3 == -99) then
      ax%ip3 = ay%ip3 + 1
   endif

   ay%ip1 = ax%ip1
   ay%ip2 = ax%ip2
   ay%ip3 = ax%ip3

   call ax%get_data_array(old_ax)
   call ay%get_data_array(old_ay)

!  If no window is defined we copy the original axes in the output file, with possibly new updated ip[123] values
   mode = non_defini
   i = 6
   do while (val(i) == def(i).and.i<=9)
      i = i + 1
   enddo
   if (i < 10) then
     mode = entier
   endif
   i = 10
   do while (val(i) == def(i).and.i<=13)
      i = i + 1
   enddo
   if (i==14) then
      if (mode == non_defini) then
         mode = copie
      endif
   else
      if (mode == non_defini) then
         mode = reel
      endif
   endif

   if (mode == copie) then
      success=dst%write(ax)
      success=dst%write(ay)

      success=dst%close()
      success=src%close()
      stop
   else
      if (mode==entier.and.(imin < 1.or.jmin < 1.or.imax < 1.or.jmax < 1.or.imin>ax%ni.or.imax>ax%ni.or.jmin>ax%nj.or.jmax>ax%nj)) then
         write(app_msg,*) 'invalid indexes : imin, jmin, imax, jmax',imin,jmin,imax,jmax
         call app_log(APP_ERROR,app_msg)
         app_status=app_end(-1)
         stop app_status
      endif
   endif

   if (mode == reel) then
      imin = 1
      do while(old_ax(imin)< xmin.and.imin<=ax%ni)
         imin = imin+1
      enddo
      imax = ax%ni
      do while(old_ax(imax)> xmax.and.imax>=1)
         imax = imax - 1
      enddo
      jmin = 1
      do while(old_ay(jmin)< ymin.and.ymin<=ax%nj)
         jmin = jmin+1
      enddo
      jmax = ax%nj
      do while(old_ay(jmax)> ymax.and.jmax>=1)
         jmax = jmax - 1
      enddo
   endif

   if (imin > imax .or. jmin > jmax) then
      write(app_msg,*) 'invalid indexes : imin, jmin, imax, jmax',imin,jmin,imax,jmax
      call app_log(APP_ERROR,app_msg)
      app_status=app_end(-1)
      stop app_status
   endif

   ax%ni = imax - imin + 1
   ax%nj = jmax - jmin + 1
   ay%ni = ax%ni
   ay%nj = ay%nj
   allocate(new_ax(ax%ni))
   allocate(new_ay(ax%nj))
   new_ax = old_ax(imin:imax)
   new_ay = old_ay(jmin:jmax)

   success=dst%write(ax,data=c_loc(new_ax))
   success=dst%write(ay,data=c_loc(new_ay))

   success=dst%close()
   success=src%close()

   app_status=app_end(-1)
   stop app_status
   end
