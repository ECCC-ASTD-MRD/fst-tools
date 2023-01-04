  program zcrop
  use app
  implicit none

#include "fst-tools_build_info.h"

  integer dateo,deet,npas,nbits,datyp,ip1,ip2,ip3
  integer ig1,ig2,ig3,ig4,swa,lng,dltf,ubc
  integer extra1,extra2,extra3,datev
  integer fstinf,fstprm,fstecr,fstouv,fnom,fstluk,fstfrm
  integer ipos, ier, i
  integer new_ni, new_nj, iun_src, iun_dst, key_ax, key_ay
  integer nix, njx, nkx, niy, njy, nky
  integer new_ip1, new_ip2, new_ip3, ip1x, ip2x, ip3x
  logical rewrite_flag
  integer mode

  integer, parameter :: non_defini = -1, copie = 0, entier = 1, reel = 2
  external fstinf,fstprm,fstecr,ccard,fstouv,fnom,fstluk

  integer imin, imax, jmin, jmax
  real xmin, xmax, ymin, ymax

  real, allocatable, dimension(:) :: ax, ay, new_ax, new_ay
  character *12 etiket
  character *4 nomvar
  character *1 grtyp, typvar

  character*128 cle(13)
  character*128 def(13),val(13)

  data cle /'src.',  'dst.', 'ip1', 'ip2', 'ip3', 'imin', 'imax', 'jmin', 'jmax',  'xmin', 'xmax', 'ymin', 'ymax' /
  data def /'void',  'void', '-99', '-99', '-99',  '-99',  '-99',  '-99',  '-99',  '-99.0','-99.0','-99.0','-99.0'/
  data val /'void',  'void','-99', '-99', '-99',  '-99',  '-99',  '-99',  '-99',  '-99.0','-99.0','-99.0','-99.0'/

  app_ptr=app_init(0,'ZCROP',VERSION,'',BUILD_TIMESTAMP)
  call app_start()

  mode = non_defini
  ipos = 0
  call ccard(cle,def,val, 13, ipos)

  iun_src = 10
  iun_dst = 11

  rewrite_flag = .false.
  ier = fnom(iun_src, val(1), 'STD+RND+R/O+OLD',0)
  ier = fnom(iun_dst, val(2), 'STD+RND+R/W', 0)

  ier = fstouv(iun_src,'RND')
  ier = fstouv(iun_dst,'RND')

  key_ax = fstinf(iun_src,nix,njx,nkx,-1, ' ',  -1,-1,-1, ' ', '>>  ')
  key_ay = fstinf(iun_src,niy,njy,nky,-1, ' ',  -1,-1,-1, ' ', '^^  ')
  ier = fstprm(key_ax,dateo,deet, npas, nix, njx, nkx, nbits, datyp, ip1x,&
              ip2x, ip3x, typvar, nomvar, etiket, grtyp, ig1, ig2, ig3,&
              ig4, swa, lng, dltf, ubc, datev, extra2, extra3)

  allocate(ax(nix))
  allocate(ay(njy))

  ier = fstluk(ax,key_ax,nix,njx,nkx)
  ier = fstluk(ay,key_ay,niy,njy,nky)

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

  new_ip1 = ip1
  new_ip2 = ip2
  new_ip3 = ip3

  nbits = 32
  datyp = 5
  if (ip1 == -99) then
    new_ip1 = ip1x
  endif

  if (ip2 == -99) then
    new_ip2 = ip2x
  endif

  if (ip3 == -99) then
    new_ip3 = ip3x + 1
  endif

! If no window is defined we copy the original axes in the output file, with possibly new updated ip[123] values
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
    ier = fstecr(ax, ax, -nbits, iun_dst, dateo, deet, npas, nix, njx, nkx, &
                 new_ip1, new_ip2, new_ip3, typvar, '>>  ', etiket, grtyp, ig1, ig2,&
                 ig3, ig4, datyp, rewrite_flag)
    ier = fstecr(ay, ay, -nbits, iun_dst, dateo, deet, npas, niy, njy, nky,&
                 new_ip1, new_ip2, new_ip3, typvar, '^^  ', etiket, grtyp, ig1, ig2,&
                 ig3, ig4, datyp, rewrite_flag)
    ier = fstfrm(iun_src)
    ier = fstfrm(iun_dst)
    stop
  else
    if (mode==entier.and.(imin < 1.or.jmin < 1.or.imax < 1.or.jmax < 1.or.imin>nix.or.imax>nix.or.jmin>njy.or.jmax>njy)) then
         write(app_msg,*) 'invalid indexes : imin, jmin, imax, jmax',imin,jmin,imax,jmax
         call app_log(APP_ERROR,app_msg)
         app_status=app_end(-1)
         stop app_status
    endif
  endif

  if (mode == reel) then
    imin = 1
    do while(ax(imin)< xmin.and.imin<=nix)
      imin = imin+1
    enddo
    imax = nix
    do while(ax(imax)> xmax.and.imax>=1)
      imax = imax - 1
    enddo
    jmin = 1
    do while(ay(jmin)< ymin.and.ymin<=njy)
      jmin = jmin+1
    enddo
    jmax = njy
    do while(ay(jmax)> ymax.and.jmax>=1)
     jmax = jmax - 1
    enddo
  endif

  if (imin > imax .or. jmin > jmax) then
    write(app_msg,*) 'invalid indexes : imin, jmin, imax, jmax',imin,jmin,imax,jmax
    call app_log(APP_ERROR,app_msg)
    app_status=app_end(-1)
    stop app_status
  endif

  new_ni = imax - imin + 1
  new_nj = jmax - jmin + 1
  allocate(new_ax(new_ni))
  allocate(new_ay(new_nj))
  new_ax = ax(imin:imax)
  new_ay = ay(jmin:jmax)


   ier = fstecr(new_ax, new_ax, -nbits, iun_dst, dateo, deet, npas, new_ni, njx, nkx, &
                new_ip1, new_ip2, new_ip3, typvar, '>>  ', etiket, grtyp, ig1, ig2,&
                ig3, ig4, datyp, rewrite_flag)
   ier = fstecr(new_ay, new_ay, -nbits, iun_dst, dateo, deet, npas, niy, new_nj, nky,&
                new_ip1, new_ip2, new_ip3, typvar, '^^  ', etiket, grtyp, ig1, ig2,&
                ig3, ig4, datyp, rewrite_flag)

   ier = fstfrm(iun_src)
   ier = fstfrm(iun_dst)

   app_status=app_end(-1)
   stop app_status
   end
