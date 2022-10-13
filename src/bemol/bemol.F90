Program bemol
  implicit none

  ! Yves Chartier
  ! Mai 2002
  ! Revision:Fev 2011,VL - to add 2nd remote destination for output

  ! V2.06 - reload with librmn_015.1
  ! V2.07 - reload with librmn_015.2

  real a

  real, dimension(:,:), allocatable :: tuile, champ
  real, dimension(:), allocatable :: ax, ay

  integer ni, nj, nk, nig, njg, nkg, nix, njx, nkx, niy, njy, nky

#include "bm_param_f90.h"

  integer  ier, nrecs, nkeys
  integer, dimension(:), allocatable :: keys
  integer  exdb, exfin, fnom, fstouv, fclos, fstinf, fstfrm, fstinl, fstluk, fstprm, fstsui, fstecr, fstlir, fstopi, fstnbr
  external exdb, exfin, fnom, fstouv, fclos, fstinf, fstfrm, fstinl, fstluk, fstprm, fstsui, fstecr, fstlir, fstopi, fstnbr

  character*4 nomvar,nomvarx
  character*2 typvar, typvarx
  character*12 etiket, etiketx
  character *1 grtyp, grref

  integer i,ip1,ip2,ip3,ip1x, ip2x, ip3x

  integer extra1, extra2, extra3, deet, deetx, npas, npasx, nbits, local_nbits, bm_nbits, nbitsx, &
       datyp, datypx, dateo, dateox, datev
  integer ig1, ig2, ig3, ig4, ig1ref, ig2ref, ig3ref, ig4ref
  integer ig3core, ig3coarse
  integer swa, lng, dltf, ubc,nbrecs, nbfiles
  integer npts, nnpas, idt, ipos, res, code_compression

  integer diesisincache,diesinf, dieslir, dies_process_flds

  !
  character * 32   cle(nparams)
  character * 256 def(nparams), val(nparams)
  character * 256 nfich(120)
  character * 1   custom_grtyp
  character * 8   user_grtyp
  integer lnkdiun(120)
  integer nbfich
  integer keyax, keyay
  logical rewrite_flag
  integer usrc, udst, ucfs, ucoarse, ucore, avg
  integer fill_code, custom_ni_start, custom_ni_end, custom_ni, custom_nj_start, custom_nj_end, custom_nj
  integer make_dst, make_core, make_coarse
  logical oktowrite
  character*8 quiet_mode

  real*8 deltat

  ! CPY (CFS is used to make code search easier)

  data cle /120 * 'SRC:',  'DST:',  'CPY:', 'COARSE:', 'CORE:',  'WHEN_MISSING.', 'COMPRESS', 'NBITS.' , &
           'IG3CORE', 'IG3COARSE', 'AVG', 'QUIET', 'NI_START', 'NI_END', 'NI', 'NJ_START', 'NJ_END', 'NJ', 'GRTYP'/
  data def /120 * 'SCRAP', 'SCRAP', 'SCRAP', 'SCRAP',  'SCRAP', 'ABORT',  'YES', '-1', &
       '1',      '2'  , '-1' , 'NO', 'AUTO', 'AUTO', 'AUTO', 'AUTO', 'AUTO', 'AUTO', 'AUTO'    /
  data val /120 * 'SCRAP', 'SCRAP', 'SCRAP', 'SCRAP',  'SCRAP', 'ABORT',  'AUTO', '-1', &
       '1',      '-1'  , '-1' , 'YES','AUTO', 'AUTO', 'AUTO', 'AUTO', 'AUTO', 'AUTO', 'AUTO'    /

  data lnkdiun / 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, &
       20, 21, 22, 23, 24, 25, 26, 27, 28, 29, &
       30, 31, 32, 33, 34, 35, 36, 37, 38, 39, &
       40, 41, 42, 43, 44, 45, 46, 47, 48, 49, &
       50, 51, 52, 53, 54, 55, 56, 57, 58, 59, &
       60, 61, 62, 63, 64, 65, 66, 67, 68, 69, &
       70, 71, 72, 73, 74, 75, 76, 77, 78, 79, &
       80, 81, 82, 83, 84, 85, 86, 87, 88, 89, &
       90, 91, 92, 93, 94, 95, 96, 97, 98, 99, &
       100, 101, 102, 103, 104, 105, 106, 107, 108,109, &
       110, 111, 112, 113, 114, 115, 116, 117, 118,119, &
       120, 121, 122, 123, 124, 125, 126, 127, 128,129       /


!--init program variables ---------------------------------------------

  ier =exdb('BEMOL','v2.06','NON')

  call init_bemol_options
  call ccard(cle,def,val, nparams, ipos)

!----------------------------------------------------------------------

  if (val(CLE_MISSING).eq.'ABORT') then
    fill_code = abort
  else if (val(CLE_MISSING).eq.'FILL_MINIMUM') then
     fill_code = minimum
  else if (val(CLE_MISSING).eq.'FILL_MAXIMUM') then
     fill_code = maximum
  else if (val(CLE_MISSING).eq.'MISSING') then
     fill_code = missing
  else
     print *, 'INVALID FILL CODE'
  endif
  call bemol_set_fill_mode(fill_code)

!----------------------------------------------------------------------

  if (val(cle_compression)(1:4).eq.'AUTO') then
    code_compression = -1
  else
    if (val(cle_compression)(1:3).eq.'YES') then
      code_compression = 1
    else
      code_compression = 0
    endif
  endif
  call bemol_set_compression(code_compression)

  read(val(CLE_NBITS), *) bm_nbits
  call bemol_set_nbits(bm_nbits)
  read(val(CLE_IG3COARSE), *) ig3coarse
  call bemol_set_ig3coarse(ig3coarse)
  read(val(CLE_IG3CORE), *) ig3core
  call bemol_set_ig3core(ig3core)
  read(val(CLE_AVG), *) avg
  call bemol_set_avgfactor(avg)

!----------------------------------------------------------------------

  read(val(cle_quiet), *) quiet_mode
  if (quiet_mode.eq.'NO') then
    ier = fstopi('MSGLVL', 10, .0)
  endif

!-----------------------------------------------------------------------------------------
  custom_ni_start = 1
  custom_ni = -1
  custom_nj_start = 1
  custom_nj = -1

  if (val(cle_nistart)(1:4) == 'AUTO') then
    custom_ni_start = -1
  else
    read(val(cle_nistart), *) custom_ni_start
  endif

  if (val(cle_niend)(1:4) == 'AUTO') then
    custom_ni_end = -1
  else
    read(val(cle_niend), *) custom_ni_end
  endif

  if (val(cle_ni)(1:4) == 'AUTO') then
    custom_ni = -1
  else
    read(val(cle_ni), *) custom_ni
  endif

  if (val(cle_njstart)(1:4) == 'AUTO') then
    custom_nj_start = -1
  else
    read(val(cle_njstart), *) custom_nj_start
  endif

  if (val(cle_njend)(1:4) == 'AUTO') then
    custom_nj_end = -1
  else
    read(val(cle_njend), *) custom_nj_end
  endif

  if (val(cle_nj)(1:4) == 'AUTO') then
    custom_nj = -1
  else
    read(val(cle_nj), *) custom_nj
  endif

  if (custom_ni_end == -1 .and. custom_ni /= -1) then
    custom_ni_end = custom_ni_start + custom_ni - 1
  endif

  if (custom_nj_end == -1 .and. custom_nj /= -1) then
    custom_nj_end = custom_nj_start + custom_nj - 1
  endif

  call bemol_set_user_domain(custom_ni_start, custom_nj_start, custom_ni_end, custom_nj_end)

!  read(val(cle_nistart), *) custom_ni_start
!  read(val(cle_ni), *) custom_ni
!  read(val(cle_njstart), *) custom_nj_start
!  read(val(cle_nj), *) custom_nj

  call dies_setninj(custom_ni_start, custom_ni_end, custom_ni, custom_nj_start, custom_nj_end, custom_nj)

!----------------------------------------------------------------------

  read(val(cle_grtyp), *) user_grtyp
  if (user_grtyp(1:4).ne.'auto') then
    custom_grtyp = user_grtyp(1:1)
  endif

!----------------------------------------------------------------------

  nbfiles = 1
  call bm_openfiles(nkeys, val, def, lnkdiun, nbfiles, usrc, udst, ucfs, ucore, ucoarse)
  call bemol_set_iun_in(usrc)
  call bemol_set_iun_dst(udst)
  call bemol_set_iun_cfs(ucfs)
  call bemol_set_iun_core(ucore)
  call bemol_set_iun_coarse(ucoarse)

!   make_dst    = -1
!   make_core   = -1
!   make_coarse = -1
!
!   if (udst /= -1) make_dst = 1
!   if (ucore /= -1) make_core = 1
!   if (ucoarse /= -1) make_coarse = 1
!
!   if (make_dst == -1) then
!      if (make_core == 1) then
!         udst = ucore
!      else
!         udst = ucoarse
!      endif
!   endif

  datev = -1
  ip1 = -1
  ip2 = -1
  ip3 = -1
  typvar = '  '
  nomvar = '    '
  etiket = '            '

  print *, 'found ', nkeys, ' keys'
  allocate(keys(nkeys))
  ier = fstinl(usrc, ni, nj, nk,  datev, etiket, ip1,ip2,ip3,typvar,nomvar,keys,nkeys,nmax)

!   if (nkeys .eq. nmax) then
!      print *, '<bemol> Maximum number of records reached : 65536>'
!      print *, '<bemol> Please contact RPN-SI to increase that limit'
!      print *, '<bemol> Aborting...'
!      call exit(13)
!   endif

!--- Initialisation fichiers de sortie


!--- Boucle principale sur tous les enregistrements des fichiers d'entree

  ier =  dies_process_flds(keys, nkeys)
  if (ier.lt.0) then
     print *, 'dieslir encountered a problem - diagnostics shown above - BEMOL aborts'
     ier = exfin('BEMOL','ABORT','NON')
     call qqexit(13)
  endif
!   do i=1,nkeys
!      datev = -1
!      ip1 = -1
!      ip2 = -1
!      ip3 = -1
!      typvar = '  '
!      nomvar = '    '
!      etiket = '            '
!      oktowrite = .false.
!
!      ier = fstprm(keys(i), dateo, deet, npas, ni, nj, nk, nbits, &
!           datyp, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, &
!           ig1, ig2, ig3, ig4, swa, lng, dltf, ubc, &
!           extra1, extra2, extra3)
!      deltat = deet*npas/3600.0
!      call incdatr(datev,dateo,deltat)
!      if (nomvar /= '^^  '.and.nomvar /= '>>  ') then
!         if (grtyp.eq.'#') then
!            res = diesisincache(keys(i))
!            if (res.lt.0) then
!               keys(i)=diesinf(keys(i), usrc, nig, njg, nk, datev, etiket, ip1, ip2, ip3, ig1, ig2, typvar, nomvar)
!               allocate(champ(nig,njg))
!               allocate(ax(nig))
!               allocate(ay(njg))
!               ier = dieslir(usrc, keys(i), champ, ax, ay, grref, ig1ref, ig2ref, ig3ref, ig4ref)
!               if (ier.lt.0) then
!                 print *, 'dieslir encountered a problem - diagnostics shown above - BEMOL aborts'
!                 ier = exfin('BEMOL','ABORT','NON')
!                 call qqexit(13)
!               endif
!               oktowrite = .true.
!            endif
!         else
!            nig = ni
!            njg = nj
!            nkg = 1
!            allocate(champ(nig,njg))
!            allocate(ax(nig))
!            allocate(ay(njg))
!            if (grtyp.eq.'Z') then
!               keyax = fstinf(udst, nix, njx, nkx, -1, '            ', ig1, ig2, ig3, '  ', '>>  ')
!               if (keyax <= 0) then
!                  keyax = fstlir(ax,usrc,nix,njx,nkx,-1,'            ',ig1,ig2,ig3,'  ','>>  ')
!                  keyay = fstlir(ay,usrc,niy,njy,nky,-1,'            ',ig1,ig2,ig3,'  ','^^  ')
!                  ier = fstprm(keyax, dateox, deetx, npasx, nix, njx, nkx, nbitsx, &
!                       datypx, ip1x, ip2x, ip3x, typvarx, nomvarx, etiketx, grref, &
!                       ig1ref, ig2ref, ig3ref, ig4ref, swa, lng, dltf, ubc, &
!                       extra1, extra2, extra3)
!               endif
!            endif
!            ier = fstluk(champ, keys(i), nig, njg, nkg)
!            oktowrite = .true.
!         endif
!
!         if (oktowrite) then
!            if (bm_nbits == -1) then
!               local_nbits = nbits
!            else
!               local_nbits = bm_nbits
!            endif
!            if (grtyp /= 'Z' .and. grtyp /= '#') then
!               call bm_vanilla_wrt(udst, champ, nig, njg, &
!                    nomvar, typvar, etiket, ip1, ip2, ip3, dateo, deet, npas, local_nbits, &
!                    grtyp, ig1, ig2, ig3, ig4)
!               if (ucore /= udst .and. make_core == 1) then
!                  call bm_vanilla_wrt(ucore, champ, nig, njg, &
!                       nomvar, typvar, etiket, ip1, ip2, ip3, dateo, deet, npas, local_nbits, &
!                       grtyp, ig1, ig2, ig3, ig4)
!               endif
!               if (ucoarse /= udst .and. make_coarse == 1) then
!                  call bm_vanilla_wrt(ucoarse, champ, nig, njg, &
!                       nomvar, typvar, etiket, ip1, ip2, ip3, dateo, deet, npas, local_nbits, &
!                       grtyp, ig1, ig2, ig3, ig4)
!               endif
!               oktowrite = .false.
!            endif
!
!            if (oktowrite) then
!               if (bm_nbits == -1) then
!                  local_nbits = nbits
!               else
!                  local_nbits = bm_nbits
!               endif
!               if (make_dst == 1) then
!                  call bm_std_wrt(udst, champ, ax, ay, nig, njg, &
!                       nomvar, typvar, etiket, ip1, ip2, ip3, dateo, deet, npas, local_nbits, &
!                       grtyp, ig1, ig2, ig3, ig4, grref, ig1ref, ig2ref, ig3ref, ig4ref)
!               endif
!
!               if (make_core == 1) then
!                  ig3 = ig3core
!                  ig4 = 0
!                  call bm_core_wrt(ucore, champ, ax, ay, nig, njg, &
!                       nomvar, typvar, etiket, ip1, ip2, ip3, dateo, deet, npas, local_nbits, &
!                       grtyp, ig1, ig2, ig3, ig4, grref, ig1ref, ig2ref, ig3ref, ig4ref)
!               endif
!
!               if (make_coarse == 1) then
!                  ig3 = ig3coarse
!                  ig4 = 0
!                  call bm_coarse_wrt(ucoarse, champ, ax, ay, nig, njg, &
!                       nomvar, typvar, etiket, ip1, ip2, ip3, dateo, deet, npas, local_nbits, &
!                       grtyp, ig1, ig2, ig3, ig4, grref, ig1ref, ig2ref, ig3ref, ig4ref, avg)
!               endif
!            endif
!
!            deallocate(champ)
!            deallocate(ax)
!            deallocate(ay)
!         endif
!      endif
!   enddo

  !------------------------------------------------------------------------------------------------

  call bm_closefiles(lnkdiun, nbfiles, udst, ucfs, ucore, ucoarse)

  !------------------------------------------------------------------------------------------------
  ier = exfin('BEMOL','v1.42','OUI')
end program bemol



      character *128 function product_id_tag()
      product_id_tag='$Id$'
      return
      end
