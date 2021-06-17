subroutine bm_core_wrt(udst, champ, ax, ay, nig, njg, & 
                      nomvar, typvar, etiket, ip1, ip2, ip3, dateo, deet, npas, datyp, nbits, &
                      grtyp, ig1, ig2, ig3, ig4, grref, ig1ref, ig2ref, ig3ref, ig4ref)
  implicit none
  
  integer udst
  integer nig,njg
  real ax(nig)
  real ay(nig)
  real champ(nig, njg)
  character*4 nomvar
  character*2 typvar
  character*12 etiket
  character*1 grtyp, grref, lgrtyp
  integer dateo, datyp, deet, npas, ig1, ig2, ig3, ig4, ig1ref, ig2ref, ig3ref, ig4ref

  integer keyax, keyay, ier
  logical rewrite_flag
  integer fstinf, fstprm, fstecr
  integer nbits, nkg, ip1, ip2, ip3
  integer i, j, nix, njx, nkx, compression_code, usr_datyp

  real, dimension(:,:), allocatable :: zcore
  real, dimension(:), allocatable:: axcore, aycore

  integer xc1, xc2, yc1, yc2, nicore, njcore
  integer findlowcoreindex, findhighcoreindex
  external findlowcoreindex, findhighcoreindex;
  
  xc1 = findlowcoreindex(ax, nig)
  xc2 = findhighcoreindex(ax, nig)
  yc1 = findlowcoreindex(ay, njg)
  yc2 = findhighcoreindex(ay, njg)

  nicore = xc2 - xc1 + 1
  njcore = yc2 - yc1 + 1

  allocate(zcore(nicore,njcore))
  call subgrid(zcore,champ,nig,njg,nicore,njcore,xc1, yc1, xc2, yc2)

  keyax = fstinf(udst, nix, njx, nkx, -1, etiket, ig1, ig2, ig3, '  ', '>>  ')
  if (keyax.lt.0) then

     allocate(axcore(nicore))
     allocate(aycore(njcore))
     do i=1,nicore
        axcore(i) = ax(i+xc1-1)
     enddo

     do j=1,njcore
        aycore(j) = ay(j+yc1-1)
     enddo

     
     ier = FSTECR(axcore, axcore, -32, udst, dateo, deet, npas, nicore, 1, 1, &
          ig1, ig2, ig3, 'X ', '>>  ', etiket, grref, &
          ig1ref, ig2ref, ig3ref, ig4ref, 5, .true.)
     
     ier = FSTECR(aycore, aycore, -32, udst, dateo, deet, npas, 1, njcore, 1, &
          ig1, ig2, ig3, 'X ', '^^  ', etiket, grref, &
          ig1ref, ig2ref, ig3ref, ig4ref, 5, .true.)
     
     deallocate(axcore)
     deallocate(aycore)

  endif
  
  call bemol_get_compression_code(compression_code)
  if (compression_code == -1) then
     usr_datyp = datyp
  else
    if (compression_code == 1) then
      if (datyp < 10) then
        usr_datyp = datyp+128
      else
        usr_datyp = datyp
      endif
     if (usr_datyp == 129) then
      usr_datyp = 134
     endif
    else
      if (datyp > 128) then
        usr_datyp = datyp-128
        if (usr_datyp == 6) then
          usr_datyp = 1
        endif
      else
        usr_datyp = datyp
      endif
    endif
  endif


  lgrtyp = grtyp
  if (lgrtyp.eq.'#') then
     lgrtyp = 'Z'
  endif
     

  ier = FSTECR(zcore, zcore, -nbits, udst, dateo, deet, npas, nicore, njcore, &
       1, ip1, ip2, ip3, typvar, nomvar, etiket, lgrtyp, &
       ig1, ig2, ig3, ig4, usr_datyp, rewrite_flag)
  return
end subroutine bm_core_wrt
