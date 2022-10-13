subroutine bm_std_wrt(udst, champ, nistart, njstart, niend, njend, &
                      nomvar, typvar, etiket, ip1, ip2, ip3, dateo, deet, npas, datyp, nbits, &
                      grtyp, ig1, ig2, ig3, ig4)
  implicit none
  
  integer udst
  integer nistart, njstart, niend, njend, nig, njg
  real champ(nistart:niend, njstart:njend)
  real, dimension(:,:), allocatable :: rchamp
  real, dimension(:), allocatable :: rax, ray
  character*4 nomvar
  character*2 typvar
  character*12 etiket
  character*1 grtyp, grref, lgrtyp
  integer dateo, deet, datyp, usr_datyp, npas, ig1, ig2, ig3, ig4, ig1ref, ig2ref, ig3ref, ig4ref

  integer keyax, keyay, ier, compression_code
  logical rewrite_flag
  integer fstinf, fstprm, fstecr
  integer nbits, nkg, ip1, ip2, ip3
  integer i,j,ni, nj, nk
  integer rni_start, rni_end, rni, rnj_start, rnj_end, rnj
  
  
  nig = niend - nistart + 1
  njg = njend - njstart + 1
    
  nkg = 1

  if (nomvar.eq.'HY'.or.nomvar.eq.'+HY+'.or.nomvar.eq.'HT') then
    rewrite_flag = .true.
  else
    rewrite_flag = .false.
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

  ier = FSTECR(champ, champ, -nbits, udst, dateo, deet, npas, nig, njg, &
      nkg, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, &
      ig1, ig2, ig3, ig4, usr_datyp, rewrite_flag)
  return
end subroutine bm_std_wrt
