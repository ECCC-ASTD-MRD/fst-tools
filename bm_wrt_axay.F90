subroutine bm_wrt_axay(udst, ax, ay, nig, njg, & 
                      typvar, etiket, ip1, ip2, ip3, dateo, deet, npas, nbits, &
                      grref, ig1ref, ig2ref, ig3ref, ig4ref)
  implicit none
  
  integer udst
  integer nig,njg
  real ax(nig)
  real ay(njg)
  real, dimension(:), allocatable :: rax, ray
  character*4 nomvar
  character*2 typvar
  character*12 etiket
  character*1 grtyp, grref, lgrtyp
  integer dateo, deet, npas, ig1, ig2, ig3, ig4, ig1ref, ig2ref, ig3ref, ig4ref

  integer keyax, keyay, ier
  logical rewrite_flag
  integer fstinf, fstprm, fstecr
  integer nbits, nkg, ip1, ip2, ip3, datyp
  integer i,j,ni, nj, nk
  integer rni_start, rni_end, rni, rnj_start, rnj_end, rnj
  
  
  nkg = 1
  keyax = fstinf(udst, NI, NJ, NK, -1, etiket, ip1, ip2, ip3, '  ', '>>  ')
  if (keyax.lt.0) then
    ier = FSTECR(ax, ax, -32, udst, dateo, deet, npas, nig, 1, 1, &
          ip1, ip2, ip3, typvar, '>>  ', etiket, grref, &
          ig1ref, ig2ref, ig3ref, ig4ref, 5, .true.)

    ier = FSTECR(ay, ay, -32, udst, dateo, deet, npas, 1, njg, 1, &
          ip1, ip2, ip3, typvar, '^^  ', etiket, grref, &
          ig1ref, ig2ref, ig3ref, ig4ref, 5, .true.)
  endif
  return
end subroutine bm_wrt_axay
