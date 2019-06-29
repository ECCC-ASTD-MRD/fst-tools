subroutine bm_openfiles(nbrecs, val, def, lnkdiun, nf, usrc, udst, ucfs, ucore, ucoarse)
  implicit none

#include "bm_param_f90.h"

  integer nbrecs
  character*256 val(*), def(*)
  integer tol
  integer lnkdiun(*)
  integer nf
  integer usrc, udst, ucfs, ucore, ucoarse

  integer i, ier, niun, nrecs
  integer fnom, fstouv, fstnbr
  integer ni, nj, nk, fstopi
  external fnom, fstouv, fstnbr, fstopi
  logical flag
  
  
  nf = 1
33 if (val(nf).ne.def(nf)) then
     nf = nf +1
     goto 33
  endif
  
  nf = nf -1
  do  i=1, nf
     ier = fnom(lnkdiun(i),val(i),'STD+RND+OLD+REMOTE+R/O',0)
     if (ier.lt. 0) then
        print *, '***********************************************'
        print *, 'bemol - fichier ',val(1),' inexistant - au revoir'
        print *, '************************************************'
        call qqexit(13)
     endif
  enddo
  
  tol = 10
  flag = .false.
  ier = fstopi('TOLRNC', tol, flag)
  nrecs = 0
  nbrecs = 0
  do i=1,nf
     nrecs = fstouv(lnkdiun(i), 'RND')
     if (nrecs.lt.0) then
        print *, '**********************************************'
        print *, '* le fichier #',val(i),'n''est pas standard random'
        print *, '**********************************************'
        call qqexit(13)
     endif
     
     nrecs = fstnbr(lnkdiun(i))
     nbrecs = nbrecs + nrecs
  enddo
  
  tol = 6
  ier = fstopi('TOLRNC', tol, flag)
  call fstlnk(lnkdiun, nf)   
  usrc = lnkdiun(1)

  if (val(cle_dst) /= 'SCRAP') then
     udst = lu_dst
     ier = fnom(udst,val(cle_dst),'STD+RND+REMOTE',0)
     nrecs = fstouv(udst, 'RND')
     if (nrecs.lt.0) then
        print *, '**********************************************'
        print *, '* problem avec fstouv  du fichier DST        *'
        print *, '**********************************************'
        call qqexit(13)
     endif
  else
     udst = -1
  endif

  if (val(cle_cfs) /= 'SCRAP') then
     ucfs = lu_cfs
     ier = fnom(ucfs,val(cle_cfs),'STD+RND+REMOTE',0)
     nrecs = fstouv(ucfs, 'RND')
     if (nrecs.lt.0) then
        print *, '**********************************************'
        print *, '* problem avec fstouv  du fichier CFS        *'
        print *, '**********************************************'
        call qqexit(13)
     endif
  else
     ucfs = -1
  endif

  if (val(cle_coarse) /= 'SCRAP') then
     if (val(cle_coarse) == val(cle_dst)) then
        ucoarse = udst
     else
        ucoarse = lu_coarse
        ier = fnom(ucoarse,val(cle_coarse),'STD+RND+REMOTE',0)
        nrecs = fstouv(ucoarse, 'RND')
     endif
  else
     ucoarse = -1
  endif

  if (val(cle_core) /= 'SCRAP') then
     if (val(cle_core) == val(cle_dst)) then
        ucore = udst
     else if (val(cle_core) == val(cle_coarse)) then
        ucore = ucoarse
     else
        ucore = lu_core
        ier = fnom(ucore,val(cle_core),'STD+RND+REMOTE',0)
        nrecs = fstouv(ucore, 'RND')
     endif
  else
     ucore = -1
  endif
  
  return
  
end subroutine bm_openfiles
