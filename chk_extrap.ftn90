  subroutine chk_extrap(gdout, gdin, li, lj, ni, nj)
#include "impnone.cdk90"

  integer gdout, gdin, li, lj, ni, nj
  real, allocatable, dimension(:,:) ::  lat_dest, lon_dest, xdest, ydest
  
  integer ier,i,j
  integer gdll
  external gdll
  logical outside
  real r_ni, r_nj
  
  allocate(lat_dest(li,lj), lon_dest(li,lj), xdest(li,lj), ydest(li,lj))
  
!  print *, 'chk_extrap', li, lj, ni, nj
  ier = gdll(gdout, lat_dest, lon_dest)
!  print *, lat_dest
!  print *, lon_dest
  call gdxyfll(gdin, xdest, ydest, lat_dest, lon_dest, li*lj)
  
  outside = .false.
  r_ni = real(ni)
  r_nj = real(nj)
  
!  print *, xdest
!  print *, ydest
  
  do j=1,lj
     do i=1,li
	if (xdest(i,j)<0.5.or.xdest(i,j)>(r_ni+0.5)) outside = .true.
	if (ydest(i,j)<0.5.or.ydest(i,j)>(r_nj+0.5)) outside = .true.
     enddo
  enddo
  
  if (outside) then
     print *, ' (CHK_EXTRAP) LA GRILLE DE DESTINATION CONTIENT '
     print *, '              DES POINTS HORS DE LA GRILLE SOURCE'
     print *, ' (CHK_EXTRAP) TERMINAISON ABRUPTE...'
     stop
  endif
  deallocate(lat_dest, lon_dest, xdest, ydest)

  return
  end
