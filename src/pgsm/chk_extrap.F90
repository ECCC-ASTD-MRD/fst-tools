  subroutine chk_extrap(gdout, gdin, li, lj, ni, nj)

  use app
  implicit none

  integer gdout, gdin, li, lj, ni, nj
  real, allocatable, dimension(:,:) ::  lat_dest, lon_dest, xdest, ydest
  
  integer ier,i,j
  integer gdll
  external gdll
  logical outside
  real r_ni, r_nj
  
  allocate(lat_dest(li,lj), lon_dest(li,lj), xdest(li,lj), ydest(li,lj))
  
  ier = gdll(gdout, lat_dest, lon_dest)
  call gdxyfll(gdin, xdest, ydest, lat_dest, lon_dest, li*lj)
  
  outside = .false.
  r_ni = real(ni)
  r_nj = real(nj)
  
  do j=1,lj
     do i=1,li
	if (xdest(i,j)<0.5.or.xdest(i,j)>(r_ni+0.5)) outside = .true.
	if (ydest(i,j)<0.5.or.ydest(i,j)>(r_nj+0.5)) outside = .true.
     enddo
  enddo
  
  if (outside) then
    call app_log(APP_ERROR,'chk_extrap: Destination grid contains points outside of source grid')
    call pgsmabt
  endif
  deallocate(lat_dest, lon_dest, xdest, ydest)

  return
  end
