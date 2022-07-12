subroutine fillgrid(dest,source,nistart,njstart,niend,njend,nit,njt,startx,starty)
  implicit none

  integer nit,njt,startx,starty,nistart,njstart,niend,njend
  real dest(nistart:niend,njstart:njend), source(nit,njt)
  real rmin,rmax
  integer i,j
  do j=1,njt
     do i=1,nit
        dest(i+startx-1,j+starty-1) = source(i,j)
     enddo
  enddo

  return
end subroutine fillgrid

!---------------------------------------------------------------------

subroutine fillgrid8(dest,source,nistart,njstart,niend,njend,nit,njt,startx,starty)
  implicit none

  integer nit,njt,startx,starty,nistart,njstart,niend,njend
  real*8 dest(nistart:niend,njstart:njend), source(nit,njt)
  real rmin,rmax
  integer i,j
  do j=1,njt
     do i=1,nit
        dest(i+startx-1,j+starty-1) = source(i,j)
     enddo
  enddo

  return
end subroutine fillgrid8

!---------------------------------------------------------------------

subroutine fillval(val,dest,nistart,njstart,niend,njend,nit,njt,startx,starty)
  implicit none

  integer nit,njt,startx,starty,nistart,njstart,niend,njend
  real dest(nistart:niend,njstart:njend), val

  integer i,j

  do j=1,njt
     do i=1,nit
        dest(i+startx-1,j+starty-1) = val
     enddo
  enddo

  return
end subroutine fillval

!---------------------------------------------------------------------
subroutine fillval8(val,dest,nistart,njstart,niend,njend,nit,njt,startx,starty)
  implicit none

  integer nit,njt,startx,starty,nistart,njstart,niend,njend
  real*8 dest(nistart:niend,njstart:njend), val

  integer i,j

  do j=1,njt
     do i=1,nit
        dest(i+startx-1,j+starty-1) = val
     enddo
  enddo

  return
end subroutine fillval8

!---------------------------------------------------------------------
subroutine findmin(rmin,fld,nistart,njstart,niend,njend,nit,njt,startx,starty)
  implicit none

  real rmin
  integer nit,njt,nistart,njstart,niend,njend,startx,starty
  real fld(nistart:niend,njstart:njend)

  integer i,j

  do j=1,njt
     do i=1,nit
        if (rmin > fld(nistart-1+i+startx-1,njstart-1+j+starty-1)) then
          rmin = fld(nistart-1+i+startx-1,njstart-1+j+starty-1)
        endif
     enddo
  enddo

  return
end subroutine findmin

!---------------------------------------------------------------------

subroutine findmin8(rmin,fld,nistart,njstart,niend,njend,nit,njt,startx,starty)
  implicit none

  real*8 rmin
  integer nit,njt,nistart,njstart,niend,njend,startx,starty
  real*8 fld(nistart:niend,njstart:njend)

  integer i,j

  do j=1,njt
     do i=1,nit
        if (rmin > fld(nistart-1+i+startx-1,njstart-1+j+starty-1)) then
          rmin = fld(nistart-1+i+startx-1,njstart-1+j+starty-1)
        endif
     enddo
  enddo

  return
end subroutine findmin8

!---------------------------------------------------------------------
subroutine findmax(rmax,fld,nistart,njstart,niend,njend,nit,njt,startx,starty)
  implicit none

  real rmax
  integer nit,njt,nistart,njstart,niend,njend,startx,starty
  real fld(nistart:niend,njstart:njend)

  integer i,j

  do j=1,njt
     do i=1,nit
        if (rmax < fld(nistart-1+i+startx-1,njstart-1+j+starty-1)) then
          rmax = fld(nistart-1+i+startx-1,njstart-1+j+starty-1)
        endif
     enddo
  enddo

  return
end subroutine findmax

!---------------------------------------------------------------------

subroutine findmax8(rmax,fld,nistart,njstart,niend,njend,nit,njt,startx,starty)
  implicit none

  real*8 rmax
  integer nit,njt,nistart,njstart,niend,njend,startx,starty
  real*8 fld(nistart:niend,njstart:njend)

  integer i,j

  do j=1,njt
     do i=1,nit
        if (rmax < fld(nistart-1+i+startx-1,njstart-1+j+starty-1)) then
          rmax = fld(nistart-1+i+startx-1,njstart-1+j+starty-1)
        endif
     enddo
  enddo

  return
end subroutine findmax8

!---------------------------------------------------------------------
subroutine fill_lastcol(dest,source,nistart,njstart,niend,njend,nit,njt,startx,starty)
  implicit none

  integer nit,njt,nig,njg,startx,starty,nistart,njstart,niend,njend
  real dest(nistart:niend,njstart:njend), source(nit,njt)

  integer i,j

  do j=1,njt
     dest(niend,j+starty-1) = source(1,j)
  enddo

  return
end subroutine fill_lastcol

!---------------------------------------------------------------------

subroutine fill_lastcol8(dest,source,nistart,njstart,niend,njend,nit,njt,startx,starty)
  implicit none

  integer nit,njt,nig,njg,startx,starty,nistart,njstart,niend,njend
  real*8 dest(nistart:niend,njstart:njend), source(nit,njt)

  integer i,j

  do j=1,njt
     dest(niend,j+starty-1) = source(1,j)
  enddo

  return
end subroutine fill_lastcol8

!---------------------------------------------------------------------
subroutine fill_lastcol2(fld,nistart,njstart,niend,njend)
  implicit none

  integer nistart,njstart,niend,njend
  real fld(nistart:niend,njstart:njend)

  integer i,j

  do j=njstart,njend
     fld(niend,j) = fld(1,j)
  enddo

  return
end subroutine fill_lastcol2

!---------------------------------------------------------------------
subroutine subgrid(subz,z,ni,nj,sni, snj,idebut, jdebut, ifin, jfin)
  implicit none

  integer i,j
  integer sni,snj,ni,nj
  integer idebut, jdebut, ifin, jfin
  real z(ni,nj),subz(sni,snj)
  integer i1, i2, j1, j2

  i1=idebut
  j1=jdebut
  i2=ifin
  j2=jfin

  do  j=1,snj
     do  i=1, sni
        subz(i, j)=z(i+idebut-1,j+jdebut-1)
     enddo
  enddo

  return
end subroutine subgrid

!---------------------------------------------------------------------
