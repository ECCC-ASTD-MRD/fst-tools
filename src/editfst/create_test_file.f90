!/* EDITFST - Collection of useful routines in C and FORTRAN
! * Copyright (C) 1975-2014  Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
program create_test_file
!use convert_ip123
use ISO_C_BINDING
implicit none
include 'rmn/convert_ip123.inc'
integer, parameter :: TYP134=1  ! f  type, compressed F type reals
integer, parameter :: TYP198=198  ! fm type, compressed reals with potentially missing values
integer, parameter :: TYP6=5      ! F  type, reals with new quantization scheme
integer, parameter :: NBITS=-16
integer, parameter :: NID=10
integer, parameter :: NJD=10
real, dimension(NID,NJD) :: array, work
real, dimension(4000,4000) :: big_array
integer :: i, j, k, ni, nj, nk, ii
character(len=4), dimension(4) :: name1 = (/ "UU  ", "VV  ", "GZ  ", "TT  "   /)
character(len=4), dimension(4) :: name2 = (/ ">>  ", "^^  ", "!!  ", "<>  "   /)
character(len=12) :: etiket
integer :: ip1, ip2, ip3
real :: p1, p2, p3
integer :: kind1a, kind2, kind3, kind1b, kind1c, kind1d
integer :: status
integer, external :: fstlir
integer :: date_s

print *,'creating standard file for editfst test'
call fnom(10,'test.fst','STD+RND',0)
call fstouv(10,'RND')
call fstopc("PRINTOPT","NINJNK+DATESTAMPO+IPALL+NOIP23+IG1234",0)
kind1a = 2  ! millibars
kind1b = 4  ! meters above ground
kind1c = 6  ! theta
kind1d = 1  ! sigma
kind2 = 10  ! hours
kind3 = 3   ! arbitrary number
!goto 10   ! if large record test
call newdate(date_s,20140115,15300000,3)
do j=1,NID
do i=1,NJD
  array(i,j) = sqrt( (i-5.5)**2 + (j-5.5)**2 )/6.364
enddo
enddo
do ii=NJD,1,-1
  print 111,array(:,ii)
enddo
print *,'============================================'
do j=1,4
  do i=0,24,6
    do k=200,1000,400
      p1 = k
      p2 = i
      p3 = j+i
      status =  encode_ip(ip1,ip2,ip3,p1,kind1a,p2,kind2,p3,kind3)
!      array=p3
    !  print *,status,ip1,kind_to_string(kind1),ip2,kind_to_string(kind2),ip3,kind_to_string(kind3)
      write(etiket,1)'ETIKET',mod(j+i,7)
    1 format(A,I6.6)
      call fstecr(array,work,NBITS,10,date_s,0,0,NID,NJD,1,ip1,ip2,ip3,'XX',name1(j),etiket,'X',0,0,0,0,TYP134,.false.)
      call incdatr(date_s,date_s,.25_8)
      status=fstlir(work,10,ni,nj,nk,-1,etiket,ip1,ip2,ip3,'XX',name1(j))
      status =  encode_ip(ip1,ip2,ip3,p1,kind1b,p2,kind2,p3,kind3)
      call fstecr(array,work,NBITS,10,date_s,0,0,NID,NJD,1,ip1,ip2,ip3,'XX',name1(j),etiket,'X',0,0,0,0,TYP134,.false.)
      call incdatr(date_s,date_s,.25_8)
      status =  encode_ip(ip1,ip2,ip3,p1,kind1c,p2,kind2,p3,kind3)
      call fstecr(array,work,NBITS,10,date_s,0,0,NID,NJD,1,ip1,ip2,ip3,'XX',name1(j),etiket,'X',0,0,0,0,TYP134,.false.)
      call incdatr(date_s,date_s,.25_8)
      status =  encode_ip(ip1,ip2,ip3,p1*.001,kind1d,p2,kind2,p3,kind3)
      call fstecr(array,work,NBITS,10,date_s,0,0,NID,NJD,1,ip1,ip2,ip3,'XX',name1(j),etiket,'X',0,0,0,0,TYP134,.false.)
      call incdatr(date_s,date_s,.25_8)
    enddo
  enddo
  ip1=63240+(j-1)*100 ; ip2=0 ; ip3 = 0
  call fstecr(array,work,NBITS,10,date_s,0,0,NID,NJD,1,ip1,ip2,ip3,'YY',name2(j),etiket,'X',0,0,0,0,TYP134,.false.)
  call incdatr(date_s,date_s,.25_8)
enddo
call fstecr(array,work,NBITS,10,date_s,0,0,NID,NJD,1,63540,0,0,'XX','HHHH',etiket,'X',0,0,0,0,TYP6,.false.)
do j=NJD,1,-1
  print 111,array(:,j)
enddo
111 format(10F6.2,5x,10F6.2)
goto 20

10 continue   ! large record test
etiket='ABCDEFG123456'
big_array(:,:)=1.5 ; big_array(1,1)=1.0
call fstecr(big_array,work,NBITS,10,0,0,0,4000,4000,1,12345,0,0,'XX','HHHH',etiket,'X',0,0,0,0,TYP198,.false.)
big_array(:,:)=0
call fstecr(big_array,work,NBITS,10,0,0,0,4000,4000,1,12346,0,0,'XX','HHHH',etiket,'X',0,0,0,0,2,.false.)
big_array(:,:)=-1.0
i=fstlir(big_array,10,ni,nj,nk,-1,etiket,12345,-1,-1,'  ','    ')
i=fstlir(big_array,10,ni,nj,nk,-1,etiket,12346,-1,-1,'  ','    ')

20 continue
call fstfrm(10)
stop
end
