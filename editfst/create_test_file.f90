program create_test_file
use convert_ip123
implicit none
integer, dimension(10,10) :: array, work
integer :: i, j, k
character(len=4), dimension(4) :: name1 = (/ "UU  ", "VV  ", "GZ  ", "TT  "   /)
character(len=4), dimension(4) :: name2 = (/ ">>  ", "^^  ", "!!  ", "<>  "   /)
character(len=12) :: etiket
integer :: ip1, ip2, ip3
real :: p1, p2, p3
integer :: kind1a, kind2, kind3, kind1b, kind1c, kind1d
integer :: status

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

do j=1,4
  do i=0,24,6
    do k=200,1000,400
      p1 = k
      p2 = i
      p3 = j+i
      status =  encode_ip(ip1,ip2,ip3,p1,kind1a,p2,kind2,p3,kind3)
      array=p3
    !  print *,status,ip1,kind_to_string(kind1),ip2,kind_to_string(kind2),ip3,kind_to_string(kind3)
      write(etiket,1)'ETIKET',mod(j+i,7)
    1 format(A,I6.6)
      call fstecr(array,work,-16,10,0,0,0,10,10,1,ip1,ip2,ip3,'XX',name1(j),etiket,'X',0,0,0,0,134,.false.)
      status =  encode_ip(ip1,ip2,ip3,p1,kind1b,p2,kind2,p3,kind3)
      call fstecr(array,work,-16,10,0,0,0,10,10,1,ip1,ip2,ip3,'XX',name1(j),etiket,'X',0,0,0,0,134,.false.)
      status =  encode_ip(ip1,ip2,ip3,p1,kind1c,p2,kind2,p3,kind3)
      call fstecr(array,work,-16,10,0,0,0,10,10,1,ip1,ip2,ip3,'XX',name1(j),etiket,'X',0,0,0,0,134,.false.)
      status =  encode_ip(ip1,ip2,ip3,p1*.001,kind1d,p2,kind2,p3,kind3)
      call fstecr(array,work,-16,10,0,0,0,10,10,1,ip1,ip2,ip3,'XX',name1(j),etiket,'X',0,0,0,0,134,.false.)
    enddo
  enddo
  ip1=63240+(j-1)*100 ; ip2=0 ; ip3 = 0
  call fstecr(array,work,-16,10,0,0,0,10,10,1,ip1,ip2,ip3,'YY',name2(j),etiket,'X',0,0,0,0,134,.false.)
enddo
call fstfrm(10)
stop
end
