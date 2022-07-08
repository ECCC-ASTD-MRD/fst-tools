!***s/p fststatm
!*
      program fststatm
      implicit none
!*
!*AUTHOR   Yves Chartier                      July 1993
!* 
!*REVISION
!*REVISION 001  M. Lepine - Mars 2005 - ajout de la fonctionnalite fichier remote
!*
!*LANGUAGE:  fortran 90
!*
!*OBJECT (fststatm)
!*
!*FILES
!*     tape1: TSF file
!*     tape10-49: RPN standard files 
!*
!*ARGUMENTS 
!*
!*IMPLICIT     
!*
!*MODULES
      external ccard, fstlnk      
      
      character*1024 cle(44), def(44), val(44)
      data cle /40*'fst:', 'istart', 'iend', 'jstart', 'jend'/
      data def /40*'scrap', '1',  '1000000000',  '1', '1000000000' /
      data val /40*'scrap', '1',  '1000000000',  '1',  '1000000000'/
      

      integer fnom, ier, fstouv, fstopi, fstopc
      logical flag
      
      integer date, ip1, ip2, ip3, imix(2), jmix(2)
      character*12 etiket
      character*4 nomvar
      character*2 typvar

      integer i, ipos, nf, level
      integer ni, nj, nk
      integer istart, iend, jstart, jend


      integer lnkdiun(40)
      data lnkdiun /10, 11, 12, 13, 14, 15, 16, 17, 18, 19, &
           20, 21, 22, 23, 24, 25, 26, 27, 28, 29, &
           30, 31, 32, 33, 34, 35, 36, 37, 38, 39, &
           41, 41, 42, 43, 44, 45, 46, 47, 48, 49 /

      call ccard(cle, def, val, 44, ipos)
      read(val(41), *)  istart
      read(val(42), *)  iend
      read(val(43), *)  jstart
      read(val(44), *)  jend

      istart = max(1, istart)
      jstart = max(1, jstart)


      if (istart .gt. iend) then
         WRITE(6, *)' **   CAN''T PROCESS REGION: istart = ', &
                    istart, ' > iend = ', iend
         stop 123
      endif

      if (jstart .gt. jend) then
         WRITE(6, *)' **   CAN''T PROCESS REGION: jstart = ', &
                    jstart, ' > jend = ', jend
         stop 456
      endif

      if (istart .gt. 1 .and. jstart .gt. 1) then
         print *, '************************************************* &
                   *****************************'
         WRITE(6, 40), istart, iend, jstart, jend
         print *, '************************************************* & 
                   *****************************'
      endif

 40   format ('** PROCESSING REGION: ', &
              ' (istart, iend): ', '(',i6,',',i6,'),', &
              ' (jstart, jend): ', '(',i7,',',i7,')') 
     
      level = 6
      flag = .false.
      ier = fstopc('MSGLVL', 'ERRORS', flag)

      nf = 1
 33   if (val(nf) .ne. def(nf) .and. nf .le. 40) then
         nf = nf + 1
         goto 33
      endif
      
      nf = nf -1
      do 34 i = 1, nf
         ier = fnom(lnkdiun(i), val(i), 'RND+OLD+R/O+REMOTE', 0)

         if (ier .lt. 0) then
            print *, '************************************************'
            print *, ' probleme avec fichier ', val(i), ' inexistant - '
            print *, '************************************************'
            stop
         endif
 34   continue
      
      do 35 i = 1, nf
         ier = fstouv(lnkdiun(i), 'RND')
         if (ier.lt.0) then
            print *, '**********************************************'
            print *, '* le fichier #', val(i), &
                     'n''est pas standard random'
            print *, '**********************************************'
            stop
         endif
 35   continue
      date = -1
      ip1  = -1
      ip2  = -1
      ip3  = -1
      etiket = '        '
      typvar = ' '
      nomvar = '  '

      call fstlnk(lnkdiun, nf)   
      call fststat(lnkdiun(1), ni, nj, nk, date, &
                   etiket, ip1, ip2, ip3, typvar, nomvar, &
                   istart, iend, jstart, jend)

 11   format(A16)
      
      stop
      end
      
      character *128 function product_id_tag()
      product_id_tag='$Id$'
      return
      end
