      subroutine testseq
      implicit none

      integer nigem, njgem, nigauss, njgauss, iun
      parameter (nigem = 160)
      parameter (njgem = 1)
      integer npac,idat, ideet, npas, ni, nj, nk, ip1, ip2o, ip3o, llg1, llg2, llg3, llg4, idatyp
      integer ier, fnom
      external fnom


      real champ(nigem,njgem)
      character*24 chaine
      
      character*4 nomvar, cbidon2
      character*2 typvar, bidon1, grtyp, grref, cbidon1
      character*12 etiket, cbidon8
      
      iun = 0
      ier = fnom(iun,'bofseq','SEQ+FTN+UNF',0)

      rewind(iun)
 10   read (iun,err=13) npac,idat, ideet, npas, ni, nj, nk,                ip1, ip2o, ip3o, llg1, llg2, llg3, llg4, idatyp,               chaine
      print *,  npac,idat, ideet, npas, ni, nj, nk,                ip1, ip2o, ip3o, llg1, llg2, llg3, llg4, idatyp,               chaine
      read (iun) champ
      print *, champ
      goto 10

 13   continue
      stop 
      end
