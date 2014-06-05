      subroutine putfld(fld, buf, iun,ibidon,iwrit,ni,nj,nbrow,npkc,istamp)
      implicit none
      integer iun, ibidon, iwrit, ni,nj,nbrow, npkc, istamp
      real fld(ni, nj)
      real buf(ni, nj)
      
      call putfld1(fld, ni*nj, iun)
      
      return
      end
      
      subroutine putfld1(fld, ni, iun)
      implicit none
      integer ni, iun
      real fld(ni)
      integer i
      
      write(iun) (fld(i), i=1,ni)
      
      return 
      end
      
