!     ***************************************************************
!     *                     A S S E M B L E                         *
!     * Object :                                                    *
!     *         To assemble data field                              *
!     *                                                             *
!     * Arguments :                                                 *
!     *            IN     ni    : 1st dimension of field ZOUT       *
!     *            IN     nj    : 2nd dimension of field ZOUT       *
!     *            IN     nrows : 3rd dimension of field ZOUT       *
!     *            IN     slab  : data to assemble                  *
!     *            IN     nX    : dimension of hxpos                *
!     *            IN     hxpos : indicator of position in the grid *
!     *                                                             *
!     *            OUT    ZOUT  : field to return (assembled)       *
!     *                                                             *
!     ***************************************************************
      subroutine assemble(ZOUT,ni,nj,nrows,slab,nX,hxpos)
      implicit none

      integer nj, ni, nX, nrows
      real ZOUT(ni * nj, nrows)
      integer hxpos(nX)
      real slab(nX,nrows)
      integer I,k
      do k=1, nrows
         do I=1, nX
            ZOUT(hxpos(I), k) = slab(I,k)
         enddo
      enddo
      return
      end
!     ***************************************************************
!     *                     W R T S T D F                           *
!     * Object :                                                    *
!     *         To write standard file (FSTD)                       *
!     *                                                             *
!     * Arguments :                                                 *
!     *            IN    ZOUT   : data field to read                *
!     *            IN    iun    : unit number of the file           *
!     *            IN    dateo  : origin date of the field          *
!     *            IN    deet   : time step length in seconds       *
!     *            IN    npas   : time step number                  *
!     *            IN    ni     : 1st dimension of field            *
!     *            IN    nj     : 2nd dimension of field            *
!     *            IN    nrows  : 3rd dimension of field            *
!     *            IN    ip1    : descriptor 1 (1 to 32767)         *
!     *            IN    ip2    : descriptor 2 (1 to 32767)         *
!     *            IN    ip3    : descriptor 3 (1 to 32767)         *
!     *            IN    typvar : field type                        *
!     *            IN    nomvar : name of field                     *
!     *            IN    etiket : 9 caracter stamp                  *
!     *            IN    grtyp  : grid type                         *
!     *            IN    ig1    : grid descriptor 1 (0 to 2047)     *
!     *            IN    ig2    : grid descriptor 2 (0 to 2047)     *
!     *            IN    ig3    : grid descriptor 3 (0 to 65535)    *
!     *            IN    ig4    : grid descriptor 4 (0 to 65535)    *
!     *            IN    datyp  : type of data field                *
!     *            IN    Nextra : number of extra parameters        *
!     *                           (Nextra >= 0)                     *
!     *            IN    xtra   : field of optionnal variable       *
!     *                                (absent IF Nextra = 0)       *
!     *                                                             *
!     ***************************************************************
!       subroutine wrtstdf (ZOUT,iun, dateo, deet, npas, ni, nj,nxgrid,            nygrid,  nrows, ip1, ip2, ip3, typvar,nomvar, etiket,            grtyp, ig1, ig2, ig3, ig4, datyp,Nextra,xtra, nbits,            iflt,list,L, S)
!       implicit none
!       integer fstecr
!       integer ni, nj, nrows, k, Nextra, i, j
!       integer nxgrid, nygrid
!       real ZOUT(ni , nj, nrows), work(1), xtra(nrows, Nextra)
!       integer ip1(nrows), ip2(nrows), ip3(nrows),npak, nbits(nrows)
!       integer ig1, ig2, ig3, ig4
!       integer iun, datyp(nrows)
!       integer npas, deet, dateo
!       character *4 nomvar(nrows)
!       character *4 typvar(nrows)
!       character *4 grtyp
!       character *12 etiket
!       integer ierr, ier, S
!       integer L, iflt(nrows)
!       integer list(L)
!       integer sum
!       real, dimension (:,:), allocatable :: fact, temp
!
!       allocate(fact(L, (L+1)/2))
!       do k=1, (L+1)/2
!          do I=1, L
!             fact(I,k) = 0
!          enddo
!       enddo
!       do k=1,(L + 1)/2
!          sum = 0
!          do I=k, L - k + 1
!             sum = sum + list(I)
!          enddo
!          do I=k,L - k + 1
!             FACT(I,k) = float(list(I)) / float(sum)
!          enddo
!       enddo
!
!       allocate(temp(nxgrid, nj))
!       do 300 k=1, nrows
!          npak = -nbits(k)
!          if (nxgrid .eq. (ni + 1)) then
!             do j=1, nj
!                do i=1, ni
!                   Temp(i,j) = ZOUT(i,j,k)
!                enddo
!             enddo
!             do j=1,nj
!                Temp(nxgrid,j) = Temp(1,j)
!             enddo
!             if ((iflt(k) .GT. 0) .and. (L .gt. 1)) then
!                call filtre (Temp,nxgrid,nj,nrows,iflt(k),FACT,list,L)
!             endif
!             ierr = fstecr(Temp, work, npak, iun, dateo, deet,             npas, nxgrid, nj, 1, ip1(k), ip2(k), ip3(k),             typvar(k)(1:1), nomvar(k)(1:2), etiket(1:8),            grtyp(1:1), ig1, ig2, ig3, ig4, datyp(k),            .false.)
!
!          else
!             if ((iflt(k) .GT. 0) .and. (L .gt. 1)) then
!                call filtre (ZOUT(1,1,k),nxgrid,nj,nrows,iflt(k),FACT               ,list,L)
!             endif
!             ierr = fstecr(ZOUT(1,1,k), work, npak, iun, dateo, deet,             npas, ni, nj, 1, ip1(k), ip2(k), ip3(k),             typvar(k)(1:1), nomvar(k)(1:2), etiket(1:8),            grtyp(1:1), ig1, ig2, ig3, ig4, datyp(k),            .false.)
!          endif
!  300  continue
!
!       deallocate(temp)
!       deallocate(fact)
!
!       if (Nextra .ne. 0) then
!          ierr = fstecr(xtra ,WORK,npak, iun, 20002020,         1, 1, nrows, Nextra,1, 0,0,S,'|',         '||' ,'||||*||||','x',0,0,         0,0,1, .false.)
!       endif
!
!       return
!       end
!     ***************************************************************
!     *                       W S T D F X Y                         *
!     * Object :                                                    *
!     *         To write record ('>>' and '^^') in standard file    *
!     *                                                             *
!     * Arguments :                                                 *
!     *            IN    xpos   : field to write (dim : ni)         *
!     *            IN    ypos   : filed to write (dim : nj)         *
!     *            IN    iun    : unit number of the file           *
!     *            IN    datoe  : date of origine of the field      *
!     *            IN    deet   : time step lenght in seconds       *
!     *            IN    npas   : time step number                  *
!     *            IN    ni     : dimension of xpos                 *
!     *            IN    nj     : dimension of ypos                 *
!     *            IN    ip1    : descriptor 1                      *
!     *            IN    ip2    : descriptor 2                      *
!     *            IN    ip3    : descriptor 3                      *
!     *            IN    etiket : 9 caracter stamp                  *
!     *            IN    grtyp_ : grid type for ">>" and "^^"       *
!     *            IN    ig1_   : grid descriptor 1 of ">>" and "^^"*
!     *            IN    ig2_   : grid descriptor 2 of ">>" and "^^"*
!     *            IN    ig3_   : grid descriptor 3 of ">>" and "^^"*
!     *            IN    ig4_   : grid descriptor 4 of ">>" and "^^"*
!     *                                                             *
!     ***************************************************************
      subroutine wstdfxy (xpos, ypos, iun, dateo, deet, npas, ni, nj,  	                  ip1, ip2, ip3, etiket, grtyp_, ig1_,                     ig2_, ig3_, ig4_)
      implicit none
      integer fstecr
      integer ni, nj
      real xpos(ni), ypos(nj), work(1)
      integer ip1, ip2, ip3
      integer ig1_, ig2_, ig3_, ig4_
      integer datyp, npak, npas, deet, dateo

      integer i

      character *4 grtyp_
      character *12 etiket
      integer ierr, iun
      npak = -24
      datyp = 1

      ierr = fstecr(xpos, work, npak, iun, dateo, deet,                   npas, ni, 1, 1, ip1, ip2, ip3,                   'X ', '>>  ', etiket,                  grtyp_(1:1), ig1_, ig2_, ig3_, ig4_, datyp,                  .false.)
      ierr = fstecr(ypos, work, npak, iun, dateo, deet,                   npas, 1, nj, 1, ip1, ip2, ip3,                   'X ', '^^  ', etiket,                  grtyp_(1:1), ig1_, ig2_, ig3_, ig4_, datyp,                  .false.)

      return
      end
