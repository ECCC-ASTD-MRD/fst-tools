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
!> Assemble data field
subroutine assemble(ZOUT, ni, nj, nrows, slab, nX, hxpos)
    implicit none

    integer, intent(in) :: nj
    integer, intent(in) :: ni
    integer, intent(in) :: nX
    integer, intent(in) :: nrows
    real, intent(out) :: ZOUT(ni * nj, nrows)
    integer, dimension(nX), intent(in) :: hxpos
    real, intent(in) :: slab(nX, nrows)

    integer :: I, k

    do k = 1, nrows
        do I = 1, nX
            ZOUT(hxpos(I), k) = slab(I, k)
        enddo
    enddo
end

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
subroutine wstdfxy (xpos, ypos, iun, dateo, deet, npas, ni, nj, ip1, ip2, ip3, etiket, grtyp_, ig1_, ig2_, ig3_, ig4_)
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

    ierr = fstecr(xpos, work, npak, iun, dateo, deet, npas, ni, 1, 1, ip1, ip2, ip3, 'X ', '>>  ', etiket, grtyp_(1:1), ig1_, ig2_, ig3_, ig4_, datyp, .false.)
    ierr = fstecr(ypos, work, npak, iun, dateo, deet, npas, 1, nj, 1, ip1, ip2, ip3, 'X ', '^^  ', etiket, grtyp_(1:1), ig1_, ig2_, ig3_, ig4_, datyp, .false.)
end
