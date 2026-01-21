subroutine qqqident(position, separateur, items)
    implicit none

    integer, intent(in) :: position
    integer, intent(in) :: separateur
    integer, intent(in) :: items(16)

    integer, external :: argdims

! qcsepar, qitems, qnitems, qposition
#include "idents.cdk90"

    integer i

    ! print *, 'qqqident', position, separateur
    ! do i = 1, 16
    !     print *, i, items(i)
    ! enddo

    qposition = position
    if (qposition == 5) then
        qnitems = 0
    endif

    if (separateur == -1) then
        qcsepar = 'T'
    else
        write(qcsepar,'(a1)') separateur
    endif

    do i = 1, 16
        qitems(i) = 0
    enddo

    if (-1 == items(1)) then
        qnitems = 11
        do i = 1, qnitems
            qitems(i) = i
        enddo
    else
        do i = 1, argdims(3)
            qitems(i) = items(i)
        enddo
    endif
end
