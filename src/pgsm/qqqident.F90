subroutine qqqident(position, separateur, items)
    implicit none

    integer position, separateur, items(16)

    integer, external :: argdims

#include "idents.cdk90"

    integer i

    qposition = position
    if (qposition.eq.5) then
        qnitems = 0
    endif

    if (separateur.eq.-1) then
        qcsepar = 'T'
    else
        write(qcsepar,'(a1)') separateur
    endif

    do i=1,16
        qitems(i) = 0
    enddo

    if (-1.eq.items(1)) then
        qnitems = 11
        do i=1,qnitems
            qitems(i) = i
        enddo
    else
        do i=1,argdims(3)
            qitems(i) = items(i)
        enddo
    endif
end
