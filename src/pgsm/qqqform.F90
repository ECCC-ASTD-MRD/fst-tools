subroutine qqqform(theform)
    implicit none

    integer, intent(in) :: theform(4)

#include "idents.cdk90"

    integer, external :: argdims

    integer i , j, longueur

    longueur = argdims(1)

    qcform(1:16) = ' '

    do i = 1, longueur
        j = 4 * (i - 1) + 1
        write(qcform(j:j + 3),'(a4)') theform(i)
    enddo
end
