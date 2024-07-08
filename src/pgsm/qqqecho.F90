subroutine qqqecho(chaine)
    use app
    implicit none

    integer, intent(in) :: chaine(20)

    integer, external :: argdims

    integer :: i, j, longueur, iun

    character(len = 80) :: message

    longueur = argdims(1)

    message(1:80) = ' '

    do i=1, longueur
        j = 4*(i-1)+1
        write(message(j:j+3), '(a4)') chaine(i)
    enddo

    iun = 2
    call pgsmecho(iun, message, longueur*4)
end
