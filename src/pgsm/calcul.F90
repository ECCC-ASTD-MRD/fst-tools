!> Moyenne zonale ou meridionale d un champ
subroutine calcul(entre, sortie, ni, nj, poids, ccoupe, grtyp)
    implicit none

    !> East-West field dimension
    integer, intent(in) :: ni
    !> North-South field dimension
    integer, intent(in) :: nj
    !> Input field
    real, intent(in) :: entre(ni, nj)
    !> 
    real, intent(out) :: sortie(max(ni, nj))
    !> Weights used to compute the average
    real, intent(in) :: poids(nj)
    !> Cut direction (MER|ZON)
    character(len = 8), intent(in) :: ccoupe
    !> Grid type
    character(len = 1), intent(in) :: grtyp

    integer :: i, j, nii
    real :: sum, sump

    if (ccoupe == 'MER ') then

        do i = 1, ni  
            sum = 0.0
            sump = 0.0
            do j = 1, nj  
                sum = sum + entre(i, j) * poids(j)
                sump = sump + poids(j)
                sortie(i) = sum / sump
            enddo
        enddo
    else
        nii = ni

        ! SI IGTYP = 'B' ON ELIMINE LA DERNIERE LONGITUDE 
        if (grtyp == 'B') then
            nii = ni - 1
        endif

        do j = 1, nj  
            sum = 0.0
            do i = 1, nii
                sum = sum + entre(i, j)
                sortie(j) = sum / nii
            enddo
        enddo
    endif
end
