!> Calculer tourbillon relatif
subroutine tourbillon_relatif(field, nrows, ncols, lat) 
    implicit none

    !> Calculer le coriolis parameter pour chaque point de la grille.
    !> Soustraire ce champ du champ du tourbillon absolu on genere un tourbilonn relatif

    !> Champ contenant tourbillon absolu
    real, dimension(nrows, ncols), intent(inout) :: field
    !> Nombre de points sur une rangee du champ
    integer, intent(in) :: nrows
    !> Nombre de points dans une colonne du champ
    integer, intent(in) :: ncols
    !> Latitude pour chaque point du champ
    real, dimension(nrows, ncols), intent(in) :: lat

    real, parameter :: degarad = 3.1415926535 / 180.0
    real, parameter :: earth_rotation = 7.292*1.e-5
    real, parameter :: omega2 = 2 * earth_rotation

    integer :: i, j

    do j = 1, ncols
        do i = 1, nrows
            field(i, j) = field(i, j) - omega2 * sin(lat(i, j) * degarad)
        enddo
    enddo
end
