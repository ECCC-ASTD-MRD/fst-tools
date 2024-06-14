!> Transfer la moitie du champ dans l'autre
subroutine loupmir(sortie, entre, npts)
    implicit none

    !> Number of points in the input field
    integer, intent(in) :: npts
    !> Output field
    real, intent(out) :: sortie(npts)
    !> Input field
    real, intent(in) :: entre(npts)

    integer :: i, nombpl1

    nombpl1 = npts + 1

    do i = 1, npts
        sortie(i + npts) = entre(nombpl1 - i)
    enddo
end

!> Transfer champ
subroutine louptra(sortie, entre, npts)
    implicit none

    !> Number of points in the input field
    integer, intent(in) :: npts
    !> Output field
    real, intent(out) :: sortie(npts)
    !> Input field
    real, intent(in) :: entre(npts)

    integer :: i

    do i = 1, npts
        sortie(i) = entre(i)
    enddo
end


!> Initialise le champ sortie avec 1.0
subroutine loupin1(sortie, entre, npts)
    implicit none

    !> Number of points in the input field
    integer, intent(in) :: npts
    !> Output field
    real, intent(out) :: sortie(npts)
    !> Input field (unused)
    real, intent(in) :: entre(npts)

    integer :: i

    do i = 1, npts
        sortie(i) = 1.0
    enddo
end


!> Soustrait deux champs
subroutine loupsou(sortie, entre, npts)
    implicit none

    !> Number of points in the input field
    integer, intent(in) :: npts
    !> Output field
    real, intent(out) :: sortie(npts)
    !> Input field
    real, intent(in) :: entre(npts)

    integer :: i

    do i = 1, npts
        sortie(i) = entre(i) - sortie(i)
    enddo
end
