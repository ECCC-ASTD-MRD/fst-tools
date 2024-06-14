!> Replace the value in the last column of the field
subroutine lastcol(fld, val, idxmin, idxmax, step) 
    implicit none

    integer, intent(in) :: idxmin
    integer, intent(in) :: idxmax
    integer, intent(in) :: step
    real, intent(out) :: fld(idxmin:idxmax)
    real, intent(in) :: val

    integer :: i

    do i = idxmin, idxmax, step
        fld(i) = val
    enddo
end


!> Set minimum value in field
subroutine loupneg(fld, minval, idxmin, idxmax, step)
    implicit none

    integer, intent(in) :: idxmin
    integer, intent(in) :: idxmax
    real, intent(inout) :: fld(idxmin:idxmax)
    real, intent(in) :: minval
    integer, intent(in) :: step

    integer :: i

    do i = idxmin, idxmax, step
        fld(i) = amax1(fld(i), minval)
    enddo
end
