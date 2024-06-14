subroutine lopascm(inoutFld, inFld, fact, npts)
    use app
    implicit none

    !> Number of points in each fields
    integer, intent (in) :: npts
    !> Operand and output field
    real, intent(inout) :: inoutFld(npts)
    !> Second operand field
    real, intent (in) :: inFld(npts)
    !> Operator
    !> | Value | Operation                    |
    !> | ----: | :--------------------------- |
    !> |    -1 | Subtract (inoutFld - inFld)  |
    !> |     1 | Add                          |
    !> |     2 | Add the square of each point |
    !> |     3 | Multiply                     |
    !> |     4 | Division (inoutFld / inFld)  |
    integer, intent (in) :: fact

    integer :: i

    if (abs(fact) == 1) then
        ! Soustraire si fact = -1  additionner si fact = 1
        do i = 1, npts
            inoutFld(i) = inoutFld(i) + inFld(i) * fact
        enddo
    else if (fact == 2)  then
        ! Additionner chaque point des deux champs au carre
        do i = 1, npts
            inoutFld(i) = inoutFld(i)**2 + inFld(i)**2
        enddo
    else if (fact == 3) then
        ! Multiplier chaque point des deux champs
        do i = 1, npts
            inoutFld(i) = inoutFld(i) * inFld(i)
        enddo
    else  if (fact == 4) then
        ! Diviser chaque point des deux champs
        do i = 1, npts
            if (inFld(i) == 0.0) then
                call app_log(APP_ERROR, 'lopascm: One of the array alement has a 0.0 value for a division')
                call pgsmabt
            endif
            inoutFld(i) = inoutFld(i) / inFld(i)
        enddo
    else
        call app_log(APP_ERROR, 'lopascm: Invalid operation (fact)')
    endif
end
