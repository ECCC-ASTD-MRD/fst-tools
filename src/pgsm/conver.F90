subroutine conver(fld, ni, nj, cnom)
    use convers, only : nomb, nomss, bass, hauts, ecarts, facts
    implicit none

    !> Number of points on the first axis
    integer, intent(in) :: ni
    !> Number of points on the second axis
    integer, intent(in) :: nj
    !> Variable name
    character(len = 4), intent(in) :: cnom
    !> Field
    real, intent(inout) :: fld(ni,nj)

    ! Augmente un champ d une valeur uniforme (ecarts) et
    ! multiplier par un facteur approprie elimine les valeurs trop petites 
    ! ou trop grandes predeterminees

    integer :: i, j, k

    if ( nomb .eq. 0 ) return

    k = 0
    do i = 1, nomb
        if (cnom .eq. nomss(i)) then
            k = i
            exit
        endif
    enddo

    if ( k .eq. 0 ) return

    do j = 1, nj
        do i = 1, ni
            fld(i,j) = amax1(bass(k), amin1(hauts(k), (fld(i,j) + ecarts(k)) * facts(k)))
        enddo
    enddo
end
