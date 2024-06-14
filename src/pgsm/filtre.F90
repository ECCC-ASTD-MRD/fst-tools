!> Filter data
subroutine filtre (slab, NI, NJ, Npass, list, L)
    implicit none

    !> x dimension of data
    integer, intent(in) :: NI
    !> y dimension of data
    integer, intent(in) :: NJ
    !> Number of filtering passes
    integer, intent(in) :: Npass
    !> Number of entries in the list
    integer, intent(in) :: l
    !> List of filter numbers
    integer, intent(in) :: list(L)
    !> Data to filter
    real, intent(inout) :: slab(NI , NJ)

    real facteur(-4:4, 5)
    real :: temp
    integer :: k, I, J
    integer :: nb_elm

    integer :: pass
    integer :: nb_elem, istart, iend
    real :: sum

    real, dimension(ni) :: result1
    real, dimension(nj) :: result2

    nb_elem = (l+1)/2
    istart = -nb_elem + 1
    iend = nb_elem -1

    do j = 1, nb_elem
        do I = istart, iend
            facteur(i, j) = 0.0
        enddo
    enddo

    do j = 1, nb_elem-1
        sum = 0.0
        do i = -j, j
            sum = sum + list(I+nb_elem)
        enddo

        do i = -j, j
            facteur(i, nb_elem-j) = 1.0*(list(i+nb_elem)) / sum
        enddo
    enddo
    do pass = 1, Npass
        do J = 1, NJ
            do I = 2, NI-1
                temp = 0
                nb_elm = min(I-1, NI-I, L/2)
                do k = -nb_elm, nb_elm
                    temp = temp + slab(I+k, J) * facteur(k, (L/2+1)-nb_elm)
                enddo
                result1(I) = temp
            enddo
            do I = 2, NI-1
                slab(I, J) = result1(I)
            enddo
        enddo

        do I = 1, NI
            do J = 2, NJ-1
                temp = 0
                nb_elm = min(J-1, NJ-J, L/2)
                do k = -nb_elm, nb_elm
                    temp = temp + slab(I, J+k) *                   facteur(k, (L/2+1)-nb_elm)
                enddo
                result2(J) = temp
            enddo
            do J = 2, NJ-1
                slab(I, J) = result2(J)
            enddo
        enddo
    enddo
end
