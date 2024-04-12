integer function ipgsmluk(ifld, key, ni, nj, nk, nomvar, grtyp)
    implicit none
    integer key, ni, nj, nk
    integer, dimension(ni, nj) :: ifld
    character*4 nomvar
    character*1 grtyp
    external fstluk
    integer fstluk


    integer :: ier

    ier = fstluk(ifld, key, ni, nj, nk)

    if (ier < 0) then
        ipgsmluk = ier
        return
    endif

    ipgsmluk = ier
end
