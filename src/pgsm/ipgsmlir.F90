integer function ipgsmlir(ifld, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar, grtyp)
    implicit none
    integer iun, ni, nj, nk, ip1, ip2, ip3, datev
    integer ifld(ni, nj, nk)
    character*12 etiket
    character*4 nomvar
    character*2 typvar
    character*1 grtyp
    external fstlir
    integer fstlir

    integer ier

    ier = fstlir(ifld, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar)

    if (ier < 0) then
        ipgsmlir = ier
        return
    endif

    call prefiltre(ifld, ni, nj, grtyp)
    ipgsmlir = ier
end
