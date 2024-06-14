integer function ipgsmlic(ifld, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar, ig1, ig2, ig3, ig4, grtyp)

    implicit none
    integer, intent(in) :: iun, ni, nj, nk, ip1, ip2, ip3, datev, ig1, ig2, ig3, ig4
    integer, intent(out) :: ifld(ni, nj, nk)
    character(len = 12), intent(in) :: etiket
    character(len = 4), intent(in) :: nomvar
    character(len = 2), intent(in) :: typvar
    character(len = 1), intent(in) :: grtyp

    integer ier

    ier = fstlic(ifld, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar, ig1, ig2, ig3, ig4, grtyp)

    if (ier < 0) then
        ipgsmlic = ier
        return
    endif

    call prefiltre(ifld, ni, nj, grtyp)
    ipgsmlic = ier
end
