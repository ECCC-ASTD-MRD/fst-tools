integer function pgsmluk(fld, key, ni, nj, nk, nomvar, grtyp)
    use app
    implicit none

    integer, external :: fstluk

    integer, intent(in) :: key
    integer, intent(out) :: ni
    integer, intent(out) :: nj
    integer, intent(out) :: nk
    real, dimension(ni, nj), intent(out) :: fld
    character(len = 4), intent(out) :: nomvar
    character(len = 1), intent(out) :: grtyp

    integer :: ier

    ier = fstluk(fld, key, ni, nj, nk)
    if (ier < 0) then
        pgsmluk = ier
        return
    endif

    call prefiltre(fld, ni, nj, nomvar, grtyp)
    pgsmluk = ier
end
