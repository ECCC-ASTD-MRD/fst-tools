program fststat
    use app
    implicit none
#include "fst-tools_build_info.h"

    external ccard, fstlnk

    character(len = 1024), dimension(40) :: cle, def, val
    data cle /40*'fst:'/
    data def /40*'scrap'/
    data val /40*'scrap'/


    integer :: fnom, ier, fstouv, fstopc
    logical :: flag

    integer :: date, ip1, ip2, ip3
    character(len = 12) :: etiket
    character(len = 4) :: nomvar
    character(len = 2) :: typvar

    integer :: i, ipos, nf, level
    integer :: ni, nj, nk

    integer, dimension(40) :: lnkdiun
    data lnkdiun /10, 11, 12, 13, 14, 15, 16, 17, 18, 19, &
        20, 21, 22, 23, 24, 25, 26, 27, 28, 29, &
        30, 31, 32, 33, 34, 35, 36, 37, 38, 39, &
        41, 41, 42, 43, 44, 45, 46, 47, 48, 49/

    call ccard(cle, def, val, 40, ipos)

    app_ptr=app_init(0,'fststat',FSTSTAT_VERSION,'',BUILD_TIMESTAMP)
    call app_start()

    ! Count the number of files
    nf = 1
    do while (val(nf) /= def(nf) .and. nf < 40)
         nf = nf + 1
    end do

    nf = nf -1
    do i = 1, nf
        ier = fnom(lnkdiun(i), val(i), 'RND+OLD+R/O+REMOTE', 0)
        if (ier < 0) then
            call app_log(APP_ERROR,'Fichier '//trim(val(i))//' inexistant')                  
            app_status=app_end(-1)
            call qqexit(app_status)
        end if
    end do

    do i = 1, nf
        ier = fstouv(lnkdiun(i), 'RND')
        if (ier .lt. 0) then
            call app_log(APP_ERROR,'le fichier '//trim(val(i))//'n''est pas standard random')                  
            app_status=app_end(-1)
            call qqexit(app_status)
        end if
    end do

    date = -1
    ip1  = -1
    ip2  = -1
    ip3  = -1
    etiket = '        '
    typvar = ' '
    nomvar = '  '

    call fstlnk(lnkdiun, nf)
    call loop_fields(lnkdiun(1), ni, nj, nk, date, etiket, ip1, ip2, ip3, typvar, nomvar)

    app_status=app_end(-1)
    call qqexit(app_status)
end program
