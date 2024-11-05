program fststat
    use app
    use rmn_fst24
    implicit none

#include "fst-tools_build_info.h"

    external ccard

    character(len = 1024), dimension(40) :: cle, def, val
    data cle /40*'fst:'/
    data def /40*'scrap'/
    data val /40*'scrap'/

    integer :: fstopc
    logical :: success
    type(fst_file), dimension(40) :: sources

    integer :: i, ipos, nf

    call ccard(cle, def, val, 40, ipos)

    app_ptr=app_init(0,'fststat',FSTSTAT_VERSION,'',BUILD_TIMESTAMP)
    call app_logstream('stdout')
    call app_start()

    ! Count the number of files
    nf = 1
    do while (val(nf) /= def(nf) .and. nf < 40)
         nf = nf + 1
    end do

    nf = nf -1
    do i = 1, nf
        success = sources(i)%open(trim(val(i)),'STD+R/O+REMOTE')
        if (.not. success) then
            call app_log(APP_ERROR,'Cannot open file '//trim(val(i)))                  
            app_status=app_end(-1)
            call qqexit(app_status)
        end if
    end do

    if (.not. fst24_link(sources(1:nf))) then
        call app_log(APP_ERROR, 'Unable to link source files')
    else
        call loop_fields(sources(1))
    endif

    do i = 1, nf
        success = sources(i) % close()
    end do

    app_status=app_end(-1)
    call qqexit(app_status)
end program
