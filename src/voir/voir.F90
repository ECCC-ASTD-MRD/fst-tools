program voir

    use app
    use rmn_fst24
    implicit none

#include "fst-tools_build_info.h"
    integer, parameter :: ncle = 5
    external ccard, c_init_appl_var_table
    character(len = 8192) :: ccard_arg, filename
    external ccard_arg

    type(fst_file) :: source
    logical        :: success

    integer ipos

    character(len = 8) :: cles(ncle)
    character(len = 128) :: val(ncle), def(ncle)
    data cles / 'IMENT:', 'SEQ', 'STYLE', 'MOREHELP', 'V' /
    data def / '/dev/null', 'SEQ', 'NINJNK+DATEV+LEVEL+IP1+GRIDINFO', 'MOREHELP', VERSION /
    data val / '/dev/null', 'RND' , 'NOMVAR+TYPVAR+ETIKET+NIJK+IPS+DATEO+DATYP+DEET+NPAS+IGS', 2*' ' /

    call c_init_appl_var_table()
    ipos = -1
    call ccard (cles, def, val, ncle, ipos)

    app_status = 1
    IF (val(4) .eq. 'MOREHELP') THEN
        print *,"*** VOIR CALLING SEQUENCE ***"
        print *
        print *,'-IMENT [scrap:scrap]'
        print *,'-SEQ [RND:SEQ]'
        print *,'-STYLE [NOMVAR TYPVAR ETIKET NIJK DATEO DATEV STAMP LEVEL DATYP DEET NPAS IP1 IP2 IP3 IPS DECODE GRID IGS META]'
         print *
        print *,'   Example #1: -style "NIJK DATEV LEVEL"'
        print *,'   Example #2: -style DATEV+LEVEL+IP1'
        print *,'   default : NOMVAR+TYPVAR+ETIKET+NIJK+IPS+DATEO+DATYP+DEET+NPAS+IGS'
    else
        app_ptr=app_init(0,'VOIR',VOIR_VERSION,'',BUILD_TIMESTAMP)
        call app_logstream('stdout')
        call app_start()

        filename = ccard_arg(cles(1))
        if (trim(filename) == "") filename = trim(val(1))
        success = source%open(filename,'STD+R/O+REMOTE'//val(2))

        if (success) then
            call source%print_summary(string=val(3))
            success = source%close()
        endif
        app_status=app_end(-1)
    endif
    call qqexit(app_status)
end
