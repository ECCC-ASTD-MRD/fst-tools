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

    ! Default -style columns, used when -style is absent (matches the 20.0 default).
    character(len = 128), parameter :: default_style = 'NINJNK+DATEO+IG1234'

    character(len = 8) :: cles(ncle)
    character(len = 128) :: val(ncle), def(ncle)
    data cles / 'IMENT:', 'SEQ', 'STYLE', 'MOREHELP', 'V' /
    ! def(3) is used for a bare '-style' (present, no value): matches 20.0's bare default.
    ! val(3) is used when '-style' is absent: the default columns (20.0's default).
    data def / '/dev/null', 'SEQ', 'NINJNK+DATEV+LEVEL+GRIDINFO', 'MOREHELP', VERSION /
    data val / '/dev/null', 'RND' , default_style, 2*' ' /

    call c_init_appl_var_table()
    ipos = -1
    call ccard (cles, def, val, ncle, ipos)

    app_status = 1
    IF (val(4) .eq. 'MOREHELP') THEN
        print *,"*** VOIR CALLING SEQUENCE ***"
        print *
        print *,'-IMENT [scrap:scrap]'
        print *,'-SEQ [RND:SEQ]'
        print *,'-STYLE [list of + separated tokens]'
        print *,'-MOREHELP [:MOREHELP]'
        print *,'-V [:version]'
        print *
        print *,'   -style selects which columns are shown. A non-empty -style starts from a'
        print *,'   base state, each recognized token overrides it, and unrecognized tokens are'
        print *,'   ignored. Tokens are case-insensitive and may be separated by "+" or spaces.'
        print *,'   When -style is absent, the default columns are shown.'
        print *,'   A bare -style (present, no value) shows NINJNK+DATEV+LEVEL+GRIDINFO.'
        print *
        print *,'   Base state (non-empty -style, no recognized token):'
        print *,'       NOMV TV ETIQUETTE IP1 IP2 IP3 DEET NPAS DTY SIZ'
        print *
        print *,'   Default columns (when -style is absent):'
        print *,'       NOMV TV ETIQUETTE NI NJ NK (DATE-O h m s) IP1 IP2 IP3 DEET NPAS DTY SIZ G IG1 IG2 IG3 IG4'
        print *
        print *,'   Tokens that ADD a column (off in the base state):'
        print *,'       NINJNK      show the dimensions (NI NJ NK)'
        print *,'       DATEO       show the origin date (DATE-O)'
        print *,'       DATESTAMPO  show the origin date stamp (STAMP-O)'
        print *,'       DATEV       show the valid date (DATE-V) and its stamp (STAMP-V)'
        print *,'       LEVEL       show the vertical level (LEVEL)'
        print *,'       DECODE      show the decoded IP block (alias: IPALL)'
        print *,'       GRID        show the decoded grid (XG1..XG4) (alias: GRIDINFO)'
        print *,'       IG1234      show the grid descriptors (IG1..IG4)'
        print *,'       META        show the extended metadata (JSON)'
        print *,'       FULL        show everything interesting (NINJNK+DATEV+DECODE+GRID)'
        print *
        print *,'   Tokens that REMOVE a column (on in the base state):'
        print *,'       NONOMVAR    hide the variable name (NOMV) (alias: NONOMV)'
        print *,'       NOTYPVAR    hide the field type (TV) (alias: NOTYPV)'
        print *,'       NOETIKET    hide the label (ETIQUETTE) (alias: NOETIQ)'
        print *,'       NOIP1       hide IP1'
        print *,'       NOIP2       hide IP2'
        print *,'       NOIP3       hide IP3'
        print *,'       NOIP23      hide IP2 and IP3 (the 20.0 token)'
        print *,'       NOIPS       hide IP1, IP2 and IP3'
        print *,'       NODEET      hide DEET'
        print *,'       NONPAS      hide NPAS'
        print *,'       NODATYP     hide the data type (DTY) (alias: NODTY)'
        print *,'       NOSIZ       hide the data size (SIZ)'
        print *
        print *,'   Example #1: -style "NINJNK+DATEV+LEVEL"'
        print *,'   Example #2: -style "NODATEO+DECODE+GRID"'
        print *,'   default   : NINJNK+DATEO+IG1234'
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
