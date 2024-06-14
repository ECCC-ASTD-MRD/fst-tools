module symetry
    implicit none

    !> Maximum number of entries in the symmetry table
    integer, parameter :: maxnom = 256

    !> Number of entries in the symmetry table
    integer, save :: nnoms

    !> Table of variable names
    character(len = 4), save :: noms(maxnom)
    !> Table containing the symmetry corresponding to each entry in noms
    logical, save :: ssym(maxnom)

contains

    subroutine init()
        implicit none

        nnoms = 0

        CALL cmetsym('GZ', .true.)
        CALL cmetsym('TT', .true.)
        CALL cmetsym('DD', .true.)
        CALL cmetsym('WW', .true.)
        CALL cmetsym('ES', .true.)
        CALL cmetsym('F2', .true.)
        CALL cmetsym('PN', .true.)
        CALL cmetsym('PS', .true.)
        CALL cmetsym('TS', .true.)
        CALL cmetsym('QQ', .false.)
    end subroutine init


    !> Insert entry in symetry table
    subroutine cmetsym(nom, sym) 
        use app
        use pgsm_mod, only : message
        implicit none

        !> Nom du champ
        character(len = 4), intent(in) :: nom
        !> Symtrique si vrai, antisymétrique si faux
        logical, intent(in) :: sym

        if (nnoms < maxnom) then
            nnoms = nnoms + 1
            ssym(nnoms) = sym
            noms(nnoms) = nom
        else
            if (message) then
                call app_log(APP_WARNING,'cmetsym: No more room in symetry tables METSYM')
            endif
        endif
    end


    !> Update symetry table
    subroutine metsym(nom, sym) 
        use app
        implicit none

        integer, intent(in) :: nom
        logical, intent(in) :: sym

        character(len = 4) :: cnom

        write(cnom, '(A4)') nom
        write(app_msg, *) 'metsym: NOM - ', nom, 'CNOM - ', cnom 
        call app_log(APP_INFO, app_msg)
        call cmetsym(cnom, sym)
    end


    !> Check if variable is symmetrical and set symmetrical if unknown
    logical function symetri(cnom)
        use app
        use grilles, only : cgrtyp
        use pgsm_mod, only : message
        implicit none

        !> Variable name
        character(len = 4), intent(in) :: cnom

        integer :: i

        symetri = .true.

        do i = 1, nnoms
            if (cnom == noms(i)) then
                symetri = ssym(i)
                return
            endif
        enddo

        if (message) then
            if (cgrtyp == 'A' .or. cgrtyp == 'B' .or. cgrtyp =='G') then
                write(app_msg, "(//2x,'symetri: Symetry of variable ',       a4,' is unknown',      ' it will be supposed to be symetric'//)") cnom
                call app_log(APP_WARNING, app_msg)
            endif
        endif

        call cmetsym(cnom, .true.)
    end
end module symetry
