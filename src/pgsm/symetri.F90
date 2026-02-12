!> FUNCTION QUI RECONNAIT SI LA VARIABLE EST SYMETRIQUE
logical function symetri(cnom)
    use app
    implicit none

    external cmetsym

!AUTEUR  P.SARRAZIN  FEVRIER  DRPN  DORVAL  P.Q.  CANADA
!
!REVISION 4.0.2
!   MODIFICATION ARGUMENT D'ENTREE "NOM"
!      DE "INTEGER" A "CHARACTER*2"
!   Y. CHARTIER DRPN DORVAL QUEBEC
!LANGAGE RATFOR
!
!OBJET(SYMETRI)
!         VERIFIER SI UNE VARIABLE EST SYMETRIQUE .TRUE.=SYMETRIQUE
!         MESSAGE SI VARIABLE N EST PAS RECONNUE DEFAULT SYMETRIQUE
!          SI UNE VARIABLE N EST PAS RECONNUE ELLE EST CONSIDEREE
!          COMME SYMETRIQUE
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!ARGUMENTS
!   IN    NOM    NOM DE LA VARIABLE
!
!
!APPEL    VIA MACPCP,EPAISUR
!         SYMETRI(NOMBRE)
!
!MESSAGES
!         LA SYMETRIE DE LA VARIABLE  EST INCONNUE
!         ON LA SUPPOSE SYMETRIQUE
!
!MODULES
!
! - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - -*

#include "defin.cdk90"
#include "voir.cdk90"
#include "symnom.cdk90"
#include "grilles.cdk90"

    character(len = *), parameter :: fmt = &
        "(2x, 'symetri: Symetry of variable ', a4, ' is unknown it will be supposed to be symetric')"

    character(len = 4) :: cnom
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
            write(app_msg, fmt) cnom
            call app_log(APP_WARNING, app_msg)
        endif
    endif

    call cmetsym(cnom, .true.)
end
