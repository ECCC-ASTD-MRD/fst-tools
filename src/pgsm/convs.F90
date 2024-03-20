!> Batir une table avec noms, ecart, facteur, bas, haut
subroutine convs(nom, ecart, facteur, bas, haut)
    use app
    implicit none

    !> Nom (numéro) du champ que l on veut modifier
    integer, intent(in) :: nom
    !> Valeur plmnmodr au champ
    real, intent(in) :: ecart
    !> Valeur utilisée pour multiplication
    real, intent(in) :: facteur
    !> Les valeurs inférieures à bas auront la valeur de bas
    real, intent(in) :: bas
    !> Les valeurs supérieures à haut auront la valeur de haut
    real, intent(in) :: haut

    !> Appelé via la directive : conv(nom, ecart, facteur, bas, haut)
    !> La directive convs assigne a chaque table la valeur appropriee les tables augmentent a chaque appel convs

#include "voir.cdk90"
#include "convers.cdk90"
#include "dummys.cdk90"

    ! trouver si le nom existe
    !  oui- remplacer
    !  non- plmnmodr

    integer :: i
    character(len = 4) :: cnom
    character(len = 4) :: cnoma

    write(cnom,'(A4)') nom

    cnoma = cnom
    i = 1
    do while(i <= nomb .and. cnoma /= nomss(i))
        i = i + 1
    enddo

    ! definir nomb danger si plus grand que 40
    nomb = max0(nomb, i)
    if (nomb < 256) then
        hauts(i) = 1.e+30
        bass(i) = -hauts(i)
        ecarts(i) = ecart
        facts(i) = facteur
        nomss(i) = cnoma
        if (ncon >= 4) bass(i) = bas
        if (ncon == 5) hauts(i) = haut
    else
        if (message) then
            call app_log(APP_WARNING,'convs: More than 256 directives, ignoring others')
        endif
    endif
end
