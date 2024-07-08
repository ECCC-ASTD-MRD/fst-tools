!> Batir une table avec noms, ecart, facteur, bas, haut
subroutine convs(nom, ecart, facteur, bas, haut)
    use app, only : app_log, APP_WARNING
    use convers, only : nomb, bass, ecarts, hauts, facts, nomss, ncon
    use pgsm_mod, only : message
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

    integer :: i
    character(len = 4) :: cnom

    write(cnom,'(A4)') nom

    i = 1
    do while(i <= nomb .and. cnom /= nomss(i))
        i = i + 1
    enddo

    nomb = max0(nomb, i)
    if (nomb < 256) then
        hauts(i) = 1.e+30
        bass(i) = -hauts(i)
        ecarts(i) = ecart
        facts(i) = facteur
        nomss(i) = cnom
        if (ncon >= 4) bass(i) = bas
        if (ncon == 5) hauts(i) = haut
    else
        if (message) then
            call app_log(APP_WARNING, 'convs: More than 256 directives, ignoring others')
        endif
    endif
end
