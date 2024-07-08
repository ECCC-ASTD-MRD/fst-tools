module accum
    implicit none

    !> Nombre de grids pts est-ouest dans acumula
    integer, save :: nni
    !> Nombre de grids pts nord-sud dans acumula
    integer, save :: nnj
    !>
    integer, save :: nnk
    !> Date(cmc stamp) du champ acumula
    integer, save :: idatt
    !> Nombre de secondes par pas
    integer, save :: ideet
    !> Numero du pas du temp
    integer, save :: npas
    !> Niveau du champ acumula
    integer, save :: jpp1
    !> Heure du champ acumula
    integer, save :: jpp2
    !> Libre (utiliser par usager)
    integer, save :: jpp3
    !> Parametre decrit dans pgsm sous grille
    integer, save :: igg1
    !> Parametre decrit dans pgsm sous grille
    integer, save :: igg2
    !> Parametre decrit dans pgsm sous grille
    integer, save :: igg3
    !> Parametre decrit dans pgsm sous grille
    integer, save :: igg4
    !> Etiquette du champ acumula max 10 caracteres
    character(len = 12), save :: cetik
    !> Type de variabla "p"=prevision "a"=analyse
    character(len = 2), save :: ctypv
    !> Type de grille   g=gaussien  n=polaire nord.....
    character(len = 1), save :: cigty
    !> Nom du champ acumula lcar(gz),"tt"......
    character(len = 4), save :: cnumv

    !> Compteur augmenter a chaque appel pluse-pluss-moinse-moinss
    integer, save :: icnt = 0

    logical, save :: unefois = .false.
    logical, save :: once = .false.
end module accum
