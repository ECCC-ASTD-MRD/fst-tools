module convers
    integer, save :: ncon
    !> Indice augmenter a chaque appel de convs
    integer, save :: nomb

    !> Écart correspondant a chaque nom
    real, save :: ecarts(256)
    !> Facteur correspondant a chaque nom
    real, save :: facts(256)
    !> Valeur de chaque champ(minimale acceptee) correspondant a chaque champ dans la table nomss
    real, save :: bass(256)
    !> Valeur maximale de chaque pt du champ correspondant dans la table des noms
    real, save :: hauts(256)

    !> Table contenant tous les noms des champs a modifier
    character(len = 4), save :: nomss(256)

contains

    subroutine init()
        nomb = 0
        nomss = '    '
        ecarts = 0.0
        fact = 1.0
    end subroutine init
end module
