module nivos
    !> Table contenant les niveaux de pression
    integer, save :: nivospr(MXCHAMP)
    !> Nombre d'arguments pour directive moyent/moysrt
    integer, save :: nmoy
    !> Initialiser pour readlx
    integer, save :: nmo
end module nivos
