module nivos
    implicit none

    integer, parameter :: mxnivos = 31

    !> Table contenant les niveaux de pression
    integer, save :: nivospr(mxnivos)
    !> Nombre d'arguments pour directive moyent/moysrt
    integer, save :: nmoy
    !> Initialiser pour readlx
    integer, save :: nmo
end module nivos
