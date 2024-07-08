!> Sortie pas trop brutale en cas d'erreur
subroutine pgsmabt
    use app
    use files
    implicit none

    logical :: junk

    !> \todo Find out if app_status is still usefull/relevant
    app_status = app_end(13)

    if (outputFileMode == 1) junk = outputFile%close()
    call qqexit(13)
end
