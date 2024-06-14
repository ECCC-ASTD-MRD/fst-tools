!> \deprecated This grid type is not supported in the version of PGSM
subroutine griltp4(nni, nnj, ip1, ip2, ip3)
    use app
    implicit none

    integer nni, nnj, ip1, ip2, ip3
    call app_log(APP_ERROR, 'griltp4: This grid type is not supported in the version of PGSM')
    call pgsmabt
end
