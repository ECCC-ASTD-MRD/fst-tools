!> Print message if a gaussian grid does not have an even number of points
subroutine messags(ni)
    use app
    implicit none

    integer, intent(in) :: ni

    write(app_msg, *) 'messags: The number of longitude has to be even for a gaussian grid, #LONG=', ni
    call app_log(APP_WARNING, app_msg)
end
