module ecrires
    implicit none

    integer, save :: compression_level = 0
    integer, save :: etiksrt(3) = -1
    integer, save :: ip2srt = -1
    integer, save :: ip3srt
    integer, save :: niif = 1000
    integer, save :: niinc = 10
    integer, save :: niis = 1
    integer, save :: njjf = 1000
    integer, save :: njjnc = 10
    integer, save :: njjs = 1
    integer, save :: nwetiks
    integer, save :: typesrt = -1
    logical, save :: printsr = .false.
end module ecrires
