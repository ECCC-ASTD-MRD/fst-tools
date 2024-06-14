module ecrires
    integer, save :: compression_level
    integer, save :: etiksrt(3)
    integer, save :: ip2srt
    integer, save :: ip3srt
    integer, save :: niif
    integer, save :: niinc
    integer, save :: niis
    integer, save :: njjf
    integer, save :: njjnc
    integer, save :: njjs
    integer, save :: nwetiks
    integer, save :: typesrt
    logical, save :: printsr

contains

    subroutine init()
        printsr = .false.
        niis = 1
        njjs = 1
        niif = 1000
        njjf = 1000
        niinc = 10
        njjnc = 10
        ip2srt = -1
        etiksrt = -1
        typesrt = -1
        compression_level = 0
    end subroutine
end module ecrires
