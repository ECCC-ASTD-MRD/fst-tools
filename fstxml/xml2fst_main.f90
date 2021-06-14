program bidon
    implicit none
    INTERFACE
        FUNCTION xml2fst(a,b) BIND(C)
            integer :: a, xml2fst
            character :: b
        END FUNCTION
    END INTERFACE
integer :: a, result
character :: b
a = 3
b = 'a'
result = xml2fst(a,b)
stop
end
