program bidon
    implicit none
    INTERFACE
        FUNCTION fst2xml(a,b) BIND(C)
            integer :: a, fst2xml
            character :: b
        END FUNCTION
    END INTERFACE
integer :: a, result
character :: b
a = 3
b = 'a'
result = fst2xml(a,b)
stop
end
