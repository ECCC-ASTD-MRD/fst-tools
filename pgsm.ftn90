        program pgsm
        implicit none

        call pgsm2

        stop
        end


        character *128 function product_id_tag()
        implicit none
        character(len=16) :: PGSM_VERSION
        include 'version.inc'
        product_id_tag=PGSM_VERSION
        return
        end
