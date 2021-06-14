        program makebidon
        implicit none
        character *12  nomprg
        character *20  nomabs
        integer long_111, xml2fst, lng, ier
        external xml2fst
        nomprg = 'null'

        nomprg='            '
c               12345678901234567890
        nomabs='                    '
        call getarg(1,nomprg)
        call getarg(0,nomabs)
        lng = long_111(nomprg)
        ier = xml2fst(nomprg,lng)

        stop
        end


      INTEGER FUNCTION LONG_111(NOM)
      implicit none
      CHARACTER * (*) NOM
*
      INTEGER LNG,I
*
      LNG = LEN(NOM)
      DO 10 I = LNG,1,-1
         IF (NOM(I:I) .EQ. ' ') THEN
            LNG = LNG - 1
         ELSE
            GOTO 20
         ENDIF
 10   CONTINUE
 20   CONTINUE
      LONG_111 = LNG
      RETURN
      END
