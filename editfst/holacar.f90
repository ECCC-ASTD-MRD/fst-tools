!**S/R HOLACAR - UTILITAIRE PERMETTANT DE TRANSFORMER EN HOLLERITH 
!                LES CARACTERES PASSES VIA DIRECTIVES.

      SUBROUTINE HOLACAR(LABEL, LIST, NL, STRING, NC)
  
      IMPLICIT   NONE 
      INTEGER    NL, LIST(NL), STRING(NL*3), NC
      CHARACTER*(*) LABEL(NL)
!
!AUTEUR   -   Y. BOURASSA  - AVR 91
!REVISION 001 "      "     - JAN 92 
!Revision 002   M. Lepine - mars 98 - extensions pour fstd98
!Revision 003   M. Lepine - Nov  05 - remplacement de fstabt par qqexit
!LANGAGE  - FTN77
!
!ARGUMENTS
!SORTIE   - LABEL   - ETIKETTES 
!ENTREE   - LIST    - CHAMP RETOURNEE PAR ARGDOPE.
!   "     - NL      - DIMENSION DE LABEL ET LIST.
!   "     - STRING  - CHAINE DE CARACTHERES A DECODER.
!   "     - NC      - NOMBRE DE CARACTERES ALLOUE POUR LABEL. (on suppose <=12 dans le code)
!
      EXTERNAL qqexit
      INTEGER  NCW, I, J, K, L, M
      character *12 temp12
      INTEGER  RSHIFT, X, Y

      RSHIFT(X, Y) = ishft(X, -(Y))
      NCW = 4

!     PASSE DE HOLLERITH A CARACTERES
      DO 10 K=1,NL
         L = RSHIFT(LIST(K), 16)             ! position du debut d'extraction dans string
         I = IAND(255, RSHIFT(LIST(K), 8))   ! nombre de caracteres par entier
         IF(I .GT. NC) THEN
            WRITE(6,*)' LA VARIABLES LIMITEES A',NC,' CARACTERES'
            CALL qqexit(40)
         ENDIF
         IF(IAND(255, LIST(K)) .NE. 3) THEN  ! ce ne sont pas des caracteres qui cont dans string
            WRITE(6,*)' ARGUMENT MAL PASSE'
            CALL qqexit(41)
         ENDIF
         J = I/NCW
         IF(J*NCW .LT. I) J = J+1             ! nombre de mots a convertir
         M = L+J-1
         WRITE(LABEL(K), '(3A4)') (STRING(J),J=L,M)
   10    CONTINUE 

      RETURN
      END 




















