*** S/P BSORT  TRILLER DES REELS EN ORDRE DECROISSANT
* 
      SUBROUTINE BSORT(NIVEAU,NOMBRE) 

      IMPLICIT NONE
      INTEGER NOMBRE
      REAL    NIVEAU(NOMBRE)
*AUTEUR G.PELLERIN CMC DORVAL P.Q. CANADA - SEP93
* 
*LANGUAGE FTN77
* 
*ARGUMENTS
* IN/OUT  NIVEAU -CONTIENT LES NIVEAUX A METTRE EN ORDRE DECROISSANT
* IN/OUT  NOMBRE -NOMBRE DE REELS EN ORDRE DECROISSANT
* 
  
      INTEGER I, J
  
      REAL  X 
  
      IF(NOMBRE .GT. 1) THEN
  
         DO 10 I = 1, NOMBRE
            DO 10 J = (I+1), NOMBRE
               IF(NIVEAU(I) .LT. NIVEAU(J))  THEN 
                  X         = NIVEAU(J) 
                  NIVEAU(J) = NIVEAU(I) 
                  NIVEAU(I) = X 
               ENDIF
   10          CONTINUE 
  
*        ELIMINATION DES NOMBRES EGAUX DE LA LISTE
         I = 1
         DO 20 J=2,NOMBRE 
            IF(NIVEAU(I) .NE. NIVEAU(J)) THEN 
               I = I+1
               IF(I .NE. J) NIVEAU(I) = NIVEAU(J) 
            ENDIF 
   20       CONTINUE
  
         NOMBRE = I 
      ENDIF 
  
      RETURN
      END 
