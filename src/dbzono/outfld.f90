!  S/R OUTFLD
!
      SUBROUTINE OUTFLD(nom,CHAMP,NX,NY)
      IMPLICIT NONE
!
!Auteur: G.Pellerin (FEV94)
!
!revision Aucune
!
!Language ftn77
!
!objet(OUTFLD)
!     calculer la moyenne par variable et par niveau
!
!modules
!
      INTEGER NX,NY,II,JJ
      REAL CHAMP(NX,NY)
      REAL SOMX,SOMX2
      CHARACTER nom *2

      SOMX2=0.
      DO JJ=1,NY
       SOMX=0.
       DO II=1,NX
       SOMX = SOMX + CHAMP(II,JJ)
       SOMX2= SOMX2+ CHAMP(II,JJ)
       END DO
!              moyenne sur bandes 
        if(NX.gt. 0) then
        somx=somx/nx
        print *,nom,' moyenne =' ,somx
        else
        print *,'outfld - un des arguments a 0'
        endif
      END DO
!              moyenne sur bandes et niveaux
      II=NX*NY 
      IF (II .GT.0 .and. ny.gt.1) then
        SOMX2 = SOMX2/II
        print *,'outfld - moyenne du champ =',SOMX2
      ENDIF

      RETURN
      END
