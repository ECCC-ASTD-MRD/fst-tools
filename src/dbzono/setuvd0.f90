!S/R LITZON
!
      SUBROUTINE SETUVD0 (SE, STAGE, S, NK, KA) 
!
      IMPLICIT NONE 
      INTEGER NK, KA
      REAL SE(NK), S(NK)
      LOGICAL STAGE 
! 
      INTEGER K
      LOGICAL INIT
      DATA INIT /.TRUE./
      SAVE INIT 
! 
!Author G.PELLERIN -aou93
!
!Revision  none   
!
!Object 
!          to calculate the centered sigma levels
!
!Arguments
!
!          - Output -
! SE       sigma levels for ('E')
!
!          - Input -
! STAGE    .TRUE. for 'E' (staggered) level intervals
! S        sigma levels
! NK       vertical dimension
! KA       level to extract
!
!
      IF (STAGE) THEN 

         DO 1 K=1,KA-1
1           SE(K)=(S(K)+S(K+1))/2 
         DO 4 K=KA,NK 
4           SE(K)=1 
      ELSE

         DO 2 K=1,NK
2           SE(K)=S(K)
      ENDIF 

      IF (INIT) THEN

      PRINT *,' S/R SETUVDO...STAGE= ',STAGE
      PRINT '(3A10/(I10,2F10.6))' , 'K','S(K)','SE(K)',(K,S(K),SE(K),K=1,NK) 
      PRINT *,' KA=',KA,' NK=',NK 
      IF (KA.LT.NK) PRINT *,'  '
      IF (KA.LT.NK) PRINT *,' ***************************'
      IF (KA.LT.NK) PRINT *,' * ANEMOMETRE 5M INACTIF   *'
      IF (KA.LT.NK) PRINT *,' ***************************'
      IF (KA.LT.NK) PRINT *,'  '
      PRINT *,' RAPPORT DE CENTRAGE DU NIVEAU KA' 
      PRINT *,' (1-S(KA))/(1-S(KA-1))=',(1-S(KA))/(1-S(KA-1)) 
      INIT = .FALSE.
      ENDIF 

      RETURN
      END 
