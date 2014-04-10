!** S/R FERMES FERME  LES FICHIERS SOURCES
      SUBROUTINE FERMES
      use configuration
      IMPLICIT      NONE
!
!AUTEUR
!VERSION ORIGINALE  - Y. BOURASSA NOV 90
!REVISION 001         "      "    MAR 92 VARIABLE NFSO (NOMBRE DE SOURCE OUVERTS)
!                                        CHANGE ALLEL A FATVOI
!         002         "      "    MAI 92 FCLOS SUB.>FUNCTION.
!         003         "      "    FEV 14 mode DRYRUN
!     LANGUAGE FTN77
!
!#include "maxprms.cdk"
!#include "desrs.cdk"
!#include "key.cdk"
!#include "char.cdk"
!#include "tapes.cdk"
!#include "fiches.cdk"
!#include "logiq.cdk"
!
!*
      INTEGER, external :: FSTVOI, FSTFRM, FSTRWD, FSTUNL, FSTOPC, FCLOS
      integer :: I, J
      CHARACTER(len=128) :: DN
  
!     TRAITEMENT DES FICHIERS SOURCES
      IF( OUVS ) THEN
         IF(NFSO .GT. 1) I = FSTUNL( )
         DO 10 J=1,NFSO
            I = FSTFRM( SOURCES(J) )
            I = FCLOS(  SOURCES(J) )
   10       CONTINUE
         OUVS = .FALSE.
         NFSO = 0
      ENDIF
      RETURN
      END
  
!** S/R FERMED FERME  LE FICHIER DESTINATION
      SUBROUTINE FERMED
      use configuration
      IMPLICIT      NONE
!AUTEUR
!VERSION ORIGINALE  - Y. BOURASSA NOV 90
!REVISION 001         "      "    MAR 92 VARIABLE NFSO (NOMBRE DE SOURCE OUVERTS)
!                                        CHANGE ALLEL A FATVOI
!         002         "      "    MAI 92 FCLOS SUB.>FUNCTION.
!         003         "      "    FEV 14 mode DRYRUN
!*
      INTEGER, external :: FSTVOI, FSTFRM, FSTRWD, FSTUNL, FSTOPC, FCLOS
      integer :: I
      CHARACTER(len=128) :: DN
!     TRAITEMENT DU FICHIER DESTINATION 
      if(dryrun) then  ! dry run, on ne fait rien
        OUVD = .FALSE.
        return
      endif
      DN = ND
      IF( OUVD ) THEN    ! fichier destination ouvert
         IF( VD ) THEN   ! voir contenu du fichier destination
            I = FSTOPC('MSGLVL', 'INFORM', .FALSE.)
            IF( DSEQ ) I = FSTRWD(3)  ! fichier sequentiel, on rembobine
            IF( INDEX(DNOM,'FTN') .NE. 0) THEN
               I = FSTVOI(3, 'SEQ')
            ELSE
               I = FSTVOI(3, 'STD')
            ENDIF
            I = FSTOPC('MSGLVL', DEF1b(13), .FALSE.)
         ENDIF
         I = FSTFRM( 3 )
         I = FCLOS(  3 )
         OUVD = .FALSE.
      ENDIF
      RETURN
  
      END 
