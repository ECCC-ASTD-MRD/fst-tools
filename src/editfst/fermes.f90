!** S/R FERMES FERME  LES FICHIERS SOURCES
      SUBROUTINE FERMES
      use rmn_fst24
      use configuration
      IMPLICIT      NONE
!
!AUTEUR
!VERSION ORIGINALE  - Y. BOURASSA NOV 90
!REVISION 001         "      "    MAR 92 VARIABLE NFSO (NOMBRE DE SOURCE OUVERTS)
!                                        CHANGE ALLEL A FATVOI
!         002         "      "    MAI 92 FCLOS SUB.>FUNCTION.
!         003         M.Valin     FEV 14 mode DRYRUN / remplacement des comdecks par un module
!     LANGUAGE FTN77

      integer :: I, J
      logical :: success
  
!     TRAITEMENT DES FICHIERS SOURCES
      IF( OUVS ) THEN
         IF(NFSO .GT. 1) success = sources(1)%unlink()
         DO 10 J=1,NFSO
            success = sources(j)%close()
   10       CONTINUE
         OUVS = .FALSE.
         NFSO = 0
      ENDIF
      RETURN
      END
  
!** S/R FERMED FERME  LE FICHIER DESTINATION
      SUBROUTINE FERMED
      use rmn_fst24
      use configuration
      IMPLICIT      NONE
!AUTEUR
!VERSION ORIGINALE  - Y. BOURASSA NOV 90
!REVISION 001         "      "    MAR 92 VARIABLE NFSO (NOMBRE DE SOURCE OUVERTS)
!                                        CHANGE ALLEL A FATVOI
!         002         "      "    MAI 92 FCLOS SUB.>FUNCTION.
!         003         M.Valin     FEV 14 mode DRYRUN / remplacement des comdecks par un module

      integer :: I
      logical :: success

!     TRAITEMENT DU FICHIER DESTINATION 
      if(dryrun) then  ! dry run, on ne fait rien
        OUVD = .FALSE.
        return
      endif
      IF( OUVD ) THEN    ! fichier destination ouvert
         IF( VD ) THEN   ! voir contenu du fichier destination
            IF( DSEQ ) I = destination%rewind()  ! fichier sequentiel, on rembobine
            call destination%print_summary()
         ENDIF
         success = destination%close()

         OUVD = .FALSE.
      ENDIF
      RETURN
  
      END 
