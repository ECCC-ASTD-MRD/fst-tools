!** S/R DMPDES MET EDITFST EN MODE XPRES SI L'USAGER VEUT TOUT LE FICHIER
!       IMPRIME L'INTERPRETATION DES DIRECTIVES DESIRE SI EN MODE DEBUG
      SUBROUTINE DMPDES
      use ISO_C_BINDING
      use configuration
      use app
      IMPLICIT NONE

      include 'excdes.inc'
  
!     AUTEUR - Y. R. BOURASSA AVR 86
!Revision 002   M. Lepine - mars 98  - extensions pour fstd98
!Revision 003   M.Valin   - mai 2014 - utiliser les fonctions des fichiers standard pour
!                                      l'impression des directives
!     LANGUAGE FTN77
!*  
      INTEGER I, J, K, L
  

      ESAIS = .TRUE.   !   ON  NOTE UNE TENTATIVE DE COPIE
      DM1   = .TRUE.
  
!     AUCUNE DIRECTIVE ?
      IF(NREQ .EQ. 0) THEN  ! NREQ = no de la directive courante
         XPRES = .NOT.SCRI  ! si pas de criteres supplementaires, on veut vraiment tout
         call app_log(APP_DEBUG,'dmpdes: Copying whole file')
         RETURN
      ENDIF
  
!     IMPRESSION DES REQUETES
!     appeler la routine appropriee des fichiers standard pour ce faire (si DEBUG)
      IF( DEBUG ) call Dump_Request_Table  
      return
!     il faudrait idealement pouvoir demander aux fichiers standard s'il y a des directives en vigueur
      END 
