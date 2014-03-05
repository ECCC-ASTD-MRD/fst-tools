!** S/R DMPDES MET EDITFST EN MODE XPRES SI L'USAGER VEUT TOUT LE FICHIER
!       IMPRIME L'INTERPRETATION DES DIRECTIVES DESIRE SI EN MODE DEBUG
      SUBROUTINE DMPDES
      use configuration
      IMPLICIT NONE 
  
!     AUTEUR - Y. R. BOURASSA AVR 86
!Revision 002   M. Lepine - mars 98 - extensions pour fstd98  
!     LANGUAGE FTN77
!#include "maxprms.cdk"
!#include "logiq.cdk"
!#include "desrs.cdk"
!#include "fiches.cdk"
!#include "char.cdk"
  
      INTEGER I, J, K, L
  
!     NOTE UNE TENTATIVE DE COPIE
      ESAIS = .TRUE.
      DM1   = .TRUE.
  
!     AUCUNE DIRECTIVE ?
      IF(NREQ .EQ. 0) THEN
         XPRES = .NOT.SCRI
         IF( DEBUG ) WRITE(6,*)' ON DEMANDE TOUT LE FICHIER '
         RETURN
      ENDIF
  
!     ANALYSE DES REQUETES
      L = 0
      DO 20 J=4,1,-1
         DO 10 K=1,NREQ
            IF(REQ(11,J,K) .GT. 0) THEN 
!              LISTE DE PARAMETRES
               L = 1
               IF( DEBUG ) THEN
                  IF(J.EQ.4) WRITE(6,601) (REQ(I,J,K),I=1,REQ(11,J,K))
                  IF(J.EQ.1) WRITE(6,602) (REQ(I,J,K),I=1,REQ(11,J,K))
                  IF(J.EQ.2) WRITE(6,606) (REQ(I,J,K),I=1,REQ(11,J,K))
                  IF(J.EQ.3) WRITE(6,607) (REQ(I,J,K),I=1,REQ(11,J,K))
               ENDIF
            ELSEIF(REQ(11,J,K) .EQ. -1)THEN
!              INTERVALE AVEC SAUT.
               L = 1
               IF( DEBUG ) THEN
                  IF(J.EQ.4) WRITE(6,611) (REQ(I,J,K),I=1,3)
                  IF(J.EQ.1) WRITE(6,612) (REQ(I,J,K),I=1,3)
                  IF(J.EQ.2) WRITE(6,616) (REQ(I,J,K),I=1,3)
                  IF(J.EQ.3) WRITE(6,617) (REQ(I,J,K),I=1,3)
               ENDIF
            ENDIF
!           SI(J.EQ.4 ET L.EQ.1) LES DATES NE SONT PAS TOUTES =-1
            IF(J.EQ.4 .AND. L.EQ.1) DM1 = .FALSE. 
   10       CONTINUE
   20    CONTINUE

      DO 30 K=1,NREQ
    
         IF(REQN(K) .GT. 0) THEN
!           LISTE DE PARAMETRES NOM
            L = 1
            IF( DEBUG ) WRITE(6,603) (NOMS(I,K),I=1,REQN(K))
         ENDIF
         
         IF(REQT(K) .GT. 0) THEN
!           LISTE DE PARAMETRES TYPVAR
            L = 1
            IF( DEBUG ) WRITE(6,604) (TYPS(I,K),I=1,REQT(K))
         ENDIF
         
         IF(REQE(K) .GT. 0) THEN
!           LISTE DE PARAMETRES ETIKETTES
            L = 1
            IF( DEBUG ) WRITE(6,605) (ETIS(I,K),I=1,REQE(K))
         ENDIF
   30    CONTINUE

      XPRES = (L .EQ. 0) .AND. .NOT.SCRI
      IF(XPRES .AND. DEBUG) PRINT*,' ON DEMANDE TOUT LE FICHIER '
      RETURN

  601 FORMAT(' JULSEC = ',10I12)
  602 FORMAT(' IP1    = ',10I7)
  603 FORMAT(' NOMVAR = ',10(1X,A4))
  604 FORMAT(' TYPVAR = ',10(1X,A2))
  605 FORMAT(' ETIKET = ',10(1X,A12))
  606 FORMAT(' IP2    = ',10I7)
  607 FORMAT(' IP3    = ',10I7)
  611 FORMAT(' JULSEC = ',I12,' @ ',I12,' DELTA ',I12)
  612 FORMAT(' IP1    = ',I8,' @ ',I8,' DELTA ',I4)
  616 FORMAT(' IP2    = ',I8,' @ ',I8,' DELTA ',I4)
  617 FORMAT(' IP3    = ',I8,' @ ',I8,' DELTA ',I4)
      END 
