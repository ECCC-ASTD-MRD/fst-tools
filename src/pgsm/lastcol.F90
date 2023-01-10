!
      subroutine lastcol(sortie,valeur,istart,ifini,incre) 
   implicit none
!
!AUTEUR P. SARRAZIN DORVAL QUEBEC CANADA (DRPN)
!
!OBJET(LASTCOL)
!         LASTCOL - LA DERNIERE COLONNE DU CHAMP PREND LA VALEUR(VALEUR)
!         LOUPNEG - ELIMINE VALEUR NEGATIVE DANS LE CHAMP SI VALEUR=0.0
!
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!ARGUMENTS
!   OUT   SORTIE - RESULTAT DE L'OPERATION SUR CHAMP(NOMBRE)
!   IN    VALEUR - VALEUR DE LA DERNIERE COLONNE DU CHAMP
!                  OU VALEUR MINIMAL DU CHAMP A GARDE
!   IN    ISTART - PREMIERE INDEX DU CHAMP
!   IN    IFINI  - DERNIERE INDICE DU CHAMP
!   IN    INCRE  - NOMBRE DE PTS AUGMENTATION DE L'INDICE ISTART
!
!                  NOMBRE EST (MAXIMUM) LA MOITIE DU NOMBRE DES POINTS
!
!APPEL
!     - VIA GRILLE,MACPCP
!     - CALL LASTCOL(SORTIE,VALEUR,ISTART,IFINI,INCRE)
!     - CALL LOUPNEG(SORTIE,VALEUR,ISTART,IFINI,INCRE)
!
!--------------------------------------------------------------------------
!
#include "voir.cdk90"
!
!
      real sortie(1),valeur
      integer istart,ifini,incre,i
!     
!         INITIALISE LA DERNIERE COLONNE DU CHAMP 
!
      do i=istart,ifini,incre
         sortie(i)=valeur
      enddo
      return
!
!----------------------------------------------------------------------
!
      entry loupneg(sortie,valeur,istart,ifini,incre)
!
!              ELIMINE VALEUR NEGATIVE
!
      do i=istart,ifini,incre
         sortie(i)=amax1(sortie(i),valeur)
      enddo
!     
      return
      end
      
      
