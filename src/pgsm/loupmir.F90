!
      subroutine loupmir(sortie,entre,nombre)
   implicit none
!
!AUTEUR P. SARRAZIN DORVAL QUEBEC CANADA (DRPN)
!
!OBJET(LOUPMIR)
!         LOUPMIR - TRANSFER LA MOITIE DU CHAMP DEFINI DANS L'AUTRE
!                   MOITIE RENVERSER COMME UN MIROIR
!
!         LOUPTRA - TRANSFER UN CHAMP DANS UN AUTRE
!
!         LOUPIN1 - INITIALISE UN CHAMP AVEC LA VALEUR 1
!
!         LOUPSOU - SOUSTRAIRE DEUX CHAMPS
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!ARGUMENTS
!   OUT   SORTIE - RESULTAT DE L'OPERATION SUR CHAMP(NOMBRE)
!   IN    ENTRE  - CHAMP QUI PEUT SERVIR AU CALCUL
!   IN    NOMBRE - NOMBRE DE POINTS DANS LE CHAMP EXCEPTE LOUPTRA OU
!                  NOMBRE EST (MAXIMUM) LA MOITIE DU NOMBRE DES POINTS
!
!APPEL
!     - VIA GRILLE,COUPZM,LIREN,EPAISUR 
!     - CALL LOUPMIR(SORTIE,ENTRE,NOMBRE)
!     - CALL LOUPTRA(SORTIE,ENTRE,NOMBRE)
!     - CALL LOUPIN1(SORTIE,ENTRE,NOMBRE)
!     - CALL LOUPSOU(SORTIE,ENTRE,NOMBRE)
!
!--------------------------------------------------------------------------
!
#include "voir.cdk90"
!
!
!
      real sortie(1),entre(1)
      integer nombre,nombpl1,i
!     
!            TRANSFER LA MOITIE DU CHAMP DANS L'AUTRE
!
!
      nombpl1=nombre + 1
!
      do i=1,nombre
         sortie(i + nombre) = entre(nombpl1 - i)
      enddo
!     
      return
!     
!------------------------------------------------------------
!
      entry louptra(sortie,entre,nombre)
!
!
!              TRANSFER CHAMP 
!
      do i=1,nombre
         sortie(i)=entre(i)
      enddo
!     
      return
!-------------------------------------------------------------
!
       entry loupin1(sortie,entre,nombre)
!
!
!           INITIALISE LE CHAMP SORTIE AVEC 1.0
!
       do i=1,nombre
          sortie(i)=1.0
       enddo
!     
       return
!-----------------------------------------------------------
!
       entry loupsou(sortie,entre,nombre)
!
!
!           SOUSTRAIT DEUX CHAMPS
!
       do i=1,nombre
          sortie(i)=entre(i) - sortie(i)
       enddo
!     
       return
       end
