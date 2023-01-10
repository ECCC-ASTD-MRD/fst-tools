!     
!**S/P CALCUL  MOYENNE ZONALE OU MERIDIONALE D UN CHAMP
!     
      subroutine calcul(entre, sortie, ni, nj, poids, ccoupe, cigtyp)
   implicit none
!
!AUTEUR 
!    P. SARRAZIN  DORVAL QUEBEC AVRIL 85 DRPN 
!REVISION 4.0.2
!   CONVERSION DES VARIABLES HOLLERITH EN CARACTERE
!   Y. CHARTIER AOUT 90 DRPN DORVAL QUEBEC. 
!
!LANGAGE RATFOR
!
!OBJET(CALCUL)
!            CALCUL MOYENNE ZONALE OU MERIDIONALE 
!            LA MOYENNE ZONALE(EST-OUEST) CONTIENT NJ POINTS
!            LA MOYENNE MERIDIONALE(NORD-SUD) CONTIENT NI POINTS
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!ARGUMENTS
!  IN   ENTRE  -CHAMP(NI,NJ) LUT PAR COUPZM ROUTINE
!  IN   SORTIE -CHAMP(NI) MERIDIONAL  CHAMP(NJ) ZONALE
!               CONTENANT MOYENNE ZONALE OU MERIDIONALE
!  IN   NI     -DIMENSION DU CHAMP ENTRE EST-OUEST
!  IN   NJ     -DIMENSION DU CHAMP ENTRE NORD-SUD 
!  IN   POIDS  -UTILISE POUR LE CALCUL DE LA MOYENNE MERIDIONALE D UN 
!               CHAMP GAUSSIEN MAX NJ
!  IN   JCOUP  -COUPE ZONALE JCOUP="ZON"
!               COUPE MERIDIONALE JCOUP="MER"
!  IN   IGTYP  -TYPE DE GRILLE SI IGTYP="B" ELIMINER DERNIERE LONG
!
!APPEL
!         -VIA ROUTINE COUPZM 
!          CALL CALCUL(ENTRE, SORTIE, NI, NJ, POIDS, JCOUP, IGTYP)
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
#include "voir.cdk90"
!
      integer i,ni,j,nj,nii
!
      real entre(ni,nj),sortie(1),poids(1),sum,sump
      character ccoupe*8, cigtyp*1
!
!     SI COUPE MERIDIONALE
!     
      if (ccoupe.eq.'MER ') then
!
         do i=1,ni  
            sum=0.0
            sump=0.0
            do j=1,nj  
               sum=sum + entre(i,j)*poids(j)
               sump = sump + poids(j)
               sortie(i)=sum/sump
            enddo
         enddo
!
!   COUPE ZONALE
!
      else
         nii=ni
!     
!     SI IGTYP='B' ON ELIMINE LA DERNIERE LONGITUDE 
!     
         if (cigtyp.eq.'B') then
            nii=ni-1
         endif
!     
         do j=1,nj  
            sum=0.0
            do i=1,nii
               sum=sum + entre(i,j)
!     
               sortie(j)=sum/nii
            enddo
         enddo
      endif
      return
      end
      
