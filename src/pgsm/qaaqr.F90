!     
!**S/P QAAQR   CALCUL TOURBILLON RELATIF
!
      subroutine qaaqr(qaqr, li, lj, xlat) 
   implicit none
!
!AUTEUR  P. SARRAZIN DORVAL QUEBEC JUIN 83 DRPN
!
!LANGAGE RATFOR
!
!OBJET(QAAQR)
!            CALCUL LE CORIOLIS PARAMETER POUR CHAQUE POINT DE LA GRILLE
!            SOUSTRAIRE CE CHAMP DU CHAMP DU TOURBILLON ABSOLU ON GENERE
!            UN TOURBILONN RELATIF
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!ARGUMENTS
!  IN OUT QAQR  CHAMP CONTENANT TOURBILLON ABSOLU 
!  IN     LI    NOMBRE DE POINTS SUR UNE RANGEE DU CHAMP QAQR
!  IN     LJ    NOMBRE DE POINTS DANS UNE COLONNE DU CHAMP QAQR
!  IN     XLAT  LATITUDE POUR CHAQUE POINT DU CHAMP QAQR
!
!APPEL
!         -VIA ROUTINE SCALAIR
!         CALL QAAQR(QAQR, LI, LJ, XLAT)
!
!MESSAGES 
!          -AUCUN
!
!IMPLICITES
!         - AUCUN
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!
!
      integer i,j,li,lj
      real xlat(li,lj),qaqr(li,lj),degarad,omega2
!
!
!  rotation de la terre 7.292*1.e-5

      degarad = 3.1415926535/180.
      omega2= 2*7.292*1.e-5  
!
!
      do j= 1,lj
         do i = 1,li
            qaqr(i,j) = qaqr(i,j) - omega2*sin(xlat(i,j)*degarad)
         enddo
      enddo
!
      return 
      end
      
