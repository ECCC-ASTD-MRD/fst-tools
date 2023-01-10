!     
!**   S/P  CONVER, PLMNMOD ECART AU CHAMP ET MULTIPLIER PAR FACTEUR
!     
!     AUTEUR P.SARRAZIN MAI 82 DRPN DORVAL QUEBEC CANADA
!     
      subroutine conver(z, ni, nj, cnom)
   implicit none
!     
!LANGAGE RATFOR
!
!OBJET(CONVER)
!          AUGMENTE UN CHAMP D UNE VALEUR UNIFORME (ECARTS) ET
!          MULTIPLIER PAR UN FACTEUR APPROPRIE ELIMINE LES VALEURS TROP PETITES 
!          OU TROP GRANDES PREDETERMINEES
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!ARGUMENTS
!  IN-OUT Z   - CHAMP(NI,NJ) QUI SERA MODIFIE
!  IN     NI  - NOMBRE DE PTS DANS LA DIRECTION EST-OUET
!  IN     NJ  - NOMBRE DE PTS DANS LA DIRECTION NORD-SUD
!  IN     NOM - NOM DE LA VARIABLE DU CHAMP A MODIFIER
!
!APPEL   VIA CALL
!        CALL CONVER(CHAMP,NI,NJ,NOM)  APPELLE DANS ECRITUR 
!
!  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
#include "voir.cdk90"
!
!
#include "convers.cdk90"
!
!
#include "dummys.cdk90"
!
      integer ni,nj,i,k,j
      real z(ni,nj)
      character*4 cnom
!     
      if ( nomb .eq. 0 ) return
      
      k = 0
      do i = 1,nomb
         if (cnom .eq. nomss(i)) then
            k = i
            goto 10
         endif
      enddo
 10   continue
      
      if ( k .eq. 0 ) return
!     
      do j=1,nj
         do i=1,ni
            z(i,j) = amax1(bass(k),amin1(hauts(k),(z(i,j) +             ecarts(k))*facts(k)))
         enddo
      enddo
!     
      return 
      end
