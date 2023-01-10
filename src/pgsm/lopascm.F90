!
   subroutine lopascm(sortent,entre,fact,nombre)
      use app
      implicit none
!
!AUTEUR P. SARRAZIN DORVAL QUEBEC CANADA (DRPN)
!
!OBJET(LOPASCM)
!         LOPASCM - ADDITIONNE,SOUSTRAIT,MULTIPLI DEUX CHAMPS OU SOMME
!                   DEUX CHAMPS DONT CHAQUE PT EST AU CARRE 
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!ARGUMENTS
! IN-OUT  SORTENT - RESULTAT DE L'OPERATION ET ENTRE DU CHAMP
!   IN    ENTRE   - DEUXIEME CHAMP POUR OPERATION 
!   IN    FACT    - OPERATEUR 1=ADDITIONNE , -1=SOUSTRAIT 3=MULTIPLI
!                             2=ADDITIONNE CHAQUE POINT AU CARRE
!   IN    NOMBRE  - NOMBRE DE POINTS DANS LES CHAMPS SORTENT/ENTRE
!
!APPEL
!     - VIA PLMNMOD 
!      CALL LOPASCM(SORTENT,ENTRE,FACT,NOMBRE)
!
!----------------------------------------------------------------------
!
#include "voir.cdk90"
!
!
!
      integer nombre,i,fact
      real sortent(nombre),entre(nombre)
!     
!       SOUSTRAIT SI FACT=-1  ADDITIONNE SI FACT=1
!
      if (abs(fact).eq.1) then
         do i=1,nombre
            sortent(i)=sortent(i) + entre(i)*fact
         enddo
!     
!     ADDITIONNE CHAQUE POINT DES DEUX CHAMPS AU CARRE
!     
      else if (fact.eq.2)  then
         do i=1,nombre
            sortent(i)=sortent(i)**2 + entre(i)**2
         enddo
!     
!     MULTIPLIT CHAQUE POINT DES DEUX CHAMPS
!     
      else if (fact.eq.3) then
         do i=1,nombre
            sortent(i)=sortent(i)*entre(i)
         enddo
      else  if (fact.eq.4) then
         do i=1,nombre
            if (entre(i).eq.0.0) then
               call app_log(APP_ERROR,'lopascm: One of the array alement has a 0.0 value for a division')
               call pgsmabt
            endif
         enddo
         do i=1,nombre
            sortent(i)=sortent(i)/entre(i)
         enddo
      else
         call app_log(APP_ERROR,'lopascm: Invalid operation (fact)')
      endif
!     
      return
      end
      
