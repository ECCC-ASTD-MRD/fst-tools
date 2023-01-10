!
!**S/P  VERLALO  VERIFI LONGITUDE ET LATITUDE
!     
   subroutine verlalo(clat,clon,nombre)
      use app
      implicit none
      
      integer nombre
      real clat(nombre),clon(nombre)
!
!AUTEUR P. SARRAZIN DORVAL QUEBEC CANADA (DRPN)
!
!OBJET(VERLALO)
!         VERLALO - VERIFIER LES LATITUDES ET LONGITUDES DU CHAMP
!
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!ARGUMENTS
!   IN    CLAT   - CHAMP DE LATITUDE
!   IN    CLON   - CHAMP DE LONGITUDES
!   IN    NOMBRE - NOMBRE DE POINTS DANS LES CHAMPS CLAT/CLON
!
!-------------------------------------------------------------------- 
      external pgsmabt,messags
!
!
#include "voir.cdk90"
!
!
      integer i
!
!
!   VERIFIER SI LATITUDES ET LONGITUDES SONT A L'INTERIEUR
!   DES LIMITES
!
       do i=1,nombre
          if (clon(i).lt.0.0) clon(i)=clon(i)+360.0
          if (clon(i).ge.360.0) clon(i)=clon(i) - 360.0
!
          if (clat(i).lt.-90.005.or.clat(i).gt.90.005) then
            write(app_msg,*)'verlalo: Wrong latitude: ',clat(i)
            call app_log(APP_ERROR,app_msg)
          endif
          clat(i)=max(-90.0,min(90.0,clat(i)))
       enddo
       return
       end
