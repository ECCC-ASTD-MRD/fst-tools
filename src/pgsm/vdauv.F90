!
!**S/P VDAUV  U ET V DE DIRECTION ET VITESSE DU VENT
!
   subroutine vdauv(srtentu,srtentv,clong,dgtord,nombre)
      use app
      implicit none
      
      integer nombre
      real srtentu(nombre),srtentv(nombre),clong(nombre),dgtord
!
!AUTEUR P. SARRAZIN DORVAL QUEBEC CANADA (DRPN)
!
!OBJET(VDAUV)
!         AVEC LA DIRECTION ET LA VITESSE DU VENT CALCUL LES VECTEURS 
!         U ET V. UTILISATION DE LA LONGITUDE
!
!ARGUMENTS
! IN-OUT   SRTENTU - ENTRE DIRECTION   SORTI VECTEUR U
! IN-OUT   SRTENTV - ENTRE VITESSE  SORTI VECTEUR V
!   IN     CLONG   - CHAMP DE LONGITUDES EN DEGRE 0-360
!   IN     DGTORD  - FACTEUR DE CONVERSION DE DEGREE A RADIAN
!   IN     NOMBRE  - NOMBRE DE POINTS DANS LES DEUX CHAMPS
!
!APPEL
!     - VIA UVECTUR 
!     - CALL VDAUV(SRTENTU,SRTENTV,CLONG,DGTORD,NOMBRE)
!
#include "grilles.cdk90"
!
!
!----------------------------------------------------------------------
!
      external pgsmabt,messags
!
      real angle,u,v
      integer i
!
!    SI LE TYPE DE GRILLE  "L"
!
      if (cgtypxy.eq.'L') then
         do i=1,nombre
            angle=dgtord*(srtentv(i) - clong(i))
            u=srtentu(i)*sin(angle)
            v=-srtentu(i)*cos(angle)
            srtentu(i)=u
            srtentv(i)=v
         enddo
!     
!     SI LE TYPE DE GRILLE GRTYPXY= "N" HEM NORD
!     
      else if (cgtypxy.eq.'N') then
         do i=1,nombre
            angle=dgtord*(dgrwxy + srtentv(i))
            u=srtentu(i)*cos(angle)
            v=srtentu(i)*sin(angle)
            srtentu(i)=u
            srtentv(i)=v
         enddo
      else
        call app_log(APP_ERROR,'vdauv: GRILLE not "L" or "N", no valid code for type "s"')
        call pgsmabt
      endif
      return
      end 
      
