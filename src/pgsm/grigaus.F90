!
!**S/P GRIGAUS   CALCUL LAT LONG DE CHAQUE PT D'UNE GRILLE GAUSSIENNE
!
   subroutine grigaus(nni,nnj,nhem)
      use app
      implicit none
!
!AUTEUR   - P. SARRAZIN JANVIER 87 DRPN DORVAL P.Q. CANADA
!
!LANGAGE - RATFOR
!
!OBJET(GRIGAUS)
!          CALCULER LA LATITUDE ET LA LONGITUDE DE TOUS LES POINTS
!          DE LA GRILLE DE SORTIE GAUSSIENNE LONGITUDE EQUIDISTANTE
!          LATITUDE GAUSSIENNE
!
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!------------------------------------------------------
!
      external memoir,pgsmabt,grgg,messags
      external ezqkdef, gdll
      integer ezqkdef, gdll
!
#include "grilles.cdk90"
!
!
#include "llccmm.cdk90"
!
!
!
      integer nni,nnj,nhem,ier,nroot
!
!   RESERVER MEMOIR POUR LATITUDE ET LONGITUDE
!
      allocate(tmplat(nni,nnj))
      allocate(tmplon(nni,nnj))
!
      lg1=nhem
!
      li=nni
      lj=nnj
!
      if (lg1.lt.0 .or.lg1.gt.2) then
         call app_log(APP_ERROR,'grigaus: GRILLE(GAUSS... L11 must be GLOBAL,NORD,SUD ')
         call pgsmabt
      endif
!
      cgrtyp='G'
      lg2=0
      lg3=0
      lg4=0
!
      if (lg1.eq.0) then
         nroot=nnj
      else
         nroot=nnj*2
      endif

!     BUFL(IROOT)  -TABLE DE RACINES DES POLYNOMES DE LEGENDRE
!     UTILISER PAR GRIGAUS POUR LE CALCUL DES LATITUDES
!     LONGITUDES DANS GRGG ROUTINE
!
      allocate(tmproot(nroot))

      tmproot(1)= 100.0
      gdout = ezqkdef(li,lj,cgrtyp,lg1,lg2,lg3,lg4,0)
      ier = gdll(gdout, tmplat, tmplon)

!
      deallocate(tmproot)
!
      return
      end
