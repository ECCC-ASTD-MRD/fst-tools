!
!**S/P GRISTDB   CALCUL LAT LONG DE CHAQUE PT D'UNE GRILLE STD "B"
!
   subroutine gristdb(nni,nnj,hem)
      use app
      implicit none
!
!AUTEUR   - P. SARRAZIN JANVIER 87 DRPN DORVAL P.Q. CANADA
!
!LANGAGE - RATFOR
!
!OBJET(GRISTDB)
!          CALCULER LA LATITUDE ET LA LONGITUDE DE TOUS LES POINTS
!          DE LA GRILLE DE SORTIE STANDARD 'B' LAT ET LONG EQUIDISTANT
!          LONGITUDE ZERO ET 360 PRESENT.
!
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!------------------------------------------------------
      external memoir,pgsmabt,grll,lastcol,messags
      external ezqkdef, gdll
      integer ezqkdef, gdll
!
!
#include "llccmm.cdk90"
!
!
#include "grilles.cdk90"
!
!
      integer nni,nnj,hem,ier
      real xla0,xlo0,dlat,dlon,valeur
!
      li=nni
      lj=nnj
!
!
!   RESERVER MEMOIR POUR LATITUDE ET LONGITUDE
!
      allocate(tmplat(nni,nnj))
      allocate(tmplon(nni,nnj))
!
      lg1=hem
      cgrtyp='B'
      lg2=0
      lg3=0
      lg4=0
!
      if (lg1.lt.0 .or. lg1.gt.2) then
         call app_log(APP_ERROR,'gristdb: GRILLE(STDB... must be GLOBAL,NORD,SUD ')
         call pgsmabt
      endif
!

      gdout = ezqkdef(li,lj,cgrtyp,lg1,lg2,lg3,lg4,0)
      ier = gdll(gdout, tmplat, tmplon)
!
!
      return
      end
