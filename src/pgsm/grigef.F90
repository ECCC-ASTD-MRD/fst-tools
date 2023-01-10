!
!**S/P GRIGEF   CALCUL LAT LONG DE CHAQUE PT D'UNE GRILLE "E"
!
   subroutine grigef(it,nni,nnj,xlat1,xlon1,xlat2,xlon2)
      use app
      implicit none
!
!AUTEUR   - y. chartier april 94
!
!LANGAGE - RATFOR
!
!     OBJET(GRISTDB)
!     CALCULER LA LATITUDE ET LA LONGITUDE DE TOUS LES POINTS
!     DE LA GRILLE DE SORTIE STANDARD 'e' LAT ET LONG EQUIDISTANT
!     LONGITUDE ZERO ET 360 PRESENT.
!
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
      integer nni,nnj,it,ier
      real xlat1,xlon1,xlat2,xlon2
      real xla0,xlo0,dlat,dlon,valeur
!
      li=nni
      lj=nnj
!
!
!     RESERVER MEMOIR POUR LATITUDE ET LONGITUDE
!
      allocate(tmplat(nni,nnj))
      allocate(tmplon(nni,nnj))
      allocate(tmplatg(nni,nnj))
      allocate(tmplong(nni,nnj))
!
      cgrtyp='E'
      call cxgaig(cgrtyp,lg1,lg2,lg3,lg4,xlat1,xlon1,xlat2,xlon2)
      gdout = ezqkdef(li,lj,cgrtyp,lg1,lg2,lg3,lg4,0)
      ier = gdll(gdout, tmplat, tmplon)

      return
      end


