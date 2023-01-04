!
!**   S/P GRIGRIB  CALCUL LATITUDE LONGITUDE DE CHAQUE PT D'UNE GRILLE GRIB
!
   subroutine grigrib(ig1,ig2,ig3,ig4)
      use app
      implicit none
!
!     AUTEUR   -  Y. CHARTIER DRPN DORVAL MAI 1996
!
!
!     LANGAGE - RATFOR
!
!     OBJET(GRIGRIB)
!     CALCULER LA LATITUDE ET LA LONGITUDE DE TOUS LES POINTS
!     DE LA GRILLE DE SORTIE POLAIRE STEREOGRAPHIQUE
!
!
!     LIBRAIRIES
!     -SOURCE  ARMNSRC,DRPN
!     -OBJET   PGSMLIB,ID=ARMNPJS.
!
!------------------------------------------------------
      external memoir,pgsmabt,grps,cigaxg,cxgaig,messags
!
!
#include "llccmm.cdk90"
#include "grilles.cdk90"
!
      character*1 gtyout
      real xg(20)
      integer nni,nnj,ihm,hem,ier,npts
      integer ig1,ig2,ig3,ig4
      integer tmpig1, tmpig2, tmpig3, tmpig4
      external ezqkdef, gdll
      integer ezqkdef, gdll

      real, dimension(:,:), allocatable :: x, y
!
!     RESERVER MEMOIR POUR LATITUDE ET LONGITUDE
!

      cgrtyp = '!'
      call igaxg95(gtyout,xg,15,cgrtyp,ig1,ig2,ig3,ig4)
      if (gtyout.eq.'H') then
         nni = nint(xg(8))
         nnj = nint(xg(9))
      endif

      allocate(tmplat(nni,nnj))
      allocate(tmplon(nni,nnj))
      allocate(tmplon(nni,nnj))
      allocate(x(nni,nnj))
      allocate(y(nni,nnj))
!
!
      li=nni
      lj=nnj
      lg1 = ig1
      lg2 = ig2
      lg3 = ig3
      lg4 = ig4
!
!
      npts = nni*nnj
      call ez_llflamb(tmplat,tmplon,x,y,npts,cgrtyp,ig1,ig2,ig3,ig4)
!
!      call cxgaig('L', tmpig1, tmpig2, tmpig3, tmpig4,0.,0.,1.0,1.0)
!      gdout = ezgdef(npts,1,'Y','L', tmpig1, tmpig2, tmpig3, tmpig4,
!     $        tmplon,tmplat)

      gdout = ezqkdef(nni,nnj,cgrtyp,ig1,ig2,ig3,ig4,1)
      ier = gdll(gdout, tmplat, tmplon)

      deallocate(x)
      deallocate(y)

      return
      end


