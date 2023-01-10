!
!**S/P GRILLPS  CALCUL LATITUDE LONGITUDE DE CHAQUE PT D'UNE GRILLE P.S.
!
   subroutine grillps(nni,nnj,pi,pj,d60,dgrw,hem)
      use app
      implicit none
!
!AUTEUR   - P. SARRAZIN JANVIER 87 DRPN DORVAL P.Q. CANADA
!
!LANGAGE - RATFOR
!
!OBJET(GRILLPS)
!          CALCULER LA LATITUDE ET LA LONGITUDE DE TOUS LES POINTS
!          DE LA GRILLE DE SORTIE POLAIRE STEREOGRAPHIQUE
!
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!------------------------------------------------------
       external memoir,pgsmabt,grps,cigaxg,cxgaig,messags
!
!
#include "llccmm.cdk90"
!
!
#include "grilles.cdk90"
!
!
       external ezqkdef, gdll
       integer ezqkdef, gdll

       integer nni,nnj,ihm,hem,ier
       real pi,pj,d60,dgrw,pp1,pp2,pp3,pp4
!
!   RESERVER MEMOIR POUR LATITUDE ET LONGITUDE
!
       allocate(tmplat(nni,nnj))
       allocate(tmplon(nni,nnj))
!
!
       li=nni
       lj=nnj
!
!
       ihm=hem
       if (ihm.eq.1) then
          cgrtyp='N'
       else
          cgrtyp='S'
       endif

       call cxgaig(cgrtyp,lg1,lg2,lg3,lg4,pi,pj,d60,dgrw)
       call cigaxg(cgrtyp,pp1,pp2,pp3,pp4,lg1,lg2,lg3,lg4)

      if (ihm.lt.1.or.ihm.gt.2) then
         call app_log(APP_ERROR,'grillps: GRILLE(PS... must be NORD or SUD')
         call pgsmabt
      endif
!
      gdout = ezqkdef(li,lj,cgrtyp,lg1,lg2,lg3,lg4,0)
      ier = gdll(gdout, tmplat, tmplon)
!
!
!
      return
      end
