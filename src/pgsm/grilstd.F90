!
!**S/P GRILSTD   CALCUL LATITUDE LONGITUDE DE CHAQUE PT D'UNE GRILLE STD
!
   subroutine grilstd(nni,nnj,hem)
      use app
      implicit none
!
!AUTEUR   - P. SARRAZIN JANVIER 87 DRPN DORVAL P.Q. CANADA
!
!LANGAGE - RATFOR
!
!OBJET(GRILSTD)
!          CALCULER LA LATITUDE ET LA LONGITUDE DE TOUS LES POINTS
!          DE LA GRILLE DE SORTIE STANDARD INTERVAL REGULIER MAIS DECALE
!          1/2 POINT DU POLE ET DE L'EQUATEUR
!
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!------------------------------------------------------
       external memoir,pgsmabt,grll,messags
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
       real xla0,xlo0,dlat,dlon
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
!

       lg1=hem
       cgrtyp='A'
       lg2=0
       lg3=0
       lg4=0

       gdout = ezqkdef(li,lj,cgrtyp,lg1,lg2,lg3,lg4,0)
       ier = gdll(gdout, tmplat, tmplon)
!
!
       if (lg1.ne.0.and.lg1.ne.1.and.lg1.ne.2) then
          call app_log(APP_ERROR,'griltd: GRILLE(STD... must be GLOBAL,NORD,SUD ')
          call pgsmabt
       endif
!
       return
       end

