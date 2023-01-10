!
!**S/P GRLALON   CALCUL LATITUDE LONGITUDE DE CHAQUE PT D'UNE GRILLE LATLON
!
   subroutine grlalon(nni,nnj,p1,p2,p3,p4)
      use app
      implicit none
!
!AUTEUR   - P. SARRAZIN JANVIER 87 DRPN DORVAL P.Q. CANADA
!
!LANGAGE - RATFOR
!
!OBJET(GRLALON)
!          CALCULER LA LATITUDE ET LA LONGITUDE DE TOUS LES POINTS
!          DE LA GRILLE DE SORTIE LATLON INTERVAL REGULIER TYPE "L"
!
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!------------------------------------------------------
       external memoir,pgsmabt,grll,cigaxg,cxgaig,messags
!
!
#include "lires.cdk90"
!
!
#include "ecrires.cdk90"
!
!
#include "llccmm.cdk90"
!
!
#include "packin.cdk90"
!
!
#include "charac.cdk90"
!
!
#include "indptr.cdk90"
!
!
#include "grilles.cdk90"
!
!
#include "voir.cdk90"
!
       external ezqkdef, gdll
       integer ezqkdef, gdll

       integer nni,nnj,ier
       real p1,p2,p3,p4,pp1,pp2,pp3,pp4
!
       li=nni
       lj=nnj
!
!   RESERVER MEMOIR POUR LATITUDE ET LONGITUDE ET CALL ECRIRE (IWRK)
!
       allocate(tmplat(nni,nnj))
       allocate(tmplon(nni,nnj))
!
!
!   RECALCUL  LAT,LONG,DELTA LAT,DELTA LONG
!   MEME VALEUR A L'ENTRE COMME A LA SORTIE
!
!
       cgrtyp='L'
       call cxgaig(cgrtyp,lg1,lg2,lg3,lg4,p1,p2,p3,p4)
       call cigaxg(cgrtyp,pp1,pp2,pp3,pp4,lg1,lg2,lg3,lg4)
!
       gdout = ezqkdef(li,lj,cgrtyp,lg1,lg2,lg3,lg4,0)
       ier = gdll(gdout, tmplat, tmplon)
       return
       end
