!
!**S/P GRISTEREO  CALCUL LATITUDE LONGITUDE DE CHAQUE PT D'UNE GRILLE P.S.
!
   subroutine gristereo(nni,nnj,d60,dgrw,clat,clon)
      use app
      implicit none
!
!AUTEUR   - P. SARRAZIN JANVIER 87 DRPN DORVAL P.Q. CANADA
!
!LANGAGE - RATFOR
!
!OBJET(GRISTEREO)
!          CALCULER LA LATITUDE ET LA LONGITUDE DE TOUS LES POINTS
!          DE LA GRILLE DE SORTIE STEREOGRAPHIQUE GENERALISEE
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
       real pi,pj,d60,dgrw,pp1,pp2,pp3,pp4,clat,clon
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
       cgrtyp='T'

       call cxgaig(cgrtyp,lg1,lg2,lg3,lg4,d60,dgrw,clat,clon)
       call cigaxg(cgrtyp,pp1,pp2,pp3,pp4,lg1,lg2,lg3,lg4)

      gdout = ezqkdef(li,lj,cgrtyp,lg1,lg2,lg3,lg4,0)
      ier = gdll(gdout, tmplat, tmplon)
!
!
!
      return
      end
