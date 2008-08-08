!     
!**S/P GRILTP4   LIRE LAT LONG DE CHAQUE PT D'UNE GRILLE TYPE "X" OU "Y"
!
      subroutine griltp4(nni,nnj,ip1,ip2,ip3)
#include "impnone.cdk90"
!
!AUTEUR   - P. SARRAZIN JANVIER 87 DRPN DORVAL P.Q. CANADA
!
!LANGAGE - RATFOR
!
!OBJET(GRILTP4)
!          LIRE LA LATITUDE ET LA LONGITUDE DE TOUS LES POINTS
!          DE LA GRILLE DE SORTIE TAPE4 LAT LON ET ECRIRE LES DEUX
!          RECORDS LAT LONG SUR FICHIER STANDARD TYPE "X" SI
!          IP1,IP2,IP3 SONT DEFINIS TYPE "Y" ET SERONT TRANSFERRE DANS
!          IG1,IG2,IG3   IG4=0
!
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!------------------------------------------------------
      integer nni,nnj,ip1,ip2,ip3
      write(6,*)  'CE TYPE DE GRILLE N EST PAS SUPPORTE DANS CETTE VERSION DE PGSM'
      call pgsmabt
      return
      end
