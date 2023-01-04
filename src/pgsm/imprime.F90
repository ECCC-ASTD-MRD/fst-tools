!
!**S/P  IMPRIME CHAMP LUT SUR FICHIER D ENTRE OU DE SORTI
!
      subroutine imprime(cnom,champ,ni,nj)
   implicit none
!
!AUTEUR P. SARRAZIN JUIN 85 DRPN DORVAL P.Q. CANADA
!
!REVISION 4.0.2
!   MODIF. ARGUMENT NOM (ENTIER -> CHARACTER*2)
!   Y. CHARTIER DRPN DORVAL QUEBEC
!
!LANGAGE RATFOR
!
!OBJET(IMPRIME)
!        IMPRIME AVEC LA DIRECTIVE PRINTEN RECORD LUT SUR FICHIER D ENTRE
!        OU IMPRIME AVEC LA DIRECTIVE PRINTSR RECORD QUE L ON VA ECRIRE
!        L USAGER CONTROL LE NOMBRE DE LOCATIONS A IMPRIMER 
!        FENETRE DU CHAMP A IMPRIMER DEFINIT PAR L'USAGER
!        DANS LA DIRECTIVE PRINTEN/PRINTSR MODIFIE LE COMMON
!        LIRES OU ECRIRES PRINTEN=OUI,NIS,NJS,NIF,NJF,NINC,NJNC
!        NIS = POINT DE DEPART DANS LA DIRECTION I (EST-OUEST)
!        NJS = POINT DE DEPART DANS LA DIRECTION J (NORD-SUD)
!        NIF = DERNIER POINT DANS LA DIRECTION I (EST-OUEST)
!        NJF = DERNIER POINT DANS LA DIRECTION J (NORD-SUD) 
!        NINC= INTERVAL DANS LA DIRECTION I
!        NJNC= INTERVAL DANS LA DIRECTION J
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!ARGUMENTS
!  IN     NOM   -NOM DU CHAMP QUE L ON VEUT IMPRIMER 2 CARACTERES
!  IN     CHAMP -CONTIENT LE CHAMP QUE L ON VEUT IMPRIMER
!  IN     NI    -DIMENSION DU CHAMP EST=OUEST
!  IN     NJ    -DIMENSION DU CHAMP NORD-SUD
!
!
!MESSAGES 
!
!MODULES
!         PGSMABT
!
!APPEL   VIA DIRECTIVE
!         PRINTEN(OUI,NIS,NJS,NIF,NJF,NINC,NJNC)
!         PRINTSR(OUI,NIS,NJS,NIF,NJF,NINC,NJNC)
!
! - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
#include "voir.cdk90"
#include "lires.cdk90"
#include "ecrires.cdk90"
!
      integer i,j,ni,nj,njff,niff,niifs,njjfs
      character*4 cnom
      real champ(ni,nj)

      niff=nif
      njff=njf
      if (nif.gt.ni)  niff=ni
      if (njf.gt.nj)  njff=nj
!     
      write(6,600) cnom,nis,njs,niff,njff,ninc,njnc
 600  format(' PRINT CHAMP(LU) NOM=',a2,'  NIS=',i3,'  NJS=',i3,      '  NIFF=',i3,      '  NJFF=',i3,'  NINC=',i3,'  NJNC=',i3)
 620  format(' PRINT CHAMP(ECRIT) NOM=',a2,'  NIS=',i3,'  NJS=',i3,      '  NIFF=',i3,      '  NJFF=',i3,'  NINC=',i3,'  NJNC=',i3)
!     
      do j=njs,njff,njnc
         write(6,630) j
         write(6,610) (champ(i,j),i=nis,niff,ninc)
      enddo
!     
      if (niff.lt.nis)  then
         write(6,*)    ' NIS.lt.NIF DIRECTIVE PRINTEN=OUI,NIS,NJS,NIF,NJF,NINC,NJNC'
      endif

      if (njff.lt.njs)  then
         write(6,*)      ' NJS.lt.NJF DIRECTIVE PRINTEN=OUI,NIS,NJS,NIF,NJF,NINC,NJNC'
      endif
 610  format(1h ,10e13.5)
 630  format('  RANGEE NO ',i3)
      return 
      entry imprims(cnom,champ,ni,nj)
!     
      niifs=niif
      njjfs=njjf
      if (niif.gt.ni)  niifs=ni
      if (njjf.gt.nj)  njjfs=nj
!     
      write(6,620) cnom,niis,njjs,niifs,njjfs,niinc,njjnc
!     
      do j=njjs,njjfs,njjnc 
         write(6,630) j
         write(6,610) (champ(i,j),i=niis,niifs,niinc)
      enddo
!     
      if (niifs.lt.niis)  then
         write(6,*)      ' NIS.lt.NIF DIRECTIVE PRINTSR=OUI,NIS,NJS,NIF,NJF,NINC,NJNC'
      endif
      if (njjfs.lt.njjs)  then
         write(6,*)      ' NJS.lt.NJF DIRECTIVE PRINTSR=OUI,NIS,NJS,NIF,NJF,NINC,NJNC'
      endif
      return 
      end
      
