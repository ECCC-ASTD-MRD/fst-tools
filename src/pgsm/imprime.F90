!
!**S/P  IMPRIME CHAMP LU SUR FICHIER D ENTREE OU DE SORTIE
!
      subroutine imprime(cnom,champ,ni,nj)
      use app
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
!        IMPRIME AVEC LA DIRECTIVE PRINTEN RECORD LUE SUR FICHIER D ENTREE
!        OU IMPRIME AVEC LA DIRECTIVE PRINTSR RECORD QUE L ON VA ECRIRE
!        L USAGER CONTROLE LE NOMBRE DE LOCATIONS A IMPRIMER 
!        FENETRE DU CHAMP A IMPRIMER DEFINIE PAR L'USAGER
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
      write(app_msg,600) cnom,nis,njs,niff,njff,ninc,njnc
      call app_log(APP_INFO,app_msg)
 600  format(' PRINT CHAMP(LU) NOM=',a2,'  NIS=',i3,'  NJS=',i3,      '  NIFF=',i3,      '  NJFF=',i3,'  NINC=',i3,'  NJNC=',i3)
 620  format(' PRINT CHAMP(ECRIT) NOM=',a2,'  NIS=',i3,'  NJS=',i3,      '  NIFF=',i3,      '  NJFF=',i3,'  NINC=',i3,'  NJNC=',i3)
!     
      do j=njs,njff,njnc
         write(app_msg,630) j
         call app_log(APP_INFO,app_msg)
         write(app_msg,610) (champ(i,j),i=nis,niff,ninc)
         call app_log(APP_INFO,app_msg)
      enddo
!     
      if (niff.lt.nis)  then
         write(app_msg,*)    ' NIS.lt.NIF DIRECTIVE PRINTEN=OUI,NIS,NJS,NIF,NJF,NINC,NJNC'
         call app_log(APP_INFO,app_msg)
      endif

      if (njff.lt.njs)  then
         write(app_msg,*)      ' NJS.lt.NJF DIRECTIVE PRINTEN=OUI,NIS,NJS,NIF,NJF,NINC,NJNC'
         call app_log(APP_INFO,app_msg)
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
      write(app_msg,620) cnom,niis,njjs,niifs,njjfs,niinc,njjnc
      call app_log(APP_INFO,app_msg)
!     
      do j=njjs,njjfs,njjnc 
         write(app_msg,630) j
         call app_log(APP_INFO,app_msg)
         write(app_msg,610) (champ(i,j),i=niis,niifs,niinc)
         call app_log(APP_INFO,app_msg)
      enddo
!     
      if (niifs.lt.niis)  then
         write(app_msg,*)      ' NIS.lt.NIF DIRECTIVE PRINTSR=OUI,NIS,NJS,NIF,NJF,NINC,NJNC'
         call app_log(APP_INFO,app_msg)
      endif
      if (njjfs.lt.njjs)  then
         write(app_msg,*)      ' NJS.lt.NJF DIRECTIVE PRINTSR=OUI,NIS,NJS,NIF,NJF,NINC,NJNC'
         call app_log(APP_INFO,app_msg)
      endif
      return 
      end
      
