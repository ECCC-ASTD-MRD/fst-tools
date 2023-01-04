!
!**S/P HEURE INITIALISER TABLE HEURE
!
   subroutine heure(ih1,ih2,ih3,ih4,ih5,ih6,ih7,ih8,ih9,ih10,      ih11,ih12,ih13,ih14,ih15,ih16,ih17,ih18,ih19,ih20,      ih21,ih22,ih23,ih24,ih25,ih26,ih27,ih28,ih29,       ih30,ih31,ih32,ih33,ih34,ih35,ih36,ih37,ih38,ih39,ih40) 
      use app
      implicit none
      
      external pgsmabt,messags
!
!AUTEUR P. SARRAZIN JANVIER 82 DRPN DORVAL P.Q. CANADA
!
!REVISION 
!        P. SARRAZIN JAN 85 POUR AUGMENTER DE 20 A 40 HEURES
!
!LANGAGE RATFOR
!
!OBJET(HEURE)
!         EXTRAIRE LES HEURES DEMANDER PAR L'USAGER ECRIRE DANS
!         LA TABLE HEURE
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!ARGUMENTS
!  IN     IH1.....IH40  HEURE DEMANDER PAR L'USAGER (READLX)
!
!MESSAGES 
!         MAUVAISE DIRECTIVE HEURE NHEURE=
!
!MODULES
!         PGSMABT
!
!APPEL   VIA DIRECTIVE
!         HEURE(IH1,IH2,IH3.....................IH40)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "defin.cdk90"
#include "heures.cdk90"
#include "voir.cdk90"
#include "accum.cdk90"
!
      integer ih1,ih2,ih3,ih4,ih5,ih6,ih7,ih8,ih9,ih10,ih11,ih12,ih13
      integer ih14,ih15,ih16,ih17,ih18,ih19,ih20,ih21,ih22,ih23,ih24
      integer ih25,ih26,ih27,ih28,ih29,ih30,ih31,ih32,ih33,ih34,ih35
      integer ih36,ih37,ih38,ih39,ih40
!     
!     
      nheure = min0(MXHEURE,nheure)
!     
!     
!     
      go to (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,      23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40)nheure
 40   heures(40) = ih40
 39   heures(39) = ih39
 38   heures(38) = ih38
 37   heures(37) = ih37
 36   heures(36) = ih36
 35   heures(35) = ih35
 34   heures(34) = ih34
 33   heures(33) = ih33
 32   heures(32) = ih32
 31   heures(31) = ih31
 30   heures(30) = ih30
 29   heures(29) = ih29
 28   heures(28) = ih28
 27   heures(27) = ih27
 26   heures(26) = ih26
 25   heures(25) = ih25
 24   heures(24) = ih24
 23   heures(23) = ih23
 22   heures(22) = ih22
 21   heures(21) = ih21
 20   heures(20) = ih20
 19   heures(19) = ih19
 18   heures(18) = ih18
 17   heures(17) = ih17
 16   heures(16) = ih16
 15   heures(15) = ih15
 14   heures(14) = ih14
 13   heures(13) = ih13
 12   heures(12) = ih12
 11   heures(11) = ih11
 10   heures(10) = ih10
 9    heures(9)  = ih9
 8    heures(8)  = ih8
 7    heures(7)  = ih7
 6    heures(6)  = ih6
 5    heures(5)  = ih5
 4    heures(4)  = ih4
 3    heures(3)  = ih3
 2    heures(2)  = ih2
 1    heures(1)  = ih1
!     
!     
!     #  change de location a cause de readlx
      nhur = nheure  
      return 
      end
