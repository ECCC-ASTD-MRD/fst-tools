!
!**s/p coupe, calcul coupe zonale\meridionale
!
#include "defin.cdk90"
   subroutine pgcoupe(nom,lcoupe,ipr1,ipr2,ipr3,ipr4,ipr5,ipr6,      ipr7,ipr8,ipr9,ipr10,ipr11,ipr12,ipr13,ipr14,ipr15,ipr16,      ipr17,ipr18,ipr19,ipr20,ipr21,ipr22,ipr23,ipr24,ipr25,      ipr26,ipr27,ipr28,ipr29,ipr30)
      use app            
      implicit none
      external coupzm,messags,fstcvt,pgsmabt
      integer  fstcvt
!     
!     auteur p. sarrazin avril 85 drpn dorval p.q. canada
!     
!     revision 
!     4.0.2 - conversion en caracteres de toutes les variables
!     de type hollerith
!     y. chartier- dorval quebec juillet 90 drpn.
!
!     
!     langage ratfor
!     
!     objet(coupe)
!     lire un champ sur une grille "g","l","b","c","a"  et calcul une
!     moyenne zonale est-ouest ou meridionale nord-sud pour chaque niveau
!     max(30) de l'usager 
!     
!librairies
!     -source  armnsrc,drpn
!     -objet   pgsmlib,id=armnpjs.
!     
!     arguments
!     in    nom        nom du champ requis.....z,tt,es......
!     in    lcoupe     lcoupe=lcar(zon)  coupe zonale est-ouest
!     lcoupe=lcar(mer)  coupe meridionale nord-sud
!     in    ipr1-ipr30 niveau de l'usager optionel
!     
!     implicites
!     
!     
!modules
!     coupzm
!     
!     appel
!     via directive
!     moyent(nom,lcoup,ipr..........)
!     moysrt(nom,lcoup,ipr..........)
!     maximum de 30 ipr
!     
!     messages 
!     pas assez d'arguments directive moyent/moysrt
!
!     
!     implicites
!     
#include "voir.cdk90"
!     
!     
#include "heures.cdk90"
!     
!     
#include "nivos.cdk90"
!     
!     
#include "accum.cdk90"
!     
!     
#include "indptr.cdk90"
!     
!
#include "dates.cdk90"
!
!
#include "champs.cdk90"
!
!
#include "cfldinf.cdk90"
!
!
#include "lires.cdk90"
!
!
#include "ecrires.cdk90"
!
      integer nom,lcoupe,ipr1,ipr2,ipr3,ipr4,ipr5,ipr6,ipr7,ipr8
      integer ipr9,ipr10,ipr11,ipr12,ipr13,ipr14,ipr15,ipr16,ipr17
      integer ipr18,ipr19,ipr20,ipr21,ipr22,ipr23,ipr24,ipr25
      integer ipr26,ipr27,ipr28,ipr29,ipr30
      integer iunit,nparm,i
!     
      character*8 cjcoup
!     
!     initialiser nivospr
!     
      do i=1,30
         nivospr(i)=-1
      enddo
!     
!     
      iunit=1
 1000 nmoy = min0(31,nmoy)
      go to (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,        21,22,23,24,25,26,27,28,29,30,31) nmoy
 31   nivospr(30) = ipr30
 30   nivospr(29) = ipr29
 29   nivospr(28) = ipr28
 28   nivospr(27) = ipr27
 27   nivospr(26) = ipr26
 26   nivospr(25) = ipr25
 25   nivospr(24) = ipr24
 24   nivospr(23) = ipr23
 23   nivospr(22) = ipr22
 22   nivospr(21) = ipr21
 21   nivospr(20) = ipr20
 20   nivospr(19) = ipr19
 19   nivospr(18) = ipr18
 18   nivospr(17) = ipr17
 17   nivospr(16) = ipr16
 16   nivospr(15) = ipr15
 15   nivospr(14) = ipr14
 14   nivospr(13) = ipr13
 13   nivospr(12) = ipr12
 12   nivospr(11) = ipr11
 11   nivospr(10) = ipr10
 10   nivospr(9) = ipr9
 9    nivospr(8) = ipr8
 8    nivospr(7) = ipr7
 7    nivospr(6) = ipr6
 6    nivospr(5) = ipr5
 5    nivospr(4) = ipr4
 4    nivospr(3) = ipr3
 3    nivospr(2) = ipr2
 2    nivospr(1) = ipr1
!     
!   1 nomvar = nom
 1    continue
!
      
!     sauve la valeur de nmoy a cause de readlx
      nmo = nmoy   
!     
      nparm = max0(1,nmo-2)
!     
!     
      write(cnomvar,'(a2)') nom
      
!     jcoup=lcoupe
      if (lcoupe.eq.1) then
         cjcoup = 'ZON'
      endif
  
      if (lcoupe.eq.2) then
         cjcoup = 'MER'
      endif

      call coupzm(iunit,cnomvar,cjcoup) 
      return
!     
!     directive moysrt lire sur fichier de sorti
!     
      entry moysrt(nom,lcoupe,ipr1,ipr2,ipr3,ipr4,ipr5,ipr6,ipr7,ipr8,      ipr9,ipr10,ipr11,ipr12,ipr13,ipr14,ipr15,ipr16,      ipr17,ipr18,ipr19,ipr20,ipr21,ipr22,ipr23,ipr24,ipr25,      ipr26,ipr27,ipr28,ipr29,ipr30)
      iunit=2
      go to 1000
!     
      end 
      
