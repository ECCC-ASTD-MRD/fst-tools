!
!**S/P CMETSYM   MISE A JOUR DES TABLES DE SYMETRIE
!
   subroutine cmetsym(cnom,sym) 
      use app
      implicit none
      
      external pgsmabt,messags
!
!AUTEUR  P.SARRAZIN  FEVRIER 82  DRPN DORVAL  P.Q. CANADA
!
!LANGAGE RATFOR
!
!OBJET(CMETSYM)
!          DEFINIR NOM D UN CHAMP AVEC .TRUE. OU .FALSE.
!          VRAI=SYMETRIQUE  FAUX ANTISYMETRIQUE
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!ARGUMENTS
!  IN   NOM    NOM DU CHAMP
!  IN   SYM    VALEUR  .TRUE. / .FALSE. 
!
!IMPLICITES
!MESSAGES 
!         PLUS DE PLACE DANS LES TABLES DE SYMETRIE
!         MAUVAISE DIRECTIVE METSYM DOIT AVOIR 2 ARGUMENTS
!
!
!APPEL    - VIA MAIN PGSM
!         CALL CMETSYM(CNOM,.TRUE./.FALSE.)
!
!
!MODULES  PGSMABT
!
!---------------------------------------------------------------------
!
#include "defin.cdk90"
#include "symnom.cdk90"
#include "voir.cdk90"
#include "dummys.cdk90"
!
      logical sym
      character*4 cnom 
!     
!     sauve nsym dans nsymm a cause readlx qui remet a zero      nsymm = nsym 
!
      if (nnoms.lt.maxnoms) then
         nnoms = nnoms + 1
         ssym(nnoms) = sym
         noms(nnoms) = cnom
      else
         if (message) then
            call app_log(APP_WARNING,'cmetsym: No more room in symetry tables METSYM')
         endif
      endif
      
      return 
      end
!**   S/P METSYM   MISE A JOUR DES TABLES DE SYMETRIE
!     
   subroutine metsym(nom,sym) 
      use app
      implicit none

      external cmetsym
!
!AUTEUR  P.SARRAZIN  FEVRIER 82  DRPN DORVAL  P.Q. CANADA
!
!REVISION 4.0.2
!   SEPARATION DE METSYM EN 2 PARTIES, L'UNE AVEC LE NOM STORE DANS UN ENTIER,
!      L'AUTRE AVEC LE NOM STORE DANS UNE CHAINE DE CARACTERES
!   Y. CHARTIER DRPN DORVAL QUEBEC
!LANGAGE RATFOR
!
!OBJET(METSYM)
!          INTERFACE A LA ROUTINE "CMETSYM"
!
      integer nom
      logical sym
      character*2 cnom
      
      write(cnom, '(A4)') nom
      write(app_msg, *) 'metsym: NOM - ',nom,'CNOM - ', cnom 
      call app_log(APP_INFO,app_msg)
      call cmetsym(cnom, sym)
      
      
      return
      end
      

