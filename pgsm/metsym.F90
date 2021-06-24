!
!**S/P CMETSYM   MISE A JOUR DES TABLES DE SYMETRIE
!
      subroutine cmetsym(cnom,sym) 
#include "impnone.cdk90"
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
      character(len=2) cnom
      
      write(cnom, '(A4)') nom
      write(6, *) 'METSYM: NOM - ',nom,'CNOM - ', cnom 
      call cmetsym(cnom, sym)
      
      
      return
      end
      

