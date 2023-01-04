!
!**S/P PAIRVCT  REMPLACE OU AJOUTE NOM AU DICTIONNAIRE COMMON/PAIR/...
!
   subroutine pairvct(nomusag, varuu, varvv, varmodule, vardir) 
      use app
      implicit none 
!
!AUTEUR P. SARRAZIN DORVAL QUE CANADA FEV 87
!
!REVISION 4.0.2
!   CONVERSION DES VARIABLES HOOLERITH EN CARACTERE
!REVISION 5.6.1
!   INCLUSION DE LA VARIABLE WD - DIRECTION DU VENT Y.Chartier - Aout 1996  
!
!LANGAGE RATFOR
!
!OBJET(PAIRVCT)
!          REMPLACE OU AJOUTE DANS LA TABLE PAIRE DU COMMON/PAIR/..
!          POUR REFERENCE PAR L'USAGER QUI PERMET CERTAINES INTERPOLATIONS
!          DE VARIABLES PAIRES. 2 SETS DE VARIABLES PAIRES INITIALISE 
!          DANS PGSM UU,VV  US,VS.
!
!LIBRAIRIES
!
!          - SOURCE  PGSM ID=ARMNSRC     MFA
!          - OBJET PGSMLIB,ID=ARMNPJS    XMP
!
!ARGUMENTS
!
!  IN    NOMUSAG  NOM DE L'USAGER DONNE PAR LA DIRECTIVE PAIRES
!  IN    VARUU  NOM DE 2 CARACTERES DE LA PREMIERE VARIABLE PAIRE
!  IN    VARUU   NOM DE 2 CARACTERES DE LA DEUXIEME VARIABLE PAIRE
!  IN    VARMODULE  SI VARMODULE .NE.0 NOM DE 2 CARACTERES IDENTIFIANT
!                 LE CHAMP DE SORTIE VITESSE DU VENT  EX:"UV"
!                 SI VARMODULE.EQ.0 INTERPOLATION DE 2 CHAMPS AVEC ORIENTATION
!                 GEOGRAPHIQUE NOM DU PREMIER CHAMP POUR LA SORTIE=VARUU
!                 NOM DU DEUXIEME CHAMP=VARVV
!
!APPEL
!         - VIA DIRECTIVE PAIRES(NOMUSAG,VARUU,VARVV,VARMODULE)
!
!MESSAGE
!         - 'VERIFIER NOMBRE D ARGUMENTS DIRECTIVE PAIRES(3 OU 4 ARGS)'
!           'PAIRES DEJA INITIALISE'
!           'PAIRES("VENT","UU","VV","UV")
!           'PAIRES("UV","UU","VV","0") 
!           'PAIRES("VENTUVS","US","VS","UV")
!           'PAIRES("UVS","UU","VV","0")
!
!IMPLICITES
!MODULES
!
!
!- - -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
#include "defin.cdk90"
#include "pairs.cdk90"
!
!
      integer nomusag(2),varuu,varvv,varmodule,vardir
      character*8 cnomusr
      character*4 cvaruu, cvarvv, ccontrl, cvarwd
      integer i, nw
      integer argdims
      
      external fstcvt
      integer  fstcvt
      
      if (npairuv.lt.3.or.npairuv.gt.5) then
         call app_log(APP_ERROR,'pairvct: Check PAIRES directives for 3 or 4 arguments')
         return
      endif
!
!   VERIFI SI NOM EXISTE DANS LA TABLE SI OUI ON REMPLACE
!   SI NON ON AJOUTE SI LA TABLE N'EST PAS PLEINE 
!
      nw = min(argdims(1), 2) 
      write (cnomusr, 100) (nomusag(i), i=1,nw)
 100  format(2a4)
      
      write (cvaruu, 200) varuu
      write (cvarvv, 200) varvv
      write (ccontrl, 200) varmodule
      write (cvarwd, 200) vardir
 200  format(a4)
      
      if (varmodule.eq.0) ccontrl = '??'
      if (vardir.eq.0) cvarwd = '??'
      
      write (app_msg,*) 'pairvct: PAIRES: ',cnomusr, cvaruu, cvarvv, ccontrl, cvarwd
      call app_log(APP_INFO,app_msg)
      
      call pairvc2(cnomusr, cvaruu, cvarvv, ccontrl, cvarwd)
      return
      end 
  
!**S/P PAIRVC2  REMPLACE OU AJOUTE NOM AU DICTIONNAIRE COMMON/PAIR/...
      subroutine pairvc2(cnomusr,cvaruu,cvarvv,ccontrl,cvarwd)
!
!AUTEUR P. SARRAZIN DORVAL QUE CANADA FEV 87
!
!REVISION 4.0.2
!   CONVERSION DES VARIABLES HOLLERITH EN CARACTERE
!
!
!LANGAGE RATFOR
!
!     OBJET(PAIRVCT)
!          REMPLACE OU AJOUTE DANS LA TABLE PAIRE DU COMMON/PAIR/..
!          POUR REFERENCE PAR L'USAGER QUI PERMET CERTAINES INTERPOLATIONS
!          DE VARIABLES PAIRES. 2 SETS DE VARIABLES PAIRES INITIALISE 
!          DANS PGSM UU,VV  US,VS.
!
!LIBRAIRIES
!
!          - SOURCE  PGSM ID=ARMNSRC     MFA
!          - OBJET PGSMLIB,ID=ARMNPJS    XMP
!
!ARGUMENTS
!
!  IN    NOMUSAG  NOM DE L'USAGER DONNE PAR LA DIRECTIVE PAIRES
!  IN    VARUU  NOM DE 2 CARACTERES DE LA PREMIERE VARIABLE PAIRE
!  IN    VARUU   NOM DE 2 CARACTERES DE LA DEUXIEME VARIABLE PAIRE
!  IN    VARMODULE  SI VARMODULE .NE.0 NOM DE 2 CARACTERES IDENTIFIANT
!                 LE CHAMP DE SORTIE VITESSE DU VENT  EX:"UV"
!                 SI VARMODULE.EQ.0 INTERPOLATION DE 2 CHAMPS AVEC ORIENTATION
!                 GEOGRAPHIQUE NOM DU PREMIER CHAMP POUR LA SORTIE=VARUU
!                 NOM DU DEUXIEME CHAMP=VARVV
!
!APPEL
!         - VIA DIRECTIVE PAIRES(NOMUSAG,VARUU,VARVV,VARMODULE)
!
!MESSAGE
!         - 'VERIFIER NOMBRE D ARGUMENTS DIRECTIVE PAIRES(3 OU 4 ARGS)'
!           'PAIRES DEJA INITIALISE'
!           'PAIRES("VENT","UU","VV","UV")
!           'PAIRES("UV","UU","VV","0") 
!           'PAIRES("VENTUVS","US","VS","UV")
!           'PAIRES("UVS","UU","VV","0")
!
!MODULES
!
!
!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      use app
      implicit none
#include "defin.cdk90"
#include "pairs.cdk90"
!
      character cnomusr*8, cvaruu*4 , cvarvv*4 , ccontrl*4 , cvarwd*4 
      integer np
      logical remplac
!     
!   VERIFI SI NOM EXISTE DANS LA TABLE SI OUI ON REMPLACE
!   SI NON ON AJOUTE SI LA TABLE N'EST PAS PLEINE 
!
      remplac=.false.
      do np=1,npair
         if (cnomusr.eq.paire(np)(1:8)) then
            paire(np)( 1: 8) = cnomusr
            paire(np)( 9:12) = cvaruu
            paire(np)(13:16) = cvarvv
            paire(np)(17:20) = ccontrl
            paire(np)(21:24) = cvarwd
            remplac=.true.
            write (app_msg, *) 'pairvct: PAIRE(NP): ', paire(np)
            call app_log(APP_INFO,app_msg)
             endif
      enddo
!     
      if (remplac)  go to 1000
!
!   SI ON N'A PAS REMPLACE DANS LA TABLE ON AJOUTE
!
      npair = npair + 1
      if (npair.gt.NPAIRMX) then
         write(app_msg,666) npair,npairmx
         call app_log(APP_ERROR,app_msg)
 666     format(1x,'pairvct: Too many pairs in table  NPAIR=',i5,         /'   NPAIRMX=',i5)
         return
      endif
!
      paire(np)( 1: 8) = cnomusr 
      paire(np)( 9:12) = cvaruu 
      paire(np)(13:16) = cvarvv 
      paire(np)(17:20) = ccontrl 
      paire(np)(21:24) = cvarwd
      write (app_msg,*) 'pairvct: PAIRE(NP): ', paire(np)
      call app_log(APP_INFO,app_msg)
return 
!
!
 1000  call app_log(APP_INFO,'pairvct: 2 pair variables replaced')
!     
      return 
      end

