!
!**   S/P EXTRAIRE L'EXPONENTIEL DE CHAQUE POINT D UN CHAMP DANS ACCUMULATEUR
!     ET MULTIPLIER LE RESULTAT PAR LE FACT
   subroutine operat(fact,ecart,divi)
      use app
      implicit none
      
      external pgsmabt,messags
!
!AUTEUR P.SARRAZIN JUIN 83 DRPN DORVAL P.Q. CANADA
!
!LANGAGE RATFOR
!
!OBJET(OPERAT)
!         EXTRAIRE L'EXPONENTIEL OU LOGARITHME D UN CHAMP DANS
!         L'ACCUMULATEUR DEJA LUT PAR LA DIRECTIVE LIREE OU LIRES
!         ET MULTIPLIER LE RESULTAT PAR FACT
!         CALCUL LA MOYENNE DE CHAQUE POINT DU CHAMP DANS ACCUMULATEUR
!         PFOIS ADDITIONNER OU SOUSTRAIRE UNE CONSTANTE DU CHAMP DANS
!         L'ACCUMULATEUR ET MULTIPLIER OU DIVISER LE RESULTAT
!         EXTRAIRE LA RACINE CARRE DE CHAQUE POINT DU CHAMP DANS ACCUMULATEUR
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!ARGUMENTS
!  IN    FACT    -FACTEUR DE CONVERSION
!  IN    ECART    - UTILISER PAR PFOIS CONSTANTE AJOUTER A CHAQUE POINT
!  IN    DIVI     - UTILISER PAR PFOIS DIVISE CHAQUE POINT DU CHAMP
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!MESSAGES
!        'DIRECTIVE LIREE OU LIRES DOIT ETRE APPELE '
!        'VERIFIER EXPON-PFOIS-MOYENE-RACINE-ALOGN'
!
!MODULES
!         PGSMABT
!
!APPEL    VIA DIRECTIVE
!          PFOIS(FACT,ECART,DIVI) - EXPON(FACT)- MOYENE(FACT) - ALOGN(FACT)
!          RACINE(FACT)
!
!---------------------------------------------------------------
!
!
#include "voir.cdk90"
#include "chck.cdk90"
#include "accum.cdk90"
#include "llccmm.cdk90"
!
! -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      integer i,j, it
      real divi,ec,ecart,fact
!
!     VERIFIER SI DIRECTIVE LIREE OU LIRES A ETE APPELE
!
!     pfois(+ecart*fact/divi)
      it=1
!
!   A CAUSE DE LA DOCUMENTATION ECART DEVIENT FACT POUR PFOIS
!
      ec=fact
      fact=ecart
      ecart=ec
!     erreur faut appeler liree ou lires
 1000 if (ichck.eq.0)   then
         call app_log(APP_ERROR,'operat: LIREE or LIRES has to be called before EXPON-PFOIS-MOYENE-RACINE-ALOGN directives')
         call pgsmabt
      endif
!
!     IT=2 EXTRAIT L'EXPONENTIEL DE CHAQUE POINT ET MULTIPLIT PAR FACT
!     IT=0 EXTRAIT LE LOGARITHME DE CHAQUE POINT ET MULTIPLIT PAR FACT
!     IT=1 PFOIS AJOUTE ECART  MULTIPLIT PAR FACT ET DIVISE PAR DIVI
!
!     IT=3 MOYENE CAL CUL LA MOYENNE DE CHAQUE POINT (ICNT)
!     IT=4 EXTRAIRE LA RACINE CARRE DE CHAQUE PT DU CHAMP
!
      if (it.eq.1) then
         if (message) call app_log(APP_INFO,'operat: PFOIS(ECART,FACTEUR,DIVISEUR)')
      endif
!     $(  # exponentiel
      if (it.eq.2) then
         if (message) call app_log(APP_INFO,'operat: EXPON(FACTEUR)')
      endif
!     $(  # moyenne
      if (it.eq.3) then
         if (message) call app_log(APP_INFO,'operat: MOYENE(FACTEUR)')
      endif

!     $(  # racine
      if (it.eq.4) then
         if (message) call app_log(APP_INFO,'operat: RACINE(FACTEUR)')
      endif
!     $(  # logarithme
      if (it.eq.0)  then
         if (message) call app_log(APP_INFO,'operat: ALOGN(FACTEUR)')
      endif
!
      if (it.eq.0) then
         do j=1,nnj
            do i=1,nni
               tmpif0(i,j)= (alog(tmpif0(i,j))*fact)
            enddo
         enddo
      endif

       if (it.eq.1) then
         do j=1,nnj
            do i=1,nni
             tmpif0(i,j)= (tmpif0(i,j)+ecart)*fact/divi
            enddo
         enddo
       endif

       if (it.eq.2) then
         do j=1,nnj
            do i=1,nni
             tmpif0(i,j)= (exp(tmpif0(i,j))*fact)
            enddo
         enddo
       endif

       if (it.eq.3) then
         do j=1,nnj
            do i=1,nni
             tmpif0(i,j)=(tmpif0(i,j)/icnt)*fact
            enddo
         enddo
       endif

       if (it.eq.4) then
         do j=1,nnj
            do i=1,nni
             tmpif0(i,j)= (sqrt(tmpif0(i,j))*fact)
            enddo
         enddo
       endif

       if (it.eq.5) then
         do j=1,nnj
            do i=1,nni
             tmpif0(i,j)= (abs(tmpif0(i,j))*fact)
            enddo
         enddo
       endif

       if (it.eq.6) then
         do j=1,nnj
            do i=1,nni
             tmpif0(i,j)= (tmpif0(i,j)*(tmpif0(i,j))*fact)
            enddo
         enddo
       endif

       return
!
       entry alogn(fact)
!
!    IT=0 ON CALCUL LE LOGARITE DE CHAQUE PT
!
       it=0
       go to 1000
!
!
!     EXPONENTIEL DE CHAQUE POINT DU CHAMP
!
       entry expon(fact)
       it=2
       go to 1000
!
!
!     PRENDRE LA MOYENNE DE CHAQUE PTS DES CHAMPS ACCUMULES
!     DANS L'ACCUMULATEUR ET MULTIPLIER PAR FACT
!
       entry moyene(fact)
       it=3
!
!     VERIFIER SI COMPTEUR EST PLUS GRAND QUE 1
!
!     # erreur
       if (icnt.le.1) then
          if (message) then
            write(app_msg,*)'operat: dividing by value <= 1 ICNT=',icnt
            call app_log(APP_ERROR,app_msg)
          endif
       endif
!
       go to 1000
!
!     PRENDRE LA RACINE CARRE DE CHAQUE POINT DANS LE CHAMP ACCUMULA
!     ET MULTIPLIER CHAQUE POINT PAR FACT
!
       entry racine(fact)
       it=4
       go to 1000

      entry carre(fact)
       it=6
       go to 1000

      entry absolu(fact)
       it=5
       go to 1000
       end


