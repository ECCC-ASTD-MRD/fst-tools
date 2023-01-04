!
!**S/P  ADDITIONNE SOUSTRAIT MULTIPLIT MODULE 2 CHAMPS
!
   subroutine plmnmod(nom,type,idat,niv,ihr,ip3,etiqet)
      use app
      implicit none
!
      external fstinf,fstsui,fstprm,pgsmabt,imprime
      external lopascm,messags,memoir,pgsmluk,fstcvt
      integer fstinf,fstsui,fstprm,pgsmluk,fstcvt
!
!AUTEUR P. SARRAZIN AOUT 82 DRPN DORVAL P.Q. CANADA
!
!LANGAGE RATFOR
!
!OBJET(PLMNMOD)
!         LIRE UN CHAMP SUR FICHIER 1 OU 2 DE MEME NATURE ET DIMENSIONS
!         CELUI DANS L ACCUMULATEUR ET QUE L ON AJOUTE , SOUSTRAIT , MULTIPLIT
!         OU FAIT LA SOMME DE CHAQUE POINT DES DEUX CHAMPS AU CARRE.
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!ARGUMENTS
!   IN   NOM    NOM DU CHAMP  LCAR(GZ),"TT"....
!   IN   TYPE   TYPE DE CHAMP P=PREVISION  A=ANALYSE
!   IN   NIV    NIVEAU DU CHAMP
!   IN   IDAT   DATE DU CHAMP CMC STAMP
!   IN   IHR    HEURE DU CHAMP
!   IN   IP3    LIBRE (USAGER)
!   IN   ETIQET ETIQUETTE DU CHAMP 10 CARACTERES
!
!- - -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!MESSAGES
!         RECORD N EXISTE PAS SUR FICHIER
!         DIRECTIVE LIREE OU LIRES DOIT-ETRE APPELE AVANT
!                 'NI   ENTRE =',NIE,'NI LIREE\LIRES=',NNI,
!                 'NJ   ENTRE =',NJE,'NJ LIREE\LIRES=',NNJ,
!                 'NK   ENTRE =',NKE,'NK LIREE\LIRES=',NNK,
!                 'DIMENSION DU CHAMP   MAUVAISE'
!                 TYPE DE GRILLE DIFFERENT FATAL CHAMP=
!                 MAUVAISE HEMISPHERE CHAMP ...DOIT-ETRE=
!                 ERREUR  2 CHAMPS DIFFERENTS
!                 GRILLE INCONU DIRECTIVE  PLUS-MOIN-MODULE
!
!APPEL VIA DIRECTIVE
!       PLUSE(NOM,TYPE,IDAT,NIV,IHR,IP3,ETIQET) FICHIER D'ENTRE
!       PLUSS(NOM,TYPE,IDAT,NIV,IHR,IP3,ETIQET)  FICHIER DE SORTIE
!       MOINSE(NOM,TYPE,IDAT,NIV,IHR,IP3,ETIQET) FICHIER D'ENTRE
!       MOINSS(NOM,TYPE,IDAT,NIV,IHR,IP3,ETIQET)  FICHIER DE SORTIE
!       MODUL2E(NOM,TYPE,IDAT,NIV,IHR,IP3,ETIQET) FICHIER D'ENTRE
!       MODUL2S(NOM,TYPE,IDAT,NIV,IHR,IP3,ETIQET)  FICHIER DE SORTIE
!       FOISE(NOM,TYPE,IDAT,NIV,IHR,IP3,ETIQET) FICHIER D'ENTRE
!       FOISS(NOM,TYPE,IDAT,NIV,IHR,IP3,ETIQET)  FICHIER DE SORTIE
!
!MODULES  FSTINF,FSTSUI,PGSMABT,FSTPRM,MEMOIR
!
!----------------------------------------------------------------------
!
!
#include "lires.cdk90"
#include "voir.cdk90"
#include "ecrires.cdk90"
#include "chck.cdk90"
#include "accum.cdk90"
#include "llccmm.cdk90"
#include "blancs.cdk90"
#include "styles.cdk90"

!
      character*12 cetike,cetiket
      character*4 cnomvar, cnumve
      character*2 ctypvar,ctypve
      character*1 cigtyp,cigtye
!
      integer type,etiqet(3),nom,aa,letiqet(3)
      integer idat,idate,ideete,if1,ig1e,ig2e,ig3e,ig4e
      integer ihr,ip3,irec,itot,iunit,jp1e,jp2e,jp3e,nie
      integer niv(2),nje,nke,npase
      integer cnbits,cdatyp,cswa,clng,cdltf,cubc,extra1,extra2,extra3
      integer argdims,letiket(3)
      external argdims
      integer lniv
      real p
      character*8 string
!
!    AA  MULTIPLICATEUR POUR AJOUTER OU SOUSTRAIRE
!
      aa=1
      iunit=1
!
!    VERIFIER SI DIRECTIVE LIREN OU LIRSR A ETE APPELE
!
 100  if (ichck.eq.0)  then
!     erreur faut appeler liren ou lirsr
         call app_log(APP_ERROR,'plmnmod: Directives LIREE or LIRES msut be called before')
         call pgsmabt
      endif
!
!   MODIFICATION DE HOLLERITH A CARACTERE
!
      cnomvar = '    '
      ctypvar = '  '
      cetiket = '            '
      cigtyp  = ' '

      letiket(1) = etiqet(1)
      letiket(2) = blancs
      letiket(3) = blancs
      if (argdims(7).gt.1) then
         letiket(2) = etiqet(2)
      endif
      if (argdims(7).gt.2) then
         letiket(3) = etiqet(3)
      endif

      lniv = niv(1)
      if (argdims(4) > 1) then
         p = transfer(niv(1), p)
         call convip_plus(lniv, p, -1*niv(2)-1000, ip1style, string, .false.)
      endif

      ier = fstcvt(    nom,    type,  letiket,     -1,      cnomvar, ctypvar, cetiket, cigtyp,     .true.)
!
      irec=fstinf(iunit,nie,nje,nke,idat,cetiket,lniv,ihr,ip3,      ctypvar, cnomvar)


      if (irec.lt.0)  then
!     arret record n'EXISTE PAS
         call app_log(APP_ERROR,'plmnmod: FSTINF failed,check PLUSE/S - MOINS(E\S) - MODUL2E/S - FOIS(E\S) directives')
         call pgsmabt
      endif


 10   if (irec.gt.-1) then
!
!
         ier = fstprm(irec,idate,ideete,npase,nie,nje,nke,          cnbits,cdatyp,         jp1e,jp2e,jp3e,ctypve,cnumve,cetike,cigtye,         ig1e,ig2e,ig3e,ig4e,         cswa, clng, cdltf, cubc, extra1, extra2, extra3)
         if (ier.lt.0) then
            call app_log(APP_ERROR,'plmnmod: FSTPRM failed')
         endif
!
!     VERIFIER SI GRILLE GAUSSIENNE NI DOIT ETRE PAIR
!
         if (cigtye.eq.'G'.and.mod(nie,2).ne.0)  call messags(nie)
!
!
         if (nie.ne.nni) then
            write(app_msg,600)nie,nni
            call app_log(APP_ERROR,app_msg)
            call pgsmabt
         endif

         if (nje.ne.nnj) then
            write(app_msg,610)nje,nnj
            call app_log(APP_ERROR,app_msg)
            call pgsmabt
         endif

         if (nke.ne.nnk) then
            write(app_msg,620)nke,nnk
            call app_log(APP_ERROR,app_msg)
            call pgsmabt
         endif
!
         if (cigty.ne.cigtye) then
            write(app_msg,660)cigtye,cigty
            call app_log(APP_ERROR,app_msg)
            call pgsmabt
         endif
!
         if (cigty.eq.'G'.or.cigty.eq.'A'.or.cigty.eq.'B') then
            if (ig1e.ne.igg1) then
               write(app_msg,*)'plmnmod: Wrong hemisphere ig1e =',ig1e,' has to be ',igg1,'. heck PLUSE/S - MOINS(E\S) - MODUL2E/S - FOIS(E\S) directives'
               call app_log(APP_ERROR,app_msg)
               call pgsmabt
            endif
         else
            if (cigty.eq.'N'.or.cigty.eq.'S'.or.cigty.eq.'L') then
               if (ig1e.ne.igg1.or.ig2e.ne.igg2.or.               ig3e.ne.igg3.or.ig4e.ne.igg4) then
                  call app_log(APP_ERROR,'plmnmod: 2 different fields, check PLUSE/S - MOINS(E\S) - MODUL2E/S - FOIS(E\S) directives')
                  call pgsmabt
               endif
            endif
         endif
!
!
!    ALLOCATION DE LA MEMOIRE
!
         allocate(tmpif1(nni,nnj))
!
         ier = pgsmluk(tmpif1, irec, nni, nnj, nnk,cnomvar,cigty)
         if (ier.lt.0)  then
            call app_log(APP_ERROR,'plmnmod: pgsmluk failed')
            return
         endif


         if (printen)  call imprime(cnumve,tmpif1,nni,nnj)
!
!
!     AJOUTE 1 AU COMPTEUR ICNT DANS COMMON ACCUM INITIALISER
!     A 1 DANS MAIN PROGRAM
!
         icnt = icnt + 1
!
!
!     ADDITIONNE-SOUSTRAIT-MODULE-MULTIPLIT CHAQUE PTS DES DEUX CHAMPS
!
         itot=nni*nnj*nnk
         call lopascm(tmpif0,tmpif1,aa,itot)
!
!
         deallocate(tmpif1)
!
         if (aa.ne.1.or.unefois.or.once) goto 11
         irec = fstsui(iunit,nie,nje,nke)
!
         goto 10
      endif
 11   continue
!
      return
      entry pluss(nom,type,idat,niv,ihr,ip3,etiqet)
!
!   AA=MULTIPLICATEUR POUR AJOUTER
!
      aa=1
      iunit = 2
      go to 100
!
      entry moinse(nom,type,idat,niv,ihr,ip3,etiqet)
!
!     AA=MULTIPLICATEUR POUR  SOUSTRAIRE
!
      aa=-1
      iunit = 1
      go to 100
!
      entry moinss(nom,type,idat,niv,ihr,ip3,etiqet)
!
!     AA=MULTIPLICATEUR POUR  SOUSTRAIRE
!
      aa=-1
      iunit = 2
      go to 100
!
      entry modul2e(nom,type,idat,niv,ihr,ip3,etiqet)
!
!     2   AA=2 ADDITIONNER LES DEUX CHAMPS AU CARRE
!
      aa=2
      iunit = 1
      go to 100
!
      entry modul2s(nom,type,idat,niv,ihr,ip3,etiqet)
!
!     AA=2 ADDITIONNER LES DEUX CHAMPS AU CARRE
!
      aa=2
      iunit = 2
      go to 100
!
      entry foise(nom,type,idat,niv,ihr,ip3,etiqet)
!
!     AA=3 MULTIPLIER CHAQUE PT DES DEUX CHAMPS
!
      aa=3
      iunit = 1
      go to 100
!
      entry foiss(nom,type,idat,niv,ihr,ip3,etiqet)
!
!     AA=3 MULTIPLIER CHAQUE PT DES DEUX CHAMPS
!
      aa=3
      iunit = 2
      go to 100
!
      entry divisee(nom,type,idat,niv,ihr,ip3,etiqet)
!
!     AA=4 MULTIPLIER CHAQUE PT DES DEUX CHAMPS
!
      aa=4
      iunit = 1
      go to 100
!
      entry divises(nom,type,idat,niv,ihr,ip3,etiqet)
!
!     AA=4 MULTIPLIER CHAQUE PT DES DEUX CHAMPS
!
      aa=4
      iunit = 2
      go to 100
!
600  format(2x,'plmnmod: Wrong field dimension: NI  ENTRE =',i10,'NI ACCUMULATEUR=',i10)
610  format(2x,'plmnmod: Wrong field dimension: NJ  ENTRE =',i10,'NJ ACCUMULATEUR=',i10)
620  format(2x,'plmnmod: Wrong field dimension: NK  ENTRE =',i10,'NK ACCUMULATEUR=',i10)
660  format(2x,'plmnmod: Wrong grid: GRILLE ENTRE=',a1,'ACCUMULATEUR=',a1)
!
      end


