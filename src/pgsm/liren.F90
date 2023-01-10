!
!**S/P   LIREN   LIRE UN CHAMP DANS ACCUMULATEUR
!
   subroutine liren(nom, type, idat, niv, ihr, ip3, etiqet)
      use app
      implicit none
      
      external fstinf,pgsmlir,memoir,fstprm,pgsmabt,imprime
      external fstopc,messags,fstcvt
      integer fstinf,pgsmlir,fstprm,fstopc,fstcvt
!
!AUTEUR P. SARRAZIN  AOUT 82 DRPN DORVAL P.Q. CANADA
!
!LANGAGE RATFOR
!
!OBJET(LIREN)
!         LIRE UN CHAMP SUR FICHIER 1 OU 2 ET SAUVE DANS UN ACCUMULATEUR
!         POUR ETRE UTILISER PAR LES DIRECTIVES PLUSE-PLUSS
!         MOINSE-MOINSS-PFOIS-MOYENE-RACINE-MODUL2E-MODUL2S
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!ARGUMENTS
!   IN   NOM     NOM DU CHAMP LCAR(GZ),"TT"......
!   IN   TYPE    TYPE DE CHAMP "P"=PREVISION  "A" ANALYSE
!   IN   NIV     NIVEAU DU CHAMP
!   IN   IHR     HEURE DU CHAMP
!   IN   IP3     LIBRE(USAGER) COMPTEUR POUR MOYENE UTILISER PAR ECRITS
!   IN   ETIQET  ETIQUETTE 10 CARACTERES
!
!IMPLICITES
!MESSAGES
!         RECORD N EXISTE PAS SUR FICHIER (FSTINF DANS LIREN)
!         RECORD N EXISTE PAS (PGSMLIR DANS ROUTINE LIREN)
!
!MODULES  FSTINF,PGSMABT,FSTPRM,MEMOIR,PGSMLIR
!
!APPEL     VIA DIRECTIVE
!         LIREE(NOM, TYPE, IDAT, NIV, IHR, IP3, ETIQUET)
!         LIRES(NOM, TYPE, IDAT, NIV, IHR, IP3, ETIQUET)
!
! -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
#include "chck.cdk90"
#include "voir.cdk90"
#include "accum.cdk90"
#include "llccmm.cdk90"
#include "indptr.cdk90"
#include "lires.cdk90"
#include "ecrires.cdk90"
#include "dummys.cdk90"
#include "blancs.cdk90"
#include "styles.cdk90"
!
!
      character *12 cetiqet
      character *4 cnomvar
      character *2 ctypvar
      character*1 cigtyp
      integer etiqet(3),idat,ihr,iip3,ip3,irec1,iunit,niv(2),nom,num1,type
      integer cnbits,cdatyp,extra1,extra2,extra3,cubc,cdltf,clng,cswa
      integer iopc
      integer argdims, letiket(3)
      external argdims
      integer lniv
      real p
      character*8 string
!
      iunit=1
      iip3=ip3
      if (ip3.eq.4095)iip3=-1
!
!     MODIFICATION DE HOLLERITH A CARACTERE
!
 100  cnomvar = '    '
      ctypvar = '  '
      cetiqet = '            '
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

      ier = fstcvt(    nom,   type, letiket,    -1,      cnomvar,ctypvar,cetiqet,cigtyp,     .true.)
      irec1=fstinf(iunit,nni,nnj,nnk,idat,cetiqet,lniv,ihr,iip3,      ctypvar,cnomvar)
      if (irec1 .lt. 0)   then
         call app_log(APP_ERROR,'liren: Record does not exist')
         call pgsmabt
      endif

!
!  #  clef pour directive pluse,moinse,ecrits....
      ichck=1
!
!
      ier = fstprm(irec1,idatt,ideet,npas,nni,nnj,nnk, cnbits,cdatyp,      jpp1,jpp2,jpp3,ctypvar,cnomvar,cetiqet,cigtyp,igg1,igg2,igg3,      igg4,cswa, clng, cdltf, cubc, extra1, extra2, extra3)


      if (ier .lt. 0) call app_log(APP_ERROR,'liren: FSTPRM failed')


      cnumv = cnomvar
      ctypv = ctypvar
      cetik = cetiqet
      cigty = cigtyp
!
!       VERIFIER SI GRILLE GAUSSIENNE NI DOIT ETRE PAIR
!
      if (cigtyp.eq.'G'.and.mod(nni,2).ne.0)  call messags(nni)
!
!
!    ALLOCATION DE LA MEMOIRE
!
      allocate(tmpif0(nni,nnj))
!
      if (.not.message) iopc= fstopc('TOLRNC','DEBUGS',.true.)
      num1 =pgsmlir(tmpif0,iunit,nni,nnj,nnk,idat,cetiqet,jpp1,jpp2,      jpp3,ctypvar,cnomvar,cigtyp)
!
      if (num1 .lt. 0) then
         call app_log(APP_ERROR,'liren: Record does not exist')
         call pgsmabt
      endif

      if (printen)  call imprime(cnomvar,tmpif0,nni,nnj)
!
!     SI COMTEUR .NE. 4095  ICNT=1
!
      icnt = 1
      if (iunit.eq.1.and.ip3.eq.   4095)  icnt = jpp3
      if (iunit.eq.2.and.ip3.eq.   4095)  icnt = jpp3
!
!
      return
!
      entry lirsr(nom, type, idat, niv, ihr, ip3, etiqet)
!
      iunit = 2
      iip3=ip3
      if (ip3.eq.4095) iip3=-1
      go to 100
      end

