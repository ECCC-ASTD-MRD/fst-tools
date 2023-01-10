!
!**S/P   COMME   LIRE UN CHAMP DANS ACCUMULATEUR
!
      subroutine comme(iunit, nom, type, idat, niv, ihr, ip3, etiqet)
         use app
         implicit none

      external fstinf,pgsmlir,memoir,fstprm,pgsmabt,imprime
      external fstopc,messags,fstcvt
      integer fstinf,pgsmlir,fstprm,fstopc,fstcvt
!
!AUTEUR Y. CHARTIER
!
!LANGAGE FORTRAN 77
!
!OBJET(COMME)
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
!         RECORD N EXISTE PAS SUR FICHIER (FSTINF DANS COMME)
!         RECORD N EXISTE PAS (PGSMLIR DANS ROUTINE COMME)
!
!MODULES  FSTINF,PGSMABT,FSTPRM,MEMOIR,PGSMLIR
!
!APPEL     VIA DIRECTIVE
!         LIREE(NOM, TYPE, IDAT, NIV, IHR, IP3, ETIQUET)
!         LIRES(NOM, TYPE, IDAT, NIV, IHR, IP3, ETIQUET)
!
! -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
#include "accum.cdk90"
#include "chck.cdk90"
#include "voir.cdk90"
#include "llccmm.cdk90"
#include "indptr.cdk90"
#include "lires.cdk90"
#include "dummys.cdk90"
#include "grilles.cdk90"
#include "gdz.cdk90"
#include "blancs.cdk90"
!
!
      character *12 cetiqet
      character *4 cnomvar
      character *2 ctypvar
      character *1 cigtyp
      character *4 cbidon
      integer etiqet(3),idat,ihr, ip3,irec1,iunit,niv,nom,num1,type
      integer cnbits,cdatyp,extra1,extra2,extra3,cubc,cdltf,clng,cswa
      integer iopc, bidon,i

      integer ezqkdef, ezgxprm,gdll, argdims, letiket(3)
      external  ezqkdef, ezgxprm, gdll, argdims
!
!
!     MODIFICATION DE HOLLERITH A CARACTERE
!
      bidon = 0
      cnomvar = '    '
      ctypvar = '  '
      cetiqet = '            '
      cigtyp  = ' '


      letiket(1) = etiqet(1)
      letiket(2) = blancs
      letiket(3) = blancs
      if (argdims(8).gt.1) then
         letiket(2) = etiqet(2)
      endif
      if (argdims(8).gt.2) then
         letiket(3) = etiqet(3)
      endif

 100  ier = fstcvt(    nom,   type, letiket,    bidon,              cnomvar,ctypvar,cetiqet,cbidon,.true.)

      if (etiqet(1) .ne. -1) then
         write(cetiqet,'(3A4)') (etiqet(i), i=1,argdims(9))
      else
         cetiqet = '            '
      endif

      if (cnomvar=='    '.and.ctypvar.eq.'  '.and.cetiqet=='            '.and. niv == -1 .and.ihr==-1.and.ip3==-1.and.idat==-1) then
         call app_log(APP_ERROR,'comme: Selection parameters too vague')
         call pgsmabt
      endif

      irec1=fstinf(iunit,nni,nnj,nnk,idat,cetiqet,niv,ihr,ip3,      ctypvar,cnomvar)
      if (irec1 .lt. 0)   then
         call app_log(APP_ERROR,'comme: Record does not exist')
         call pgsmabt
      endif
!
      if (nnk.gt.1)   then
         call app_log(APP_ERROR,'comme: PGSM does not accept 3 dimension fields (NK>1)')
         call pgsmabt
      endif
!
!
!  #  clef pour directive pluse,moinse,ecrits....
!
!
      ier = fstprm(irec1,idatt,ideet,npas,nni,nnj,nnk, cnbits,cdatyp, &
         jpp1,jpp2,jpp3,ctypvar,cnomvar,cetiqet,cigtyp,igg1,igg2,igg3,      igg4,cswa, clng, cdltf, cubc,&
         extra1, extra2, extra3)


      if (ier .lt. 0) then
         call app_log(APP_ERROR,'comme: Record does not exist')
         call pgsmabt
      endif

!    ALLOCATION DE LA MEMOIRE
!
      if (cigtyp.ne.'Z'.and.cigtyp.ne.'Y') then
         gdout = ezqkdef(nni,nnj, cigtyp,igg1,igg2,igg3,igg4,iunit)
         ier = ezgxprm(gdout,li,lj,cgrtyp,         lg1,lg2,lg3,lg4,cgtypxy,ig1ref,ig2ref,ig3ref,ig4ref)
         allocate(tmplon(li,lj))
         allocate(tmplat(li,lj))
      else
	      if (iunit == 1) then
            call gritp12(7,igg1,igg2,igg3)
         else
            call gritp12(8,igg1,igg2,igg3)
         endif
      endif

!
      if (.not.message) iopc= fstopc('TOLRNC','DEBUGS',.true.)

      return
!
      end

