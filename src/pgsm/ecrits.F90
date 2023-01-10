!
!**S/P ECRITS   ECRIRE SUR FICHIER STANDARD, MS, SEQUENTIEL
!
   subroutine ecrits(nom,npac,idat,ip1,ip2,ip3,type,      etiqet,igtyp,imprim,ig1srt,ig2srt,ig3srt,ig4srt)
      use app
      implicit none

      external conver,fstecr,fclos,memoir,pgsmabt,imprims,fstopc,messags,fstcvt,putfld
      integer fstecr,fstopc,fstcvt,iopc
!
!AUTEUR  P.SARRAZIN  AOUT 82  DRPN DORVAL P.Q. CANADA
!
!LANGAGE RATFOR
!
!OBJET(ECRITS)
!          ECRIRE SUR FICHIER STANDARD AVEC ROUTINE ECRIRE
!          ECRIRE SUR FICHIER MS AVEC PUTFLD
!          ECRIRE SUR FICHIER SEQUENTIEL AVEC PUTFLD
!
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!ARGUMENTS
!   IN    NOM     NOM DU CHAMP 2 CARACTERES
!   IN    NPAC    COMPACTION DU DATA DANS CHAMP
!   IN    IDAT    DATE DU CHAMP (CMC STAMP)
!   IN    IP1     NORMALEMENT NIVEAU DU CHAMP
!   IN    IP2     HEURE DU CHAMP
!   IN    IP3     LIBRE
!   IN    TYPV    TYPE DU CHAMP 1 CARACTERE
!   IN    ETIK    ETIQUETTE 1 MOT CDC (USAGER)
!   IN    IGTYP   TYPE DE GRILLE 1 CARACTERE
!   IN    IMPRIM   IMPRIMME LES ELEMENTS DES FICHIERS
!                  D'ENTRE OU DE SORTI
!
!IMPLICITES
!MESSAGES
!          MAUVAISE DIRECTIVE NUMERO=0 FICHIER MS
!          FICHIER INCONNU ROUTINE ECRITUR
!          FIN DU FICHIER CA DEBORDE
!
!MODULES  FSTECR,PUTFLD
!
!APPEL   VIA DIRECTIVE
!        ECRITS(NOM,NPACK,IDAT,IP1,IP2,IP3,TYPE ETIQET,IGTYP,IMPRIM)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!
#include "lires.cdk90"
#include "voir.cdk90"
#include "dummys.cdk90"
#include "ecrires.cdk90"
#include "chck.cdk90"
#include "convers.cdk90"
#include "accum.cdk90"
#include "indptr.cdk90"
#include "enrege.cdk90"
#include "dates.cdk90"
#include "llccmm.cdk90"
#include "blancs.cdk90"
#include "styles.cdk90"
#include "lnkflds.cdk90"

      character *12 cetiqet
      character *4 cnomvar
      character *2 ctypvar
      character *1 cigtyp

      integer i
      integer nom,npac,idat,ip1(2),ip2,ip3,igtyp,imprim,npkc
      integer iun,istamp,etiqet(*),type,cdatyp
      integer ig1srt,ig2srt,ig3srt,ig4srt,ig1s,ig2s,ig3s,ig4s
      logical rewrit
      integer letiket(3)

      integer argdims
      external argdims
      integer lip1
      real p
      character*8 string

!-----------------------------------------------------------------
!
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

      lip1 = ip1(1)
      if (argdims(4) > 1) then
         p = transfer(ip1(1), p)
         call convip_plus(lip1, p, -1*ip1(2)-1000, 2, string, .false.)
      endif

      ier = fstcvt(      nom,    type,  letiket,  igtyp,       cnomvar, ctypvar, cetiqet, cigtyp, .true.)

      if (nom.eq.-1)        cnomvar=cnumv
      if (type.eq.-1)       ctypvar=ctypv
      if (etiqet(1).eq.-1)  cetiqet=cetik
      if (igtyp.eq.-1)      cigtyp=cigty

      npkc=npac
      if (npac.eq.-1)  npkc=-16
      if (idat.eq.-1)  idat=idatt
      if (lip1.eq.-1)  lip1=jpp1
      if (ip2.eq.-1)  ip2=jpp2
      if (ip3.eq.-1)  ip3=jpp3
      if (ip3.eq.   4095)  ip3=icnt

!
      if (necrt.lt.11) then
         ig4s=igg4
         ig3s=igg3
         ig2s=igg2
         ig1s=igg1
      endif

      if (necrt.eq.11) then
         ig4s=igg4
         ig3s=igg3
         ig2s=igg2
         ig1s=ig1srt
      endif

      if (necrt.eq.12)  then
         ig4s=igg4
         ig3s=igg3
         ig2s=ig2srt
         ig1s=ig1srt
      endif

      if (necrt.eq.13) then
         ig4s=igg4
         ig3s=ig3srt
         ig2s=ig2srt
         ig1s=ig1srt
      endif

      if (necrt.eq.14) then
         ig4s=ig4srt
         ig3s=ig3srt
         ig2s=ig2srt
         ig1s=ig1srt
      endif
!
      if (necrt.gt.9) then
         write(app_msg,660) cnumv,idatt,jpp1,jpp2,jpp3,ctypv,cetik,cigty
         call app_log(APP_INFO,app_msg)
 
 660     format('ecrits: ENTRE     ',a2,2x,i10,3x,i5,3x,i2,         3x,i3,4x,a1,4x,a10,3x,a1)
         write(app_msg,670) cnomvar,idat,lip1,ip2,ip3,ctypvar,cetiqet,cigtyp
         call app_log(APP_INFO,app_msg)
 670     format('ecrits: SORTIE    ',a2,2x,i10,3x,i5,3x,i2,         3x,i3,4x,a1,4x,a10,3x,a1)
!
      endif
!
      if (ichck.eq.0)  then
         call app_log(APP_ERROR,'ecrits: Directive LIREN or LIRSR must be called before ECRITS')
         call pgsmabt
      endif
!
!     SI LE NOM EXISTE DANS LA TABLE BATIE PAR L USAGER ALORS
!     LE CHAMP EST MODIFIE
!     EX: ACUMULA(NNI,NNJ)= AMAX1(BAS,AMIN1(HAUT,(ACUMULA(NNI,NNJ) +
!     ECART)*FACT))
!
      call conver(tmpif0, nni, nnj, cnomvar)
!
      if (printsr) then
         call imprims(cnomvar,tmpif0,nni,nnj)
      endif
!
      iun=lnkdiun(idx_ozsrt)
      if (mode.eq.1) then
         if (compression_level.eq.0) then
            cdatyp = 1
         else
            if (npac <= -16) then
            cdatyp = 134
            else if (npac == -32) then
              cdatyp = 133
            else
              cdatyp = 1
            endif
!          call armn_compress_setlevel(compression_level)
        endif

         if (iwrit.eq.+1) then
            if (.not.message) iopc= fstopc('TOLRNC','DEBUGS',.true.)
            rewrit = .true.
!
            ier = fstecr(tmpif0,tmpif0,npkc,iun,idat,ideet,npas,            nni,nnj,nnk,lip1,ip2,ip3,ctypvar,cnomvar,cetiqet,cigtyp,            ig1s,ig2s,ig3s,ig4s,cdatyp,rewrit )
         else
            if (.not.message) iopc= fstopc('TOLRNC','DEBUGS',.true.)
            rewrit = .false.
!
            ier = fstecr(tmpif0,tmpif0,npkc,iun,idat,ideet,npas,            nni,nnj,nnk,lip1,ip2,ip3,ctypvar,cnomvar,cetiqet,cigtyp,            ig1s,ig2s,ig3s,ig4s,cdatyp,rewrit)
         endif
#if defined (SGI) || defined (NEC)
      else if (mode.eq.2) then
         call app_log(APP_ERROR,'ecrits: "MS" file type are not supported with this version of PGSM')
#endif
#if defined (CRAY)
      else if (mode.eq.2)  then
         if (numero.le.0)
            call app_log(APP_ERROR,'ecrits: Wrong directive NUMERO.LE.0 file MS')
         if (numero.ge.(iset-indxs))  then
!            call closms(2)
            call fclos(lnkdiun(idx_ozsrt))
            call app_log(APP_ERROR,'ecrits: End of file overflow')
            call pgsmabt
         endif
!
         if (valid)  then
            istamp=idat
         else
            istamp = 0
         endif


         call putfld(tmpif0,tmpif0,iun,numero,iwrit,nni,nnj,          nbrow,npkc,istamp)
         if (message)then
            write(app_msg,600)ctypvar,cnomvar,lip1,ip2,            ip3,nni,nnj,iun,numero
            call app_log(APP_INFO,app_msg)
         endif
         numero = numero + numdel
!
#endif
!
      else if (mode.eq.3) then

         if (valid) then
            istamp=idat
         else
            istamp=0
         endif
         call putfld(tmpif0,tmpif0,iun,0,iwrit,nni,nnj,         nbrow,npkc,istamp)
         if (message) then
            write(app_msg,610)ctypvar,cnomvar,lip1,ip2,ip3,nni,nnj,iun
            call app_log(APP_INFO,app_msg)
         endif
      else
         if (message) then
            call app_log(APP_ERROR,'ecrits: Unknown file')
         endif
      endif
!
!
      deallocate(tmpif0)
!
      600  format(2x,'ecritur:  Record written ',2(a2,'- '),3(i5,'- '),      'size ',2(i5,'- '),'file MS ',i4,'   REC=',i4)
      610  format(2x,'ecritur:  Record written ',2(a2,'- '),3(i5,'- '),      'size ',2(i5,'- '),'file SEQUENTIEL',i4)
!
      return
      end
