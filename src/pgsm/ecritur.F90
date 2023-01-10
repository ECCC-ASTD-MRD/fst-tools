!**S/P ECRITUR   ECRIRE SUR FICHIER STANDARD, MS, SEQUENTIEL
!
   subroutine ecritur(fld,npac,idat,deet,npas,ni,nj,nk,ip1,ip2,ip3,ctypvar,cnomvar,cetiket,cgtyp,llg1,llg2,llg3,llg4)
      use app
      implicit none

      external conver,fstecr,fclos,memoir,pgsmabt,      imprims,fstopc,messags,fstcvt,putfld
      integer fstopc,fstcvt,fstecr
!
!AUTEUR  P.SARRAZIN  JANVIER 82  DRPN DORVAL P.Q. CANADA
!REVISION 4.0.2
!   MODIF. VARIABLES TYPE HOLLERITH EN CARACTERE
!   Y. CHARTIER -AOUT 90- DRPN DORVAL QUEBEC.
!Revision 5.0.0
!   Elimination appels a fstcvt pour etiksrt
!   Y. Chartier -mai  91- drpn Dorval Quebec
!
!LANGAGE RATFOR
!
!OBJET(ECRITUR)
!          ECRIRE SUR FICHIER STANDARD AVEC ROUTINE FSTECR
!          ECRIRE SUR FICHIER MS AVEC PUTFLD
!          ECRIRE SUR FICHIER SEQUENTIEL AVEC PUTFLD
!
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!ARGUMENTS
!   IN    fld   fld(NI,NJ,NK) A ECRIRE
!   IN    NPAC    COMPACTION DU DATA DANS fld
!   IN    IDAT    DATE DU fld (CMC STAMP)
!   IN    DEET    PAS DE TEMPS ENTIER SECONDES
!   IN    NPAS    NUMERO DU PAS DE TEMPS
!   IN    NI      1 ER DIMENSION DU fld
!   IN    NJ      2 IEM DIMENSION DU fld
!   IN    NK      3 IEME DIMENSION DU fld
!   IN    IP1     NORMALEMENT NIVEAU DU fld
!   IN    IP2     HEURE DU fld
!   IN    IP3ENT     LIBRE
!   IN    TYPEENT   TYPE DU fld 1 CARACTERE
!   IN    NOM     NOM DU fld 2 CARACTERES
!   IN    ETIKE   ETIQUETTE 1 MOT CDC (USAGER)
!   IN    GRTYPE  TYPE DE GRILLE 1 CARACTERE
!   IN    LLG1    DESCRIPTEUR DE GRILLE
!   IN    LLG2    PS - LLG1 POSITION J DU POLE
!   IN    LLG3         LLG2 POSITION I DU POLE
!   IN    LLG4         LLG3 DGRW*100
!                      LLG4 D60 HETOMETRE(0-36000)
!                 LAT-LON  LLG1- DLAT*100
!                          LLG2- DLON*100
!                          LLG3- (90-LAT)*100 0=POLE SUD
!                          LLG4- (LON*100) (0-36000) LONGITUDE COIN
!                 GAUSSIEN  LLG1= 1 HEMISPHERE NORD
!                           LLG1= 2 HEMISPHERE SUD
!                           LLG1= 3 GLOBALE
!
!MESSAGES
!         FIN DU FICHIER CA DEBORDE
!         MAUVAISE DIRECTIVE NUMERO=0 FICHIER MS
!         FICHIER INCONNU ROUTINE ECRITUR
!
!MODULES  FSTECR,PUTFLD
!
!APPEL
!         CALL ECRITUR(fld,NPAC,IDAT,DEET,NPAS,NI,NJ,NK,IP1,IP2,IP3,
!                      TYPEV,NOM,ETIKE,GRTYPE,LLG1,LLG2,LLG3,LLG4)
!
!- -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!
#include "lires.cdk90"
#include "voir.cdk90"
#include "ecrires.cdk90"
#include "llccmm.cdk90"
#include "indptr.cdk90"
#include "enrege.cdk90"
#include "packin.cdk90"
#include "dates.cdk90"
#include "dummys.cdk90"
#include "idents.cdk90"
#include "qqqfilt.cdk90"
#include "styles.cdk90"
#include "lnkflds.cdk90"
!
!-----------------------------------------------------------------
!
      character *24 chaine
      character *12 cetiket, cetksrt
      character *4 cnomvar
      character *2 ctypvar
      character *1 cgtyp


      character*12 letiksrt
      character*4 lnomvar
      character*2 ltypsrt

      character*72 form1,form2

      integer i, npac,idat,idatv,npas,ni,nj,nk,ip1,ip2,ip3,deet
      real fld(ni,nj,nk)
      real dummy(2)
      integer llg1,llg2,llg3,llg4,iun,istamp,ip3o,ip2o
      integer cdatyp,iopc,ier,gdout, datev
      logical rewrit

      integer gdll,ezgetgdout
      external gdll,ezgetgdout

      integer local_npac

      real*8 delta_t

      if (etiksrt(1) .eq. -1) then
         cetksrt = cetiket
      else
         write(cetksrt,'(3A4)') (etiksrt(i), i=1,nwetiks)
      endif

      if (typesrt .eq. -1) then
         ltypsrt = ctypvar
      else
         write(ltypsrt, '(A2)') typesrt
      endif

!
      if (ip3srt.ne.-1) then
         ip3o=ip3srt
      else
         ip3o=ip3
      endif
!
      if (ip2srt.ne.-1) then
         ip2o=ip2srt
      else
         ip2o=ip2
      endif

!      ltypsrt = ctypsrt
      lnomvar = cnomvar
      letiksrt = cetksrt

      if(npac == 1023) then
        local_npac = npack_orig
      else
        local_npac = npac
      endif

!
!     SI LE NOM EXISTE DANS LA TABLE BATIT PAR L USAGER ALORS
!     LE fld EST MODIFIE  EX: fld(NI,NJ) = (fld(NI,NJ)+ECART)*FACTEUR
!
      call conver(fld, ni, nj, cnomvar)
!
!
!     filtrage du fld de sortie si le fld n'est pas un stream latlon
!

      if (fltoggle(2)) then
        if (cgtyp.eq.'Y') then
         call app_log(APP_WARNING,'ecritur: Cannot filter fields on Y grid')
        else
         call app_log(APP_INFO,'ecritur: Field filtered on write')
!          call statfld4 (fld,cnomvar,0,'AVANFFLT',ni,nj,1,ni,nj,1,0,0
!     &         ,0)
          call filtre (fld, NI, NJ, 0, fltntimes(2), fltlist(1,2), fltwgtlng(2))
!          call statfld4 (fld,cnomvar,1,'APRESFLT',ni,nj,1,ni,nj,1,0,0
!     &         ,0)
        endif
      endif

!
!
!


      if (printsr)  then
         call imprims(cnomvar,fld,ni,nj)
      endif
!
      iun=lnkdiun(idx_ozsrt)
      if (mode.eq.1) then
!     IWRIT=+1  SORTI(STD,500,R)
         if (compression_level.eq.0) then
            cdatyp = 1
         else
            if (local_npac < 0 .and. local_npac >= -16) then
             cdatyp = 134
            else if (local_npac == -32) then
               cdatyp = 133
            else
               cdatyp = 1
            endif
!          call armn_compress_setlevel(compression_level)
         endif


         if (iwrit.eq.+1) then
            if (.not.message) then
               iopc= fstopc('TOLRNC','DEBUGS',.true.)
            endif
            rewrit = .true.
!
            ier = fstecr(fld,dummy,local_npac,iun,idat,deet,npas,            ni,nj,nk,ip1,ip2o,ip3o,ltypsrt,cnomvar,cetksrt,            cgtyp,llg1,llg2,llg3,llg4,cdatyp,rewrit )
         else
            if (.not.message) then
               iopc= fstopc('TOLRNC','DEBUGS',.true.)
            endif
            rewrit = .false.
            ier = fstecr(fld,dummy,local_npac,iun,idat,deet,npas,            ni,nj,nk,ip1,ip2o,ip3o,ltypsrt,cnomvar,cetksrt,            cgtyp,llg1,llg2,llg3,llg4,cdatyp,rewrit )
         endif
!
      else
         if (mode.eq.2) then
            call app_log(APP_WARNING,'ecritur: "MS" file type are not supported anymore')
         else
         endif
!
         if (mode.eq.3.or.mode.eq.4) then
            if (mode.eq.4) then
               cdatyp = 1
               write (chaine, 10) ltypsrt,lnomvar,letiksrt,cgtyp
 10            format(a2,2x,a4,a12,a1,3x)
               write (iun) npac, idat, deet, npas, ni, nj, nk,                ip1, ip2o, ip3o, llg1, llg2, llg3, llg4, cdatyp,               chaine
            endif

            write(iun) fld
            if (message) then
               write(app_msg,610)ltypsrt,cnomvar,ip1,ip2o,ip3o,ni,nj,iun
            endif
         else if (mode.eq.5) then
            if (valid) then
               call chk_userdate(datev)
               if (datev .ne. -1) then
                  istamp = datev
               else
                  istamp = idat
               endif
            else
               istamp=0
            endif

            delta_t = deet*npas/3600.0
            call incdatr(idatv,idat,delta_t)

            gdout = ezgetgdout()
            if (gdout .lt.0) gdout = 0
            if (cnomvar(1:2).ne.'LA') then
              if (associated(tmplat)) then
                deallocate(tmplat)
                nullify(tmplat)
                allocate(tmplat(ni,nj))
              endif
            endif

            if (cnomvar(1:2).ne.'LO') then
              if (associated(tmplon)) then
                deallocate(tmplon)
                nullify(tmplon)
                allocate(tmplon(ni,nj))
              endif
            endif

            ier = gdll(gdout, tmplat,tmplon)
            call pgsmwr(2,fld,ni,nj,nk,qcform,qposition,qitems,qcsepar,cnomvar,ctypvar,cetiket,            idat,idatv,dateform,ip1,ip2,ip3,tmplat,tmplon)

!
         else
            if (message) then
               call app_log(APP_ERROR,'ecritur: Unknown file')
            endif
         endif
      endif
!
!
 600  format(2x,' ENREG.ECRIT ',2(a2,'- '),3(i5,'- '),      'TAILLE ',2(i5,'- '),'FICHIER MS ',i4,'   REC=',i4)
 610  format(2x,' ENREG.ECRIT ',2(a2,'- '),3(i5,'- '),      'TAILLE ',2(i5,'- '),'FICHIER SEQUENTIEL',i4)
      return
      end

   subroutine iecritur(fld,npac,idat,deet,npas,ni,nj,nk,ip1,ip2,ip3,ctypvar,cnomvar,cetiket,cgtyp,llg1,llg2,llg3,llg4)
      use app
      implicit none
      
      external conver,fstecr,fclos,memoir,pgsmabt,      imprims,fstopc,messags,fstcvt,putfld
      integer fstopc,fstcvt,fstecr
#include "lires.cdk90"
#include "voir.cdk90"
#include "ecrires.cdk90"
#include "llccmm.cdk90"
#include "indptr.cdk90"
#include "enrege.cdk90"
#include "dates.cdk90"
#include "packin.cdk90"
#include "dummys.cdk90"
#include "idents.cdk90"
#include "qqqfilt.cdk90"
#include "styles.cdk90"
#include "lnkflds.cdk90"
!
!-----------------------------------------------------------------
!
      character *24 chaine
      character *12 cetiket, cetksrt
      character *4 cnomvar
      character *2 ctypvar
      character *1 cgtyp


      character*12 letiksrt
      character*4 lnomvar
      character*2 ltypsrt

      character*72 form1,form2

      integer i, npac,idat,idatv,npas,ni,nj,nk,ip1,ip2,ip3,deet, datev
      integer fld(ni,nj,nk)
      real dummy(2)
      integer llg1,llg2,llg3,llg4,iun,istamp,ip3o,ip2o
      integer cdatyp,iopc,ier,gdout,local_npac
      logical rewrit

      integer gdll,ezgetgdout
      external gdll,ezgetgdout

      real*8 delta_t

      if (etiksrt(1) .eq. -1) then
         cetksrt = cetiket
      else
         write(cetksrt,'(3A4)') (etiksrt(i), i=1,nwetiks)
      endif

      if (typesrt .eq. -1) then
         ltypsrt = ctypvar
      else
         write(ltypsrt, '(A2)') typesrt
      endif

!
      if (ip3srt.ne.-1) then
         ip3o=ip3srt
      else
         ip3o=ip3
      endif
!
      if (ip2srt.ne.-1) then
         ip2o=ip2srt
      else
         ip2o=ip2
      endif

!      ltypsrt = ctypsrt
      lnomvar = cnomvar
      letiksrt = cetksrt
!
      if(npac == 1023) then
        local_npac = npack_orig
      else
        local_npac = npac
      endif

      iun=lnkdiun(idx_ozsrt)
      if (mode.eq.1) then
!     IWRIT=+1  SORTI(STD,500,R)
         if (compression_level.eq.0) then
            cdatyp = 2
         else
            if (local_npac < 0 .and. local_npac >= -16) then
               cdatyp = 130
            else
               cdatyp = 2
            endif
         endif


         if (iwrit.eq.+1) then
            if (.not.message) then
               iopc= fstopc('TOLRNC','DEBUGS',.true.)
            endif
            rewrit = .true.
!
            ier = fstecr(fld,dummy,local_npac,iun,idat,deet,npas,ni,nj,nk,ip1,ip2o,ip3o,ltypsrt,cnomvar,cetksrt, cgtyp,llg1,llg2,llg3,llg4,cdatyp,rewrit )
         else
            if (.not.message) then
               iopc= fstopc('TOLRNC','DEBUGS',.true.)
            endif
            rewrit = .false.
            ier = fstecr(fld,dummy,local_npac,iun,idat,deet,npas, &
               ni,nj,nk,ip1,ip2o,ip3o,ltypsrt,cnomvar,cetksrt, cgtyp,llg1,llg2,llg3,llg4,cdatyp,rewrit )
         endif
!
      else
         if (mode.eq.2) then
            call app_log(APP_WARNING,'ecritur: "MS" file type are not supported anymore')
         else if (mode.eq.3.or.mode.eq.4) then
            if (mode.eq.4) then
               cdatyp = 1
               write (chaine, 10) ltypsrt,lnomvar,letiksrt,cgtyp
 10            format(a2,2x,a4,a12,a1,3x)
               write (iun) npac, idat, deet, npas, ni, nj, nk,  ip1, ip2o, ip3o, &
                  llg1, llg2, llg3, llg4, cdatyp, chaine
            endif

            write(iun) fld
            if (message) then
               write(app_msg,610)ltypsrt,cnomvar,ip1,ip2o,ip3o,ni,nj,iun
            endif
         else if (mode.eq.5) then
            if (valid) then
               call chk_userdate(datev)
               if (datev .ne. -1) then
                  istamp = datev
               else
                  istamp = idat
               endif
            else
               istamp=0
            endif

            delta_t = deet*npas/3600.0
            call incdatr(idatv,idat,delta_t)
         else
            if (message) then
               call app_log(APP_WARNING,'ecritur: Unknown file')
            endif
         endif
      endif
!
!
 600  format(2x,'ecritur:  Record written ',2(a2,'- '),3(i5,'- '),      'size ',2(i5,'- '),'file MS ',i4,'   REC=',i4)
 610  format(2x,'ecritur:  Record written ',2(a2,'- '),3(i5,'- '),      'size ',2(i5,'- '),'file SEQUENTIEL',i4)
      return
      end
