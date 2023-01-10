!
!**s/p macpcp interpole ajustement convectif ou precipitation
!
   subroutine macpcp(cnom,npar,itime)
      use app
      implicit none
!
!auteur  p.sarrazin fevrier 82  drpn dorval p.q. canada
!
!revision 4.0.2
!     conversion variables type hollerith -> type caractere
!   y. chartier -aout 90- drpn dorval quebec
!
!revision 5.2
!   support des grilles sources de type Z
!
!langage ratfor
!
!objet(macpcp)
!          extraire la difference entre deux champs dont les
!          heures  sont differents
!          avec routine fstinf on extrait le record necessaire pour
!          routine fstprm qui identifit les parametres utilises
!          par routine lire
!          on reserve la memoire pour les deux champs de travail
!          routine ecritur identifit la sorte de fichier utilisee
!          pour ecrire
!
!librairies
!         -source  armnsrc,drpn
!         -objet   pgsmlib,id=armnpjs.
!
!arguments
!  in    nom    nom du champ
!  in    npar    nombre de locations utilisees dans itime
!  in    itime   table contenant 2 heures ou niveaux
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!messages
!          'mauvais appel a champ il devrait y avoir 3 arguments'
!           record n'existe pas sur fichier d'entre (macpcp)
!
!modules
      external ecritur,fstinf,pgsmlir,memoir,fstprm,symetri
      external loupneg,loupsou,fstopc,pgsmabt,imprime,messags,fstcvt
      external liraxez
      integer fstprm,fstinf,pgsmlir,fstopc,fstcvt
!
!appel     via champ
!         call macpcp(nom,npar,itime)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
#include "voir.cdk90"
#include "dummys.cdk90"
#include "llccmm.cdk90"
#include "accum.cdk90"
#include "param.cdk90"
#include "indptr.cdk90"
#include "grilles.cdk90"
#include "lires.cdk90"
#include "ecrires.cdk90"
#include "dates.cdk90"
#include "packin.cdk90"
#include "gdz.cdk90"
!
      character *12 cetike,cetiket
      character *4 cnomvar, cnom
      character *1 cigtyp
      character *2 ctypvar

      real valeur
      real fbidon

      real, dimension(:), allocatable :: lclif1, lclif2

      integer i, itime(2),ig1,ig2,ig3,ig4,ip1,irec1,irec2
      integer jp1,jp2,jp3,lilj,ni,nj,nk,nn,npar,num1,num2,iopc
      integer cdatyp,cnbits
      integer cswa, clng, cdltf, cubc, extra1, extra2, extra3
      integer ezqkdef, ezdefset, ezsint, chkenrpos, datev
      integer iunit
      logical symetri,sym

      iunit = 1
      nk = 1
!
      if (npar.ne.2) then
         if (message) call app_log(APP_ERROR,'macpcp: Wrong call to MACPCP, must have 3 arguments')
         return
      endif
!
!     identifier le numero de chaque record avec fstinf
!
!     # doit etre egal a zero dans fichier d'ENTRE
      ip1=0
      call chk_userdate(datev)
!
!     modification de hollerith a caractere
!
      cnomvar = cnom
      if (etikent(1) .ne. -1) then
         write(cetiket,'(3A4)') (etikent(i), i=1,nwetike)
      else
         cetiket = '            '
      endif

      if (typeent .ne. -1) then
         write(ctypvar, '(A2)') typeent
      else
         ctypvar = '  '
      endif


      irec1=fstinf(1,ni,nj,nk,datev,cetiket,ip1,      itime(1),ip3ent,ctypvar,cnomvar)
      irec2=fstinf(1,ni,nj,nk,datev,cetiket,ip1,      itime(2),ip3ent,ctypvar,cnomvar)

!     #  record n'EXISTE PAS
      if (irec2 .lt. 0 .or.irec1 .lt. 0) then
         call app_log(APP_ERROR,'macpcp: Record does not exist in input file, check IP2-IP3ENT IP1=0')
         return
      endif

      if (nk.gt.1) then
         call app_log(APP_ERROR,'macpcp: PGSM does not accept 3 dimension fields (NK>1)')
         call pgsmabt
      endif
!
!
!     identifier parametres pour champ 1
!
      ier = fstprm( irec1, dat,deet,npas,ni, nj, nk, cnbits,cdatyp,      jp1,jp2, jp3,ctypvar,cnomvar,cetike,cigtyp,       ig1,ig2,ig3,ig4,      cswa, clng, cdltf, cubc, extra1, extra2, extra3)
      if (ier .lt. 0) call app_log(APP_ERROR,'macpcp: FSTPRM failed')
!
!     verifier si grille gaussienne ni doit etre pair
!
      if (cigtyp.eq.'G'.and.mod(ni,2).ne.0)  call messags(ni)
!
!
!     lire champ no 1
!
      allocate(lclif1(ni*nj))
      if (.not.message) iopc= fstopc('TOLRNC','DEBUGS',.true.)
!      if (userdate .eq. oui .or. userdate .eq. non) date=date2

      num1 = pgsmlir(lclif1,1,ni,nj,nk,datev,cetiket,jp1,jp2,jp3,      ctypvar,cnomvar,cigtyp)

      if (printen)  call imprime(cnomvar,lclif1,ni,nj)
!
!     identifier parametres pour champ 2
!
      ier = fstprm(irec2, dat,deet,npas,ni, nj, nk, cnbits,cdatyp,      jp1,jp2, jp3,ctypvar,cnomvar,cetike,cigtyp,      ig1,ig2,ig3,ig4,      cswa, clng, cdltf, cubc, extra1, extra2, extra3)
      if (ier .lt. 0) call app_log(APP_ERROR,'macpcp: FSTPRM failed')
!
!     verifier si grille gaussienne ni doit etre pair
!
      if (cigtyp.eq.'G'.and.mod(ni,2).ne.0)  call messags(ni)
!
!     lire champ 2
!
      allocate(lclif2(max0(li*lj,ni*nj)))
      if (.not.message) iopc= fstopc('TOLRNC','DEBUGS',.true.)
!      if (date .eq. 0 .or. date .eq. 1) date=date2
      num2 = pgsmlir(lclif2,1,ni,nj,nk,datev,cetiket,jp1,jp2,jp3,      ctypvar,cnomvar,cigtyp)
      if (printen)  call imprime(cnomvar,lclif2,ni,nj)
!
!     difference entre les deux champs
!
      nn = ni*nj
      call loupsou(lclif1,lclif2,nn)
!
!
!     interpolation horizontale
!
      if (cgrtyp.eq.'*') then
         ier = chkenrpos(1,2,ig1,ig2,ig3)
      else
!     #  variable symetrique oui=.true.
         if (cigtyp == 'A' .or. cigtyp == 'B' .or. cigtyp == 'G') then
            if (ig1 /= 0) sym = symetri(cnomvar)
         endif
         gdin = ezqkdef(ni, nj, cigtyp, ig1, ig2, ig3, ig4, iunit)
         ier = ezdefset(gdout, gdin)
         ier = ezsint(lclif2, lclif1)
      endif
!
!     #   jp1 - contient heure du premier champ
!     #   jp2 - contient heure du deuxieme champ
      jp1 = itime(1)
      jp2 = itime(2)
      jp3 = 0
!
!     deet et npas contiennent les dernieres valeurs lues dans le dernier record
!
!     eliminer toutes les valeurs du champ negative precip
!     et acumulateur d'ajustement ne peuvent etre negatif
!
      lilj=li*lj
      valeur=0.0
      call loupneg(lclif2,valeur,1,lilj,1)
!
!     ecrire sur fichier standard,ms,sequentiel
!
      if (cgrtyp.eq.'*') then
         call ecritur(lclif1,npack,dat,deet,npas,ni,nj,nk,jp1,jp2,jp3,         ctypvar,cnomvar,cetike,cigtyp,ig1,ig2,ig3,ig4)
      else
         call ecritur(lclif2,npack,dat,deet,npas,li,lj,nk,jp1,jp2,jp3,         ctypvar,cnomvar,cetike,cgrtyp,lg1,lg2,lg3,lg4)
      endif
!
!     remetre espace des champs de travail
!
      deallocate(lclif2)
      deallocate(lclif1)
!
      return
      end


