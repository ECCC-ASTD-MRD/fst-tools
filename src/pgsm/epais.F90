!
!**s/p epaisur  difference entre 2 champs de hauteur
!
   subroutine epaisur(iheur, npar, niveau)
      use app
      implicit none

      external ecritur,fstinf,pgsmlir,memoir,fstprm,pgsmabt,      fstcvt,symetri, imprime,loupsou,fstopc,messags,      liraxez
      integer fstinf,pgsmlir,fstprm,fstopc,fstcvt
      integer ezsint, ezqkdef, ezdefset
!
!auteur  p.sarrazin janvier 82  drpn dorval p.q. canada
!revision 4.0.2
!
!   conversion des variables hollerith en caracteres
!   y. chartier -aout 90- drpn dorval quebec.
!
!langage ratfor
!
!objet(epaisur)
!          lire sur fichier standard 2 champs de hauteur
!          prendre la difference entre les 2 champs ecrire le
!          resultat sur fichier approprie(standard,ms,seq)
!
!librairies
!         -source  armnsrc,drpn
!         -objet   pgsmlib,id=armnpjs.
!
!arguments
!   in    iheur   heure des champs (gz)
!   in    npar    nombre de niveaux ( 2)
!   in    niveau  table(2) contenant les 2 niveaux
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!messages
!     record n existe pas sur fichier d entre (epaisur)
!
!modules  fstinf,fstprm,pgsmlir,rgscint,ecritur
!
!appel   via champ
!        call epaisur(iheur,npar,niveau)
!
! - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
#include "llccmm.cdk90"
#include "voir.cdk90"
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
   character *12 cetiket
   character *4 cnomvar
   character *2 ctypvar
   character*1 cigtyp

   real fbidon
   integer iunit

   integer i, niveau(2), iheur, npar, ni, nj, nk, jp1, jp2, jp3, ig1, iopc
   integer ig2,ig3,ig4,irec1,irec2,num1,num2,nn,cnbits,cdatyp,cswa,clng
   integer cdltf,cubc,extra1,extra2,extra3, datev
   integer chkenrpos


      logical sym,symetri
      iunit = 1
!
!     heure ou iheur dans cette routine ne peut-etre -1 heure(tout) pas valide
!
      if (iheur.eq.-1) then
         call app_log(APP_ERROR,'epaisur: HEURE cannot be -1(ALL) with EPAIS directive')
         call pgsmabt
      endif
!
!  identifier le numero de chaque record avec fstinf
!
      call chk_userdate(datev)

!
!  modification de hollerith a caractere
!
      if (etikent(1) .ne. -1) then
         write(cetiket,'(3A4)') (etikent(i), i=1,nwetike)
      else
         cetiket = '        '
      endif

      if (typeent .ne. -1) then
         write(ctypvar, '(A2)') typeent
      else
         ctypvar = '  '
      endif


      cigtyp = ' '

!
      irec1=fstinf(1,ni,nj,nk,datev,cetiket,niveau(1),iheur,ip3ent,      ctypvar,'GZ  ')
      irec2=fstinf(1,ni,nj,nk,datev,cetiket,niveau(2),iheur,ip3ent,      ctypvar,'GZ  ')
      if (irec2 .lt. 0 .or. irec1 .lt. 0) then
         call app_log(APP_ERROR,'epaisur: Record does not exist in input file')
         return
      endif

      if (nk.gt.1) then
         call app_log(APP_ERROR,'epaisur: PGSM does not accept 3 dimension fields (NK>1)')
         call pgsmabt
      endif
!
!     identifier parametres pour champ 1
!
      ier = fstprm( irec1, dat,deet,npas,ni, nj, nk, cnbits,cdatyp,      jp1,jp2, jp3,ctypvar,cnomvar,cetiket,cigtyp,      ig1,ig2,ig3,ig4,cswa, clng, cdltf, cubc,       extra1, extra2, extra3)

      if (ier .lt. 0) then
         call app_log(APP_ERROR,'epaisur: FSTPRM failed')
      endif
!
!     verifier si grille gaussienne ni doit etre pair
!
      if (cigtyp.eq.'G'.and.mod(ni,2).ne.0) then
         call messags(ni)
      endif

!
!     lire champ no 1
!
      allocate(tmpif1(ni,nj))
      if (.not.message) then
         iopc= fstopc('TOLRNC','DEBUGS',.true.)
      endif

      call chk_userdate(datev)
!
      num1 = pgsmlir(tmpif1,1,ni,nj,nk,datev,cetiket,jp1,jp2,jp3,ctypvar, 'GZ  ', cigtyp)
!
      if (printen) then
         call imprime(cnomvar,tmpif1,ni,nj)
      endif
!
!     identifier parametres pour champ 2
!
      ier = fstprm( irec2, dat,deet,npas,ni, nj, nk, cnbits,cdatyp,      jp1,jp2, jp3,ctypvar,cnomvar,cetiket,cigtyp,       ig1,ig2,ig3,ig4, cswa,clng,cdltf,cubc,extra1,extra2,extra3)
      if (ier .lt. 0) then
         call app_log(APP_ERROR,'epaisur: FSTPRM failed')
      endif
!
!     verifier si grille gaussienne ni doit etre pair
!
      if (cigtyp.eq.'G'.and.mod(ni,2).ne.0)  then
         call messags(ni)
      endif
!
!     lire champ 2
!
      allocate(tmpif2(max0(li,ni),max0(nj,lj)))
      if (.not.message)  then
         iopc= fstopc('TOLRNC','DEBUGS',.true.)
      endif

      call chk_userdate(datev)

      num2 = pgsmlir(tmpif2,1,ni,nj,nk,datev,cetiket,jp1,jp2,jp3,  ctypvar, 'GZ  ', cigtyp)
      if (printen)  call imprime(cnomvar,tmpif1,ni,nj)
!
!  difference entre les deux champs
!
      nn = ni*nj
      call loupsou(tmpif1,tmpif2,nn)
!
!     interpolation horizontale
!
      if (cigtyp == 'A' .or. cigtyp == 'B' .or. cigtyp == 'G') then
         if (ig1 /= 0) sym = symetri(cnomvar)
      endif

      if (cgrtyp.eq.'*') then
         ier = chkenrpos(1,2,ig1,ig2,ig3)
         call ecritur(tmpif1,npack,dat,deet,npas,ni,nj,nk,         niveau(1),niveau(2),iheur,         ctypvar,'DZ  ',cetiket,cigtyp,ig1,ig2,ig3,ig4)
      else
         gdin = ezqkdef(ni, nj, cigtyp, ig1, ig2, ig3, ig4, iunit)
         ier = ezdefset(gdout, gdin)
         ier = ezsint(tmpif2, tmpif1)
!
!
         jp1 = niveau(1)
         jp2 = niveau(2)
         jp3 = iheur
!
!     ecrire sur fichier standard,ms,sequentiel
!
         call ecritur(tmpif2,npack,dat,deet,npas,li,lj,nk,jp1,jp2,jp3,         ctypvar,'DZ  ',cetiket,cgrtyp,lg1,lg2,lg3,lg4)

      endif

      deallocate(tmpif1)
      deallocate(tmpif2)
!
      return
      end

