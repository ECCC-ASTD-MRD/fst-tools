!
!**s/p chmpdif  interpole difference entre deux champs
!
#include "defin.cdk90"
      subroutine chmpdif (noment,nomsrt,ip1tab,ip2tab,ip3tab,ip1s, ip2s, ip3s)
      use app
   implicit none
      external ecritur,fstinf,pgsmlir,memoir,fstprm,symetri
      external loupneg,loupsou,fstopc,argdims,pgsmabt,imprims,grille2
      external imprime,messags,fstcvt
      external liraxez
      integer  fstinf,pgsmlir,fstprm,fstopc,fstcvt
      integer ezqkdef, ezsint, ezdefset
      integer ip1s, ip2s, ip3s
      integer incip1
!
!auteur  p.sarrazin juillet 86  drpn dorval p.q. canada
!revision
!    4.0.2 conversion des variables hollerith en caracteres
!          y. chartier aout 90 drpn dorval quebec
!    5.2   Support des grilles sources Z
!    5.7.7 npas et deet prennent la valeur du premier champ
!
!langage ratfor
!
!objet(chmpdif)
!          extraire la difference entre deux champs par rapport
!          a ip1, ip2, ou ip3 determine par l'usager:
!          chmpdif ("gz","dz",[500,1000],6,0) liste sur ip1
!          chmpdif ("pr","pr",0,[0,12],0) liste sur ip2
!          chmpdif ("tz","zt",0,12,[1,2,3,4] liste sur ip3
!          on peut changer le nom du resultat sur le fichier
!          de sortie apres interpolation de la difference
!          avec routine fstinf on extrait le record necessaire pour
!          routine fstprm qui identifie les parametres utilises
!          par routine lire
!          on reserve la memoire pour les deux champs de travail
!          routine ecritur identifit la sorte de fichier utilise
!          pour ecrire
!
!librairies
!         -source  armnsrc,drpn
!         -objet   pgsmlib,id=armnpjs.
!
!arguments
!  in    noment  nom du champ sur fichier d'entre
!  out   nomsrt  nom du champ sur fichier de sorti defaut=-1
!                nomsrt=noment
!  in    ip1tab  peut etre une liste nombre pair ou le niveau du
!                champ d'entre
!  in    ip2tab  peut etre une liste nombre pair ou l'heure du
!                champ d'entre
!  in    ip3tab  peut etre une liste nombre pair valeur determinee
!                par l'usager sur le champ d'entre
!
!
!implicites
!
!messages
!          'mauvais appel a champdif il devrait y avoir 5 arguments'
!          'record n existe pas sur fichier d entre (chmpdif)'
!
!modules  fstinf,memoir,fstprm,pgsmlir,symetry,rgscint,ecritur,argdims
!         imprime,loupsou,pgsmabt
!
!appel     via directive chmpdif (noment,nomsrt,ip1tab,ip2tab,ip3tab)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
#include "voir.cdk90"
#include "accum.cdk90"
#include "champs.cdk90"
#include "indptr.cdk90"
#include "grilles.cdk90"
#include "lires.cdk90"
#include "llccmm.cdk90"
#include "ecrires.cdk90"
#include "dates.cdk90"
#include "packin.cdk90"
#include "dummys.cdk90"
#include "gdz.cdk90"
#include "styles.cdk90"
!
      character*12 cetiket
      character*4 cnoment, cnomsrt
      character*2 ctypvar
      character*1 cigtyp

      integer ig1,ig2,ig3,ig4,irec1,irec2
      integer jp1,jp2,jp3,jp01,jp02,jp03,jp11,jp12,jp13,ni,nj,nk,nn
      integer lesips(3),jp(3)
      integer ip1tab(40),ip2tab(40),ip3tab(40),noment,nomsrt
      integer lcl_ip1tab(40)
      integer num1,num2,num3,nloop,dat1,dat2,deet1,deet2,npas1,npas2
      integer datsrt,deetsrt,npassrt,i,j,k,ii,jj,kk,iloop,n, datev
      integer ni1,ni2,nj1,nj2,nk1,nk2,cnbits,cdatyp,cswa, clng, cdltf, cubc, extra1, extra2, extra3
      integer argdims
      logical symetri,sym
      integer iunit, chkenrpos
      real fbidon, p
      character*8 string
      integer npts

      real, dimension(:),allocatable :: lclif1, lclif2

!
      iunit = 1
      nk = 1
      if (npar.lt. 5) then
         if (message) then
            call app_log(APP_WARNING,'chmpdif: Directive CHMPDIF should have at least 5 arguments')
         endif
         return
      endif

      if (.not.associated(tmplat).and.cgrtyp.ne.'*') then
         if (message) then
            call app_log(APP_WARNING,'chmpdif: Grid not defined, will use PS(2805)')
         endif
         ngr=8
         call grille2(3,51,55,26.,28.,381000.,350.,1)
      endif
!
!   trouver nombre d'arguments dans une liste (ip1,ip2,ip3)
!
      num1=argdims(3)
      num2=argdims(4)
      num3=argdims(5)
!
      nloop=0
!
      if (num1.gt.1) nloop=num1
      if (num2.gt.1) nloop=num2
      if (num3.gt.1) nloop=num3
!
      if (nloop.eq.0) then
         call app_log(APP_WARNING,'chmpdif: No list of [IP1], [IP2], [IP3]')
         return
      endif
!
!     Ajustement pour IP1s reels


      if (argdims(3) > 1) then
         if (ip1tab(1) > 1000000 .and. ip1tab(2) < 0) then
            do i=1,num1,2
               p = transfer(ip1tab(i), p)
               call convip_plus(lcl_ip1tab(i/2+1), p, &
                                -1*ip1tab(i+1)-1000, &
                                ip1style, string, .false.)
            enddo
            num1 = num1 / 2
            nloop = nloop / 2
         else
            do i=1,num1
               lcl_ip1tab(i) = ip1tab(i)
            enddo
         endif
      else
         do i=1,num1
            lcl_ip1tab(i) = ip1tab(i)
         enddo
      endif

!
!     verifier si il ,y a plus d'une liste
!
      if (num1.gt.1.and.num2.gt.1) then
         call app_log(APP_WARNING,'chmpdif: IP1 and IP2 contain a variable list')
         return
      endif

      if (num1.gt.1.and.num3.gt.1) then
         call app_log(APP_WARNING,'chmpdif: IP1 and IP3 contain a variable list')

         return
      endif

      if (num2.gt.1.and.num3.gt.1) then
         call app_log(APP_WARNING,'chmpdif: IP2 and IP3 contain a variable list')
         return
      endif

!     execution de chaque paire dans la liste
!

      do iloop=1,nloop,2
         if (num1.gt.1) then
            i=iloop
            ii=iloop+1
            j=1
            jj=1
            k=1
            kk=1
         endif
!
         if (num2.gt.1) then
            j=iloop
            jj=iloop+1
            i=1
            ii=1
            k=1
            kk=1
         endif
!
         if (num3.gt.1) then
            k=iloop
            kk=iloop+1
            j=1
            jj=1
            i=1
            ii=1
         endif
!
         call chk_userdate(datev)
!
!     identifier le numero de chaque record avec fstinf
!
!     modification de hollerith a caractere
!
         write(cnoment,'(A4)') noment
         write(cnomsrt,'(A4)') nomsrt

         if (etikent(1) .ne. -1) then
            write(cetiket,'(3A4)') (etikent(n), n=1,nwetike)
         else
            cetiket = '        '
         endif

         if (typeent .ne. -1) then
            write(ctypvar, '(A2)') typeent
         else
            ctypvar = '  '
         endif


         irec1=fstinf(1,ni1,nj1,nk1,datev,cetiket,lcl_ip1tab(i),ip2tab(j),ip3tab(k),ctypvar,cnoment)
         irec2=fstinf(1,ni2,nj2,nk2,datev,cetiket,lcl_ip1tab(ii),ip2tab(jj),ip3tab(kk),ctypvar,cnoment)
         if (irec2 .lt. 0 .or.irec1 .lt. 0) then
         call app_log(APP_WARNING,'chmpdif: Record does not exist in input file, check NOMVAR,IP1,IP2,IP3')
         return
         endif
!
!
!
         if (nk2 .gt. 1 .or. nk1.gt.1  ) then
            call app_log(APP_ERROR,'chmpdif: PGSM does not allow 3 dimension fields (NK>1)')
             call pgsmabt
         endif
!
!     verifier dimension des deux champs d'entre
!
         if (ni1.ne.ni2.or.nj1.ne.nj2.or.nk1.ne.nk2) then
            call app_log(APP_WARNING,'chmpdif: Dimensions of the 2 fields differ, check input file NI,NJ,NK')
            return
         endif
!
!
!
!     identifier parametres pour champ 1
!
         ier = fstprm( irec1, dat1,deet1,npas1,         ni, nj, nk, cnbits,cdatyp,         jp01,jp02, jp03,ctypvar,cnoment,cetiket,         cigtyp, ig1,ig2,ig3,ig4,          cswa, clng, cdltf, cubc, extra1, extra2, extra3)
         if (ier .lt. 0) then
            call app_log(APP_ERROR,'chmpdif: FSTPRM failed')
         endif
!
!     verifier si grille gaussienne ni doit etre pair
!
         if (cigtyp.eq.'G'.and.mod(ni,2).ne.0)  then
            call messags(ni)
         endif
!
!
!     lire champ no 1
!
         allocate(lclif1(ni*nj))
         if (.not.message) ier = fstopc('TOLRNC','DEBUGS',.true.)
         call chk_userdate(datev)
!
         irec1=pgsmlir(lclif1,1,ni,nj,nk,datev,cetiket,lcl_ip1tab(i),         ip2tab(j), ip3tab(k),ctypvar,cnoment,cigtyp)
!
         if (printen)  call imprime(cnoment,lclif1,ni,nj)
!
!     identifier parametres pour champ 2
!
         ier = fstprm( irec2, dat2,deet2,npas2,ni, nj, nk,          cnbits,cdatyp,         jp11,jp12,jp13,ctypvar,cnoment,cetiket,         cigtyp,ig1,ig2,ig3,ig4,         cswa, clng, cdltf, cubc, extra1, extra2, extra3)
         if (ier .lt. 0) call app_log(APP_ERROR,'chmpdif: FSTPRM failed')
!
!
!     verifier si grille gaussienne ni doit etre pair
!
         if (cigtyp.eq.'G'.and.mod(ni,2).ne.0)  call messags(ni)
!
!
!     si les deux variables identiques on la transferre
!     dans le fichier sorti
!
         datsrt=dat2
!         if (dat1.eq.dat2) datsrt=dat1
         deetsrt=deet2
!         if (deet1.eq.deet2) deetsrt=deet1
         npassrt=npas2
!         if (npas1.eq.npas2) npassrt=npas1
!
!     lire champ 2
!

         npts = li*lj
         if (npts < ni*nj) then
            npts = ni*nj
         endif

         allocate(lclif2(npts))
         if (.not.message) ier = fstopc('TOLRNC','DEBUGS',.true.)
         call chk_userdate(datev)
!
         irec2=pgsmlir(lclif2,1,ni,nj,nk,datev,cetiket,lcl_ip1tab(ii),         ip2tab(jj), ip3tab(kk),ctypvar,cnoment,cigtyp)
!
!
         if (printen)  call imprime(cnoment,lclif2,ni,nj)
!
!     difference entre les deux champs
!
         nn = ni*nj
         call loupsou(lclif1,lclif2,nn)
!
!     interpolation horizontale
!
         if (cgrtyp.eq.'*') then
            ier = chkenrpos(1,2,ig1,ig2,ig3)
         else
!     #  variable symetrique oui=.true.

            gdin = ezqkdef(ni, nj, cigtyp, ig1, ig2, ig3, ig4, iunit)
            ier = ezdefset(gdout, gdin)
            ier = ezsint(lclif2, lclif1)
         endif
!
!     ecrire le ip1,ip2,ip3 correspondant aux definitions
!
         if (num1.gt.1) then
            jp(1)=lcl_ip1tab(i)
            jp(2)=lcl_ip1tab(ii)
            jp(3)=ip2tab(j)
         endif
!
         if (num2.gt.1) then
            jp(1)=lcl_ip1tab(i)
            jp(2)=ip2tab(j)
            jp(3)=ip2tab(jj)
         endif
!
         if (num3.gt.1) then
            jp(1)=lcl_ip1tab(i)
            jp(2)=ip3tab(k)
            jp(3)=ip3tab(kk)
         endif

         if (cnomsrt.eq.'    ') then
            cnomsrt=cnoment
         endif

         lesips(1) = ip1s
         lesips(2) = ip2s
         lesips(3) = ip3s

         do i=1,3
            if (lesips(i).eq.65001) then
               jp(i) = jp01
            endif

            if (lesips(i).eq.65002) then
               jp(i) = jp11
            endif

            if (lesips(i).eq.65003) then
               jp(i) = jp02
            endif

            if (lesips(i).eq.65004) then
               jp(i) = jp12
            endif

            if (lesips(i).eq.65005) then
               jp(i) = jp03
            endif

            if (lesips(i).eq.65006) then
               jp(i) = jp13
            endif
         enddo


!
!
!     ecrire sur fichier standard,ms,sequentiel
!
!
      if (cgrtyp.eq.'*') then
         call ecritur(lclif1,npack,datsrt,deetsrt,npassrt,ni,nj,nk,         jp(1),jp(2),jp(3),         ctypvar,cnomsrt,cetiket,cigtyp,ig1,ig2,ig3,ig4)
      else
         call ecritur(lclif2,npack,datsrt,deetsrt,npassrt,         li,lj,1,jp(1),jp(2),jp(3),         ctypvar,cnomsrt,cetiket,cgrtyp,lg1,lg2,lg3,lg4)
      endif

!
!     remetre espace des champs de travail
!
      deallocate(lclif2)
      deallocate(lclif1)
!
      enddo
!
      return
      end


