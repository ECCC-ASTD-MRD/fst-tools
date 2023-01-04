!
!**   s/p coupzm  coupe zonale ou meridionale d un champ
!
#include "defin.cdk90"
   subroutine coupzm(iunit, cnom, cjcoup)
      use app
      implicit none

      external calcul,ecritur,gauss,fstinf,pgsmlir,memoir,fstprm,fstcvt,          pgsmabt,imprime,loupmir,louptra,loupin1,fstopc,messags
      integer  fstinf,pgsmlir,fstprm,fstopc,fstcvt
!
!auteur  p. sarrazin  dorval quebec avril 85 drpn
!
!revision
!     4.0.2 - conversion en caracteres de toutes les variables
!             de type hollerith
!             y. chartier- dorval quebec juillet 90 drpn.
!
!langage ratfor
!
!objet(coupzm)
!            faire une coupe zonale ou meridionale sur un champ dont
!            le type de grille est "g"-"a"-"l"-"b"-"c"
!            calcul pour une coupe meridionale sur un champ gaussien
!            "g" est fait a partir de poids calcule par gaussg
!            la moyenne zonale(est-ouest) contient nj points
!            la moyenne meridionale(nord-sud) contient ni points
!
!librairies
!         - source pgsm/un=armnsrc
!         -  objet pgsmlib,id=armnpjs sur xmp
!
!arguments
!  in   iunit  numero du fichier a lire
!  in   cnom    nom du champ 2 caracteres gz.tt.dd.......
!  in   cjcoup  coupe zonale='zon' meridionale='mer'
!
!appel
!         -via routine coupe
!         call coupzm(iunit, cnom, cjcoup)
!
!messages
!          record n'existe pas verifier directive moyent/moysrt
!          iunit=  niveaux=   heure=   nom=
!          mauvais type de grille directive moyent/moysrt
!          grtyp=
!          doit-etre "g"-"a"-"l"-"c"-"b"
!
!modules pgsmabt,rfl,fstinf,fstprm,pgsmlir,ecritur
!
!-----------------------------------------------------------------
!
#include "llccmm.cdk90"
#include "dates.cdk90"
#include "grilles.cdk90"
#include "indptr.cdk90"
#include "lires.cdk90"
#include "accum.cdk90"
#include "voir.cdk90"
#include "ecrires.cdk90"
#include "packin.cdk90"
#include "param.cdk90"
#include "heures.cdk90"
#include "nivos.cdk90"
#include "cfldinf.cdk90"
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!
      character cnom*3, cjcoup*8
      real coa(500),w(500),sia(500),rad(500),poids(500),sinm2(500),      sinm1(500),sin2(500),champ(1000)
      integer i, iunit, datev
      integer ihr,iheur,iprs,npres,irec,ni,nj,nk
      integer jp1,jp2,jp3,ig1,ig2,ig3,ig4
      integer num,ilath,j,cnbits,cdatyp,iopc,      cswa, clng, cdltf, cubc, extra1, extra2, extra3
      character*8 cdummy
      integer      dummy
      integer un
      un = 1

      cnomvar = cnom
!
!
!     loop des heures
!
      do ihr=1,nhur
         iheur=heures(ihr)
      enddo
!
      npres=max0(1,nmo-2)
      do iprs = 1,npres
!
!     identifier numero du record
!
         call chk_userdate(datev)
!
!     modification de hollerith a caractere
!

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

         cigtyp = ' '
         irec = fstinf(iunit,ni,nj,nk,datev,cetiket,nivospr(iprs),          iheur,ip3ent,ctypvar,cnomvar)
!
         if (irec .lt. 0) then
            write(app_msg,*)  'coupzm: Record does not exist, check directive MOYENT/MOYSRT IUNIT=',iunit,' NIVEAU=',nivospr(iprs),' HEURE=',iheur,'NOM=',cnom
            call app_log(APP_ERROR,app_msg)
            return
         endif

         if (nk.gt.1) then
            call app_log(APP_ERROR,'coupzm: PGSM does not accept 3 dimension fields (NK>1)')
            call pgsmabt
         endif
!
!
!     identifier parametres si type g-a-b-l-c
!
!
         ier = fstprm( irec, dat,deet,npas,ni, nj, nk, cnbits,cdatyp,         jp1,jp2, jp3,ctypvar,cnomvar,cetiket,cigtyp,          ig1,ig2,ig3,ig4,          cswa, clng, cdltf, cubc, extra1, extra2, extra3)
         if (ier .lt. 0) then
            call app_log(APP_ERROR,'coupzm: FSTPRM failed')
         endif
!
!     verifier si grille gaussienne ni doit etre pair
!
         if (cigtyp.eq.'G'.and.mod(ni,2).ne.0)  then
            call messags(ni)
         endif
!
!
!     nombre de longitude max=1000
!
         if (ni.gt.1000) then
            call app_log(APP_ERROR,'coupzm: Too many longitudes within fields (max=1000)')

            call pgsmabt
         endif
!
!     verifier si dimension nj .gt. 500
!
         if (ig1.eq.0.and.nj.gt.500) then
            call app_log(APP_ERROR,'coupzm: Dimension NJ for MOYENT-MOYSRT too large (max=500)')
            call pgsmabt
         endif
!
!     verifier type de grille
!
         if (cigtyp.ne.'G'.and.cigtyp.ne.'A'.and.cigtyp.ne.'L'         .and.cigtyp.ne.'B'.and.cigtyp.ne.'C') then
            write(app_msg,*) 'coupzm: Bad grid type: ',cigtyp,', must be G - L - B - A - C (MOYENT/MOYSRT)'
            call app_log(APP_ERROR,app_msg)
            return
         endif
!
!     lire champ
!
         allocate(tmpif1(ni,nj))
!
         if (.not.message) then
            iopc= fstopc('TOLRNC','DEBUGS',.true.)
         endif
         call chk_userdate(datev)
!
         num=pgsmlir(tmpif1,iunit,ni,nj,nk,datev,cetiket,jp1,jp2,         jp3,ctypvar,cnomvar,cigtyp)

        if (printen)  then
           call imprime(cnom,tmpif1,ni,nj)
        endif
        if (num .lt. 0)  then
           call app_log(APP_ERROR,'coupzm: Field does not exist in MOYENT/MOYSRT')
           call pgsmabt
        endif
!
!     initialiser les poids pour grille gaussienne meridionale
!
        if (cjcoup.eq.'MER'.and.cigtyp.eq.'G') then
           ilath=nj
           if (ig1.eq.0) then
              ilath=nj/2
           endif
!
           call gauss(ilath,coa,poids,sia,rad,w,sinm1,sinm2,sin2)
!
!     sauve les poids dans coa pour renverser le champ si necessaire
!
           do j=1,ilath
              coa(j)=poids(j)
           enddo
!
!     si gaussienne globale  transfer hemis nord dans hemis sud (miroir)
!
           if (ig1.eq.0)  then
              call loupmir(poids(ilath),poids(ilath),ilath)
           endif
!
!  hemisphere sud  orientation nord-sud renverse
!
           if ((ig1.eq.2.and.ig2.eq.1) .or.(ig1.eq.1.and.ig2.eq.0))then
              call louptra(poids(ilath),coa,ilath)
           else
              call loupin1(poids(1),poids(1),nj)
           endif
        endif
!     calcul moyenne zonale ou meridional
!
!
        call calcul(tmpif1,champ,ni,nj,poids,cjcoup,cigtyp)
!
!     si type de grille 'b' on reduit ni=ni-1
!
        if (cigtyp.eq.'B') then
           ni=ni-1
        endif
!
!     ecrit champ zonal
!
        if (cjcoup.eq.'ZON') then
           call ecritur(champ,npack,dat,deet,npas,un,nj,nk,jp1,jp2,           jp3,ctypvar,cnomvar,cetiket,cigtyp,ig1,ig2,ig3,ig4)
!
!     ecrit champ  meridional
!
        else
           call ecritur(champ,npack,dat,deet,npas,ni,un,un,jp1,jp2,           jp3,ctypvar,cnomvar,cetiket,cigtyp,ig1,ig2,ig3,ig4)
        endif
        deallocate(tmpif1)
      enddo

!
 600  format(' NOM= ',a2)
 601  format(' IGTYP= ',a1)
!
      return
      end

