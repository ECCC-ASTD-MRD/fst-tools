!
!**s/p uvectur interpolation des vecteurs u-v (horizontalement)
!
   subroutine uvectur (cnom1, cnom2, cnom3,iheur, npar, itabuv)
      use app
      implicit none
      
   external ecritur,cigaxg,fstinf,fstinl,ipgsmlic,pgsmlic,ipgsmlir,pgsmlir,memoir, fstcvt,fstprm,pgsmabt,imprime,vdauv, incdat,fstopc,messags
   external liraxez,  cxgaig
   integer fstinf,fstinl,ipgsmlic,pgsmlic,ipgsmlir,pgsmlir,fstprm,fstopc,fstcvt
   integer ezqkdef, ezwdint, ezuvint, ezdefset, ezsint
   real fbidon
#include "defin.cdk90"
!
!auteur  p.sarrazin fevrier 82 drpn  dorval p.q. canada
!revision 4.0.2
!   conversion des variables hollerith en caracteres
!   y. chartier -aout 90- drpn dorval quebec
!
!langage ratfor
!
!objet(uvectur)
!         interpolation des vecteurs u et v
!
!librairies
!         -source  armnsrc,drpn
!         -objet   pgsmlib,id=armnpjs.
!
!arguments
!  in    nom1   nom du premier vecteur  ex:"uu","us"...
!  in    nom2   nom du deuxieme vecteur ex:"vv","vs"...
!  in    nom3   nom du champ a ecrire apres interpolation du vent ex:"uv"
!  in    iheur   heure de la variable
!  in    npar    nombre de locations dnas itabuv
!  in    itabuv table contenant les noms (niveau)
!
!appel
!         -via routine champ
!         call uvectur(iheur, npar, itabuv)
!
!
!modules  fstinf,pgsmabt,memoir,fstprm,pgsmlir,cuvint,cigaxg,cspauv,ecritur
!
!messages
!         mauvaise directive champ (uvectur)
!         record n'existe pas sur fichier d'entre (u,v) (uvectur)
!         aucune interpolation horizontale u v
!
!----------------------------------------------------------------------
!
#include "llccmm.cdk90"
#include "accum.cdk90"
#include "lires.cdk90"
#include "pairs.cdk90"
#include "dummys.cdk90"
#include "lnkflds.cdk90"
#include "ecrires.cdk90"
#include "indptr.cdk90"
#include "dates.cdk90"
#include "grilles.cdk90"
#include "voir.cdk90"
#include "enrege.cdk90"
#include "packin.cdk90"
#include "chck.cdk90"
#include "gdz.cdk90"
#include "tp12ig.cdk90"
!
!---------------------------------------------------------------

   character*12 cetiket,cetike
   character*4 cnomvar,cnom1,cnom2, cnom3
   character*1 cigtyp
   character*2 ctypvar

   integer i,j
   integer jp1,jp2,jp3,liljt,ni,nj,nk,npar
   integer, dimension(:), allocatable :: listniv
   real, allocatable, dimension(:,:) :: latgdin, longdin
   integer itabuv(npar) ,deet,ig1,ig2,ig3,ig4
   integer iheur,ilop,iprs,irec_uu,irec_vv,iopc
   integer numu,numv,infon,dat,datdv
   integer cnbits,cdatyp,cswa,clng,cdltf,cubc,extra1,extra2,extra3,total_keys
   integer nom2
   integer tableau(1)
   integer ig1zz, ig2zz, ig3zz, ig4zz, ezsetgdout, gdwdfuv, gdll, npts
   real    d60dum, pidum, pjdum
   real xlat1,xlon1,xlat2,xlon2
   real dgtord,dumfld,xg1,xg2,xg3,xg4,datev
   integer iunit
   logical ssw
   integer entier_ou_reel

   real*8 delta_t

   real, dimension(:,:), pointer :: uuout,vvout
   iunit = lnkdiun(1)
   nk = 1

   call pgsm_get_nfstkeys(total_keys)
   allocate(listniv(total_keys))

   call chk_userdate(datev)
!
!
   do iprs = 1,npar
!
!     trouver record pour u,v  ou us,vs .....
!
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


      ier = fstinl(iunit,ni,nj,nk,datev,cetiket,itabuv(iprs),iheur,ip3ent,ctypvar,cnom1,listniv,infon,total_keys)
      if (ier .lt. 0 .or. infon.eq.0) then
         write(app_msg,610) cnom1
 610     format('uvectur: No record in file NOM=',a2)
         call app_log(APP_ERROR,app_msg)
         cycle
      endif

      if (nk.gt.1) then
         call app_log(APP_ERROR,'uvectur: PGSM does not accept 3 dimension fields (NK>1)')
         call pgsmabt
      endif
!
!
      do ilop=1,infon
!
     entier_ou_reel = 1
         irec_uu=listniv(ilop)
!
!     identifier parametres champ nom1
!
         cetike = '            '
         ier = fstprm( irec_uu, dat,deet,npas,ni, nj, nk, cnbits,cdatyp,jp1,jp2, jp3,ctypvar, cnomvar,cetike,cigtyp, ig1,ig2,ig3,ig4, cswa, clng, cdltf, cubc, extra1, extra2, extra3)
         npack_orig = -cnbits
         if (ier .lt. 0) then
            call app_log(APP_ERROR,'uvectur: FSTPRM failed')
         endif
!
!     verifier si grille gaussienne ni doit etre pair
!
 675     format(' ITYP=',a1,'   CIGTYP DE FSTPRM= ',a1)

         if (cigtyp.eq.'G'.and.mod(ni,2).ne.0)  call messags(ni)
!
!     calcul la date pour le record de la variable nom2
!
         delta_t = deet*npas/3600.0
         call incdatr(datdv,dat,delta_t)
         irec_vv = fstinf(iunit,ni,nj,nk,datdv,cetike,jp1,jp2,jp3,ctypvar,cnom2)
         if (irec_vv .lt. 0) then
            write(app_msg,610) nom2
            call app_log(APP_ERROR,app_msg)
            call pgsmabt
         endif

         if (nk.gt.1) then
          call app_log(APP_ERROR,'uvectur: PGSM does not accept 3 dimension fields (NK>1)')
          call pgsmabt
       endif
       
!!  Switch pour champs masques
      if (masque == 1) then
         if (ctypvar(1:1) == '@') then
            cycle ! Les masques sont traites dans uvecteur_masque
         else if (ctypvar(2:2) == '@') then
            call uvecteur_masque(irec_uu, irec_vv)
            cycle
         endif
      endif

!     allouer memoire

      if (cdatyp == 2 .or. cdatyp == 130 .or. cdatyp == 4 .or. cdatyp == 132) then
    allocate(itmpif1(ni,nj))
    allocate(itmpif2(ni,nj))
    allocate(itmpif3(li,lj))
    allocate(itmpif4(li,lj))
    entier_ou_reel = 2
      endif

      allocate(tmpif1(ni,nj))
      allocate(tmpif2(ni,nj))
      allocate(tmpif3(li,lj))
      allocate(tmpif4(li,lj))
!
!     lire champ nom1
!
      if (.not.message) iopc= fstopc('TOLRNC','DEBUGS',.true.)
      if (entier_ou_reel == 2) then
    numu =ipgsmlir(itmpif1,1,ni,nj,nk,datdv,cetike,jp1,jp2,jp3,ctypvar,cnom1,cigtyp)
      else
    numu = pgsmlir(tmpif1,1,ni,nj,nk,datdv,cetike,jp1,jp2,jp3,ctypvar,cnom1,cigtyp)
      endif

      if (printen)  call imprime(cnom1,tmpif1,ni,nj)
      if (.not.message) iopc= fstopc('TOLRNC','DEBUGS',.true.)
      if (entier_ou_reel == 2) then
    numv = ipgsmlic(itmpif2,1,ni,nj,nk,datdv,cetike,jp1,jp2,jp3,ctypvar,cnom2,ig1,ig2,ig3,ig4,cigtyp)
      else
    numv = pgsmlic(tmpif2,1,ni,nj,nk,datdv,cetike,jp1,jp2,jp3,ctypvar,cnom2,ig1,ig2,ig3,ig4,cigtyp)
      endif
      if (cigtyp.eq.'G'.and.mod(ni,2).ne.0)  call messags(ni)
!
      if (printen)  call imprime(cnom2,tmpif2,ni,nj)
!****************************************************************
!     si vvent=.true. on calcule la vitesse du vent

      if (entier_ou_reel == 2) then
     call cvtrfi(tmpif1, itmpif1, ni, nj)
     call cvtrfi(tmpif2, itmpif2, ni, nj)
      endif
      
      gdin = ezqkdef(ni, nj, cigtyp, ig1, ig2, ig3, ig4, iunit)

      if (vvent) then
         ssw=.false.
         if (gdout == gdin) then
            if (ctypvar == '@@') then
                uuout => tmpif1
                vvout => tmpif2
            else
              if (wdvent) then
                ssw = .true.
                allocate(latgdin(ni,nj),longdin(ni,nj))
                ier = gdll(gdin, latgdin, longdin)
                npts = ni * nj
                ier = gdwdfuv(gdin, tmpif3, tmpif4, tmpif1, tmpif2, latgdin, longdin, npts)
                cgrtyp = cigtyp
                lg1 = ig1
                lg2 = ig2
                lg3 = ig3
                lg4 = ig4
                uuout => tmpif3
                vvout => tmpif4
              else
                uuout => tmpif1
                vvout => tmpif2
                uuout = sqrt(uuout*uuout+vvout*vvout)
              endif
            endif
         else
            ier = ezdefset(gdout, gdin)
            if (ctypvar == '@@') then
               ier = ezsint(tmpif3, tmpif1)
               ier = ezsint(tmpif4, tmpif2)
            else
               ier = ezwdint(tmpif3, tmpif4, tmpif1, tmpif2)
            endif
            uuout => tmpif3
            vvout => tmpif4
         endif

     if (entier_ou_reel == 2) then
        call cvtifr(itmpif3, uuout, li, lj)
        call  iecritur(itmpif3, npack, dat, deet, npas, li, lj, nk, jp1, jp2, jp3, &
          ctypvar, cnom3, cetike, cgrtyp, lg1, lg2, lg3, lg4)
     else
        call  ecritur(uuout, npack, dat, deet, npas, li, lj, nk, jp1, jp2, jp3, &
        ctypvar, cnom3, cetike, cgrtyp, lg1, lg2, lg3, lg4)
         endif

      if (wdvent) then
         do j=1,lj
         do i=1,li
            if (tmpif4(i,j).lt.0.0) then
               tmpif4(i,j) = tmpif4(i,j) + 360.0
            endif
         enddo
         enddo
         vvout => tmpif4
         if (entier_ou_reel == 2) then
        call cvtifr(itmpif4, vvout, li, lj)
        call iecritur(itmpif4, npack, dat, deet, npas, li, lj, nk, jp1, jp2, jp3,&
        ctypvar,'WD  ',cetike,cgrtyp,lg1,lg2,lg3,lg4)
          else
        call ecritur(vvout, npack, dat, deet, npas, li, lj, nk, jp1, jp2, jp3,&
        ctypvar,'WD  ',cetike,cgrtyp,lg1,lg2,lg3,lg4)
         endif
      endif


!
!****************************************************************
!
   else
!
!     on ne fait pas d'interpolation si igtyp=grtyp  ig1=lg1  ig2=lg2
!     ig3=lg3  ig4=lg4
!
            if (cigtyp.ne.cgrtyp.or.ig1.ne.lg1.or.ig2.ne.lg2.or.ig3.ne.lg3.or.ig4.ne.lg4.or.li.ne.ni.or.lj.ne.nj) then
!
!     interpolation u,v vecteur a vitesse et direction du vent
!
!     si ssw = vrai interpoler vitesse et direction
!     faux interpoler seulement vitesse
!
               ssw = .true.
               ier = ezdefset(gdout, gdin)
!
!              cas special pour typvar = @@

               if (ctypvar == '@@') then
                  ier = ezsint(tmpif3, tmpif1)
                  ier = ezsint(tmpif4, tmpif2)
               else
                  ier = ezuvint(tmpif3, tmpif4, tmpif1, tmpif2)
               endif

               uuout => tmpif3
               vvout => tmpif4
!
!     apres interpolation horizontale passer de vitesse et direction
!     aux composantes u et v
!
!     si type de grille "x",    u-v interpolation n\s - e\o
!

            else
               deallocate(tmpif3)
               deallocate(tmpif4)
               uuout => tmpif1
               vvout => tmpif2
               if (message) then
                  call app_log(APP_WARNING,'uvectur: No horizontal interpolation')
              endif
           endif
!
!     ecrire vecteur u
!
         if (entier_ou_reel == 2) then
        call cvtifr(itmpif3, uuout, li, lj)
        call iecritur(itmpif3, npack,dat,deet,npas,li,lj,nk,jp1,jp2,jp3,ctypvar,cnom1,cetike,cgrtyp,lg1,lg2,lg3,lg4)
     else
           call ecritur(uuout,npack,dat,deet,npas,li,lj,nk,jp1,jp2,jp3,ctypvar,cnom1,cetike,cgrtyp,lg1,lg2,lg3,lg4)
         endif
!
!
!     ecrire vecteur v
!
         if (entier_ou_reel == 2) then
        call cvtifr(itmpif4, vvout, li, lj)
        call iecritur(itmpif4, npack,dat,deet,npas,li,lj,nk,jp1,jp2,jp3,ctypvar,cnom2,cetike,cgrtyp,lg1,lg2,lg3,lg4)
     else
            call ecritur(vvout,npack,dat,deet,npas,li,lj,nk,jp1,jp2,jp3,ctypvar,cnom2,cetike,cgrtyp,lg1,lg2,lg3,lg4)
         endif
!
!     fin du calcul des composantes
!
!
         endif

         if (associated(tmpif3)) deallocate(tmpif3)
         if (associated(tmpif4)) deallocate(tmpif4)
         if (associated(tmpif1)) deallocate(tmpif1)
         if (associated(tmpif2)) deallocate(tmpif2)

         if (allocated(latgdin)) deallocate(latgdin)
         if (allocated(longdin)) deallocate(longdin)

         if (entier_ou_reel == 2) then
        if (associated(itmpif1)) deallocate(itmpif1)
        if (associated(itmpif2)) deallocate(itmpif2)
        if (associated(itmpif3)) deallocate(itmpif3)
        if (associated(itmpif4)) deallocate(itmpif4)
         endif

99999 continue
      enddo
!
!
!     reinitialiser clef de controle
!
   enddo    ! fin boucle iprs
      vvent=.false.
!
!
      deallocate(listniv)
      return
      end


