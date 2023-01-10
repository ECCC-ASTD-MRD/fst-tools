!
!**s/p champ_seq  Miroir de la directive champ pour fichiers sequentiels
!
#include "defin.cdk90"
      subroutine champ_seq (listn,listip1,waitOrGo)
      use app
      implicit none
      integer listn(*),listip1(*),waitOrGo
      external ecritur,fstrwd,pgsmlir,fstprm,symetri,fstsel,fstsui,pgsmluk
      external loupneg,loupsou,argdims,pgsmabt,imprims,grille2
      external imprime,messags,fstcvt
      external liraxez
      integer  fstinf,pgsmlir,fstprm,fstcvt,fstsel,fstsui,fstrwd,pgsmluk
      integer ezqkdef, ezsint, ezdefset
!
!auteur  Yves Chartier drpn Dorval Quebec Avril 1996
!revision
!
!langage fortran
!
!objet(champ_seq)
!
!arguments
!  in    listn    liste de nomvar
!  in    listip1  liste de niveau
!  in    waitOrGo commutateur d'accumulation de directives
!
!
!implicites
!
!messages
!          'mauvais appel a champdif il devrait y avoir 3 arguments'
!
!
!modules  fstinf,memoir,fstprm,pgsmlir,symetry,rgscint,ecritur,argdims
!         imprime,loupsou,pgsmabt
!
!appel     via directive champ_seq(listn, listip1, waitOrGo)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
#include "voir.cdk90"
#include "llccmm.cdk90"
#include "accum.cdk90"
#include "champs.cdk90"
#include "indptr.cdk90"
#include "grilles.cdk90"
#include "lires.cdk90"
#include "ecrires.cdk90"
#include "dates.cdk90"
#include "packin.cdk90"
#include "dummys.cdk90"
#include "gdz.cdk90"
#include "champseq.cdk90"
#include "lnkflds.cdk90"
#include "heures.cdk90"
#include "styles.cdk90"
!
      character*12 etiket
      character*4 nomvar
      character*2 typvar
      character*1 cigtyp

      integer ig1,ig2,ig3,ig4,irec,iunit
      integer num1,num2,num3,nloop,deet
      integer ip1,ip2,ip3,i,j,k, date
      integer ni,nj,nk,nbits,datyp,swa, lng, dltf, ubc, extra1, extra2, extra3
      integer argdims
      logical symetri,sym,heureok,ip3ok,processed
      character*8 string
      real p

      real fbidon
      iunit = 1
!
      if (npar.ne. 3) then
         if (message) then
            call app_log(APP_ERROR,'champ_seq: Directive CHAMP_SEQ should have 3 arguments')
         endif
         return
      endif

      if (.not.associated(tmplat)) then
         if (message) then
            call app_log(APP_WARNING,'champ_seq: Grid not defined, will use PS(2805)')
         endif
         call grille2(3,51,55,26.,28.,381000.,350.,1)
      endif
!
!   trouver nombre d'arguments dans une liste (ip1,ip2,ip3)
!

      ntitems = ntitems + 1
      if (ntitems.gt.nmaxlist1) then
         call app_log(APP_ERROR,'champ_seq: Limit of 16 directives CHAMP_SEQ has been passed')
         call pgsmabt
      endif

      if (argdims(1).gt.nmaxlist2) then
         call app_log(APP_ERROR,'champ_seq: Limit of 16 variable names has been passed')
         call pgsmabt
      endif

      if (argdims(2).gt.nmaxlist2) then
         call app_log(APP_ERROR,'champ_seq: Limit of 16 vertical levels has been passed')
         call pgsmabt
      endif

      nitems1(ntitems) = argdims(1)
      nitems2(ntitems) = argdims(2)
      do i=1,argdims(1)
         write(listnom(ntitems,i),'(A2)') listn(i)
      enddo

      do i=1,argdims(2)
         listniv(ntitems,i) = listip1(i)
      enddo

      if (listniv(ntitems, 1) > 1000000 .and. listniv(ntitems,2)  < 0) then
         do i=1,argdims(2),2
            p = transfer(listniv(ntitems,i), p)
            call convip_plus(listniv(ntitems, i/2+1), p, &
                             -1*listniv(ntitems, (i+1))-1000, &
                             ip1style, string, .false.)
         enddo
         nitems2(ntitems) = argdims(2)/2
      endif


      if (waitOrGo.eq.WAIT) then
         return
      endif

      ier =fstrwd(lnkdiun(1))
      irec=fstsel(1,ni,nj,nk,-1,'        ',-1,-1,-1,' ','  ')
 200  irec = fstsui(1,ni,nj,nk)
      if (irec.ge.0) then
         processed = .false.
         ier = fstprm(irec, date,deet,npas,ni, nj, nk,          nbits,datyp,         ip1,ip2,ip3,typvar,nomvar,etiket,         cigtyp,ig1,ig2,ig3,ig4,         swa, lng, dltf, ubc, extra1, extra2, extra3)

         heureok = .false.
         if (heures(1).eq.-1) then
            heureok=.true.
         else
            do k=1,nhur
               if (ip2.eq.heures(k)) then
                  heureok = .true.
               endif
            enddo
         endif

 100     if (heureok.and..not.processed) then
            do i=1,ntitems
               if (.not.processed) then
                  do j=1,nitems1(i)
                     if (listnom(i,j).eq.nomvar.or.listnom(i,j).eq.' '.and..not.processed) then
                        do k=1,nitems2(i)
                           if (listniv(i,k).eq.ip1.or.listniv(i,k).eq.-1.and..not.processed) then
                              allocate(tmpif1(ni,nj))
                              allocate(tmpif2(li,lj))
                              ier=pgsmluk(tmpif1,irec,ni,nj,nk,nomvar,cigtyp)
!
                              if (nk .gt. 1) then
                                 call app_log(APP_ERROR,'champ_seq: PGSM does not allow for 3 dimension fields (NK>1)')
                                 call pgsmabt
                              endif

                              gdin = ezqkdef(ni, nj, cigtyp, ig1, ig2, ig3, ig4, iunit, fbidon, fbidon)
                              ier = ezdefset(gdout, gdin)
                              ier = ezsint(tmpif2, tmpif1)

                              call ecritur(tmpif2,npack,date,deet,npas,                              li,lj,1,ip1,ip2,ip3,                              typvar,nomvar,etiket,cgrtyp,lg1,lg2,lg3,lg4)
!
                              deallocate(tmpif2)
                              deallocate(tmpif1)
                              processed=.true.
                           endif
                        enddo
                     endif
                  enddo
               endif
            enddo
         endif
         goto 200
      endif

!**  l'interpolation est terminee - On a pass� a travers le fichier

      do i=1,ntitems
         do j=1,nitems2(i)
            listnom(i,j) = '  '
            listniv(i,j) = -1
         enddo
         nitems2(i)=0
      enddo
      ntitems=0

      return
      end

