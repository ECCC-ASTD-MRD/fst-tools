!**s/p scalair  interpolation horizontale d un champ
!               defini par l usager
!
   subroutine scalair(cnom, iheur, nniv, niveaux)
      use app
      implicit none
      
#include "defin.cdk90"
      external ecritur, pgsmluk, ipgsmluk, fstinf, fstsui, memoir, fstprm, qaaqr, fstcvt,     fstsel, symetri, imprime, itrouve, messags, pgsmabt
      external cvtrfi,cvtifr
      external liraxez
      integer  pgsmluk, ipgsmluk, fstinf, fstsui, fstprm, fstcvt, fstsel, fstinl
!      integer diesinf, dieslir, diesisincache, res
      integer ezgdef_fmem, ezqkdef, ezsint, ezdefset, ezgetopt
      logical skip
!
!auteur  p. sarrazin  dorval quebec fevrier 82 drpn
!revision 4.0.2
!   conversion des variables hollerith en caractere
!   y. chartier -aout 90- drpn dorval quebec
!revision 5.0.0
!   utilisation de la cuvee 91 des interpolateurs
!   elimination des appels a 'fstcvt' pour convertir etikent
!   y. chartier -mai 1991- drpn dorval quebec
!
!revision 5.2.1
!   utilisation de fstinl au lieu de fstinf-fstsui
!   l'utilisation des deux dernieres avec les grilles Z
!   causait un probleme lorsqu'on lisait les axes
!   y. chartier -oct. 92- drpn dorval quebec
!langage ratfor
!
!objet(scalair)
!              interpolation horizontale des scalaires gz, tt, dd, ww, qq, es.
!              d une grille a une autre pour nniv niveaux
!              ecrire resultat sur fichier standard, ms, sequentiel
!
!librairies
!         -source  armnsrc, drpn
!         -objet   pgsmlib, id=armnpjs.
!
!arguments
!  in   nom    nom du champ 2 caracteres gz.tt.dd.......
!  in   iheur  heure de la variable sur fichier d entre
!  in   nniv   nombre de niveaux
!  in   niveaux table contenant nniv niveaux
!
!appel
!         -via routine champ
!         call scalair(nom, iheur, nniv, niveaux)
!
!messages
!         mauvaise directive champ (scalair)
!         aucun niveau specifie dans directive champ
!         record n'existe pas sur fichier d'entre (scalair)
!         aucune interpolation horizontale champ
!
!
!modules pgsmabt, rfl, fstinf, fstprm, pgsmlir, rgscint, ecritur
!
!-----------------------------------------------------------------
!
#include "defin.cdk90"
#include "accum.cdk90"
#include "dates.cdk90"
#include "dummys.cdk90"
#include "ecrires.cdk90"
#include "gdz.cdk90"
#include "grilles.cdk90"
#include "indptr.cdk90"
#include "lires.cdk90"
#include "llccmm.cdk90"
#include "lnkflds.cdk90"
#include "packin.cdk90"
#include "pairs.cdk90"
#include "voir.cdk90"

   character *12 cetiket
   character *4 cnom, cnomvar, cnomx
   character *2 ctypvar
   character *1 cigtyp

   character*8 extrap_option
   integer nniv, dateo, datev, i, nunv, itrouve, key, ii, j
   integer niveaux(nniv), deet, ig1, ig2, ig3, ig4, iheur
   integer iprs, irec, iunit, jp1, jp2, jp3, ne, ni, nj, nk, total_keys, nrecs
   integer cnbits, cdatyp, cswa, clng, cdltf, cubc, extra1, extra2, extra3
   logical sym, symetri
   real fbidon
   real, dimension(:), allocatable :: ax, ay
   integer, dimension(:,:), allocatable :: ifld_in, ifld_out

   real, dimension(:,:), pointer :: tmpout
!
   integer, dimension(:), allocatable :: liste
   logical, dimension(:), allocatable :: done_fields

   nunv=0
   iunit=lnkdiun(1)
   call pgsm_get_nfstkeys(total_keys)

   allocate(liste(total_keys))
   allocate(done_fields(total_keys))
   done_fields = .false.
   cnomx = cnom
   if (cnom.eq.cnomqr) cnom=cnomqq

   do iprs = 1, nniv
!
!     conversion de l etiquette d'entree en caracteres
!
      if (etikent(1)  /=  -1) then
         write(cetiket, '(3A4)') (etikent(i), i=1, nwetike)
      else
         cetiket = '            '
      endif

      if (typeent  /=  -1) then
         write(ctypvar, '(A2)') typeent
      else
         ctypvar = '  '
      endif

!     identifier numero du record
!
      if (cnom.eq.cnommt) then
         if (.not.mtdone) then
            irec = fstinl(iunit, ni, nj, nk, -1, '            ', -1, -1, -1, '  ', cnom, liste, nrecs, total_keys)
            if (nrecs .eq. 0) then
               if (message) then
                  call app_log(APP_WARNING,'scalair: Mountain record not found')
               endif
               goto 5000
            endif

            if (nk.gt.1) then
               call app_log(APP_ERROR,'scalair: PGSM does not accept 3 dimension fields (NK>1)')
               call pgsmabt
            endif
 5000       mtdone=.true.
         endif
      else
         call chk_userdate(datev)

         irec=fstinl(iunit, ni, nj, nk, datev, cetiket, niveaux(iprs), iheur, ip3ent, ctypvar, cnom, liste, nrecs, total_keys)

         if (nrecs .eq. 0) then
            if (message) then
               call app_log(APP_WARNING,'scalair: Record does not exist in input file')
            endif
            goto 22000
         endif

         if (nk.gt.1) then
            call app_log(APP_ERROR,'scalair: PGSM does not accept 3 dimension fields (NK>1)')
            call pgsmabt
         endif
      endif
!
!
      do ii=1, nrecs
!
         irec = liste(ii)

!  On verifie d'abord que le champ n'a pas ete traite, car les masques associes sont traites ailleurs.

         if (done_fields(ii)) cycle
         ier=fstprm(irec, dateo, deet, npas, ni, nj, nk, cnbits, cdatyp, jp1, jp2, jp3, ctypvar, cnomvar, cetiket, cigtyp, &
            ig1, ig2, ig3, ig4, cswa, clng, cdltf, cubc, extra1, extra2, extra3)
         npack_orig = -cnbits
!         print *, 'npack_orig=', npack_orig, 'npack=', npack
         if (ier .lt. 0) call app_log(APP_ERROR,'scalair: FSTPRM failed')

         if (cnomvar(1:2).eq.'>>'.or.cnomvar(1:2).eq.'^^'.or.cnomvar(1:2).eq.'HY'.or.cnomvar.eq.'^>'.or.cnomvar.eq.'!!') then
            cycle
         endif

         if (ctypvar(2:2).eq.'@'.and.masque == 1) then
            if (ctypvar(1:1) /= '@') then
              call scalair_msk(irec, liste, done_fields,total_keys)
            endif
            cycle
         endif
         
         extrap_option = '        '
         ier = ezgetopt('extrap_degree', extrap_option)
         if (extrap_option(1:5) == 'abort') then
            gdin = ezqkdef(ni, nj, cigtyp, ig1, ig2, ig3, ig4, iunit)
            call chk_extrap(gdout, gdin, li, lj, ni, nj)
         endif
!
!     si la directive de champ emploi tout  champ(tout, tout) on doit faire
!     attention pour les vecteurs u-v car l'interpolation serait scalaire
!     on verifie et les interpolations pour les vecteurs ne sont pas faites
!
         do i=1, npair
            if (cnom.eq.'    ') then
               if ((paire(i)(9:10).eq.cnomvar(1:2)).or.(paire(i)(13:14).eq.cnomvar(1:2))) then
                  nunv=nunv + 1
                  goto 99999
               endif
            endif
         enddo
!
!     lire champ

         skip = .false.

	      allocate(tmpif1(ni,nj))      
!
         if (cdatyp .eq. 2 .or. cdatyp .eq. 4.or.cdatyp.eq.130.or.cdatyp.eq.132) then
            allocate(ifld_in(ni, nj))
            allocate(ifld_out(li, lj))
	         key = ipgsmluk(ifld_in, irec, ni, nj, nk, cnomvar, cigtyp)
            call cvtrfi(tmpif1, ifld_in, ni, nj)
         else
	         key = pgsmluk(tmpif1, irec, ni, nj, nk, cnomvar, cigtyp)
         endif

         if (printen) call imprime(cnomvar, tmpif1, ni, nj)
         if (cigtyp == 'A' .or. cigtyp == 'B' .or. cigtyp == 'G') then
            if (ig1 /= 0) sym = symetri(cnomvar)
         endif
!
!     on ne fait pas d'interpolation si igtyp=grtyp  ig1=lg1  ig2=lg2
!     ig3=lg3  ig4=lg4
!
         if (.not.skip) then
            if (cigtyp /= cgrtyp.or. &
               ig1 /= lg1.or.ig2 /= lg2.or.ig3 /= lg3.or.ig4 /= lg4.or. &
               li /= ni.or.lj /= nj) then
!
!     interpolation
!
               allocate(tmpif2(li,lj))
               gdin = ezqkdef(ni, nj, cigtyp, ig1, ig2, ig3, ig4, iunit)
               ier = ezdefset(gdout, gdin)
               ier = ezsint(tmpif2, tmpif1)
               tmpout => tmpif2
            else
               tmpout => tmpif1
               if (message) then
                  write(app_msg, 660) cnomvar
                  call app_log(APP_WARNING,app_msg)
               endif
660  format(2x, 'AUCUNE INTERPOLATION HORIZONTALE CHAMP=', a4)
            endif
!
!
!     ecrire sur fichier approprie(std, ms, seq)
!
            if (cnomx.eq.cnomqr) then
               call qaaqr(tmpif2, li, lj, tmplat)
               cnomvar=cnomqr
            endif

!            print *, 'Avant iecritur : cdatyp', cdatyp
            if (cdatyp .eq. 2 .or. cdatyp .eq. 4.or.cdatyp.eq.130.or.cdatyp.eq.132) then
!            print *, 'Avant iecritur : cdatyp', cdatyp
               call cvtifr(ifld_out,tmpout,li,lj)
               call iecritur(ifld_out, npack, dateo, deet, npas, li, lj, nk, jp1, jp2, jp3, ctypvar, cnomvar, cetiket, cgrtyp, lg1, lg2, lg3, lg4)
               deallocate(ifld_in, ifld_out)
            else
               call ecritur(tmpout, npack, dateo, deet, npas, li, lj, nk, jp1, jp2, jp3, ctypvar, cnomvar, cetiket, cgrtyp, lg1, lg2, lg3, lg4)
               if (associated(tmpif2)) then
                  deallocate(tmpif2)
               endif
               deallocate(tmpif1)
            endif
            endif
99999       continue
!
            if (unefois) goto 23000
!
         enddo
22000 enddo
23000 continue
!
      if (nunv.gt.0) then
         call app_log(APP_WARNING,'scalair: No interpolation on variable pair CHAMP(TOUT, TOUT), you have to use a variable name ex: CHAMP(UU, TOUT)')
         call app_log(APP_WARNING,'scalair: Vector interpolation will be scalar')
      endif
   deallocate(liste)
   return
   end


   subroutine cvtrfi(rfld, ifld, ni, nj)
   implicit none
   integer ni, nj
   real rfld(ni, nj)
   integer ifld(ni, nj)
   integer i,j

   do j=1, nj
      do i=1, ni
         rfld(i, j) = real(ifld(i, j))
      enddo
   enddo
   return
   end


   subroutine cvtifr(ifld, rfld, ni, nj)
   implicit none
   integer ni, nj
   integer ifld(ni, nj)
   real rfld(ni, nj)
   integer i,j

   do j=1, nj
      do i=1, ni
         ifld(i, j) = nint(rfld(i, j))
      enddo
   enddo
   return
   end

