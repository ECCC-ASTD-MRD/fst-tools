!
!**s/p uvectur interpolation des vecteurs u-v (horizontalement)
   subroutine uvecteur_masque(key_uu, key_vv)
      use app
      implicit none
      
      integer :: key_uu, key_vv
#include "defin.cdk90"
      external ecritur,pgsmluk,fstinf,fstsui,memoir,fstprm,qaaqr,fstcvt, &
         fstsel,symetri,imprime,itrouve,messags,pgsmabt
      external cvtr2i
      external liraxez
      integer  pgsmluk, fstinf, fstsui, fstprm, fstcvt, fstsel, fstinl, fstluk

      integer ezgdef_fmem, ezqkdef, ezuvint, ezuvint_mdm, ezwdint, ezdefset, fst_get_mask_key, key_mask
      logical skip

!
#include "accum.cdk90"
#include "dates.cdk90"
#include "defin.cdk90"
#include "dummys.cdk90"
#include "ecrires.cdk90"
#include "gdz.cdk90"
#include "grilles.cdk90"
#include "indptr.cdk90"
#include "lires.cdk90"
#include "llccmm.cdk90"
#include "packin.cdk90"
#include "pairs.cdk90"
#include "voir.cdk90"


   character(len=12):: cetiket
   character(len=4) :: cnom_uu, cnom_vv
   character(len=2) :: ctypvar
   character(len=1) :: cigtyp

   integer i, j, nunv, itrouve, ii, key_mask_out, lk
   integer deet, ig1, ig2, ig3, ig4, iheur
   integer dateo, datev, nbits, datyp, ip1, ip2, ip3, iun_out
   integer iprs, irec, iunit, ne, ni, nj, nk, total_keys, nrecs
   integer cnbits, cdatyp, cswa, clng, cdltf, cubc, extra1, extra2, extra3

   integer dateo_mask, deet_mask, npas_mask, ni_mask, nj_mask, nk_mask
   integer nbits_mask, datyp_mask, ip1_mask, ip2_mask, ip3_mask
   integer ig1_mask, ig2_mask, ig3_mask, ig4_mask, cswa_mask, clng_mask, cdltf_mask, cubc_mask
   integer datev_mask, extra2_mask, extra3_mask
   logical sym, symetri

   character(len=4)  :: cnomvar_mask
   character(len=2)  :: ctypvar_mask
   character(len=12) :: cetiket_mask
   character(len=1)  :: cigtyp_mask

   real fbidon
   real, dimension(:,:), allocatable, target :: tmp_uuin, tmp_uuout, tmp_vvin, tmp_vvout
   integer, dimension(:,:), allocatable, target :: masque_in, masque_out

   real, dimension(:,:), pointer :: uu_in, vv_in, uu_out, vv_out
   integer, dimension(:,:), pointer :: tmpmsk


   logical masque_present, masque_done, write_masque, ssw

   nunv=0
   iunit=1
   masque_present = .false.
   masque_done = .false.
   ctypvar_mask = '@@'

   ier = fstprm(key_uu, dateo, deet, npas, ni, nj, nk, nbits, datyp, ip1, ip2, ip3, ctypvar, cnom_uu, cetiket, &
            cigtyp, ig1, ig2, ig3, ig4, cswa, clng, cdltf, cubc, datev, extra2, extra3)
   ier = fstprm(key_vv, dateo, deet, npas, ni, nj, nk, nbits, datyp, ip1, ip2, ip3, ctypvar, cnom_vv, cetiket, &
            cigtyp, ig1, ig2, ig3, ig4, cswa, clng, cdltf, cubc, datev, extra2, extra3)
   allocate(tmp_uuin(ni,nj), tmp_vvin(ni,nj), tmp_uuout(li,lj), tmp_vvout(li,lj))

   ier = fst_get_mask_key(key_mask, key_uu, 0, iunit)

   if (key_mask >= 0) then
      masque_present = .true.
      allocate(masque_in(ni,nj), masque_out(li,lj))
   else
      masque_present = .false.
   endif

   ier = pgsmluk(tmp_uuin, key_uu, ni,nj,nk,cnom_uu,cigtyp)
   ier = pgsmluk(tmp_vvin, key_vv, ni,nj,nk,cnom_vv,cigtyp)


   if (printen)  call imprime(cnom_uu,tmp_uuin,ni,nj)
   if (ig1 /= 0) sym = symetri(cnom_uu)

!  interpolation ordinaire sans masque

   if (.not.masque_present) then
!     si vvent=.true. on calcule la vitesse du vent

      gdin = ezqkdef(ni, nj, cigtyp, ig1, ig2, ig3, ig4, iunit)
      ier = ezdefset(gdout, gdin)

      if (vvent) then
         ssw=.false.
         if (wdvent) then
            ssw = .true.
         endif

         ier = ezwdint(tmp_uuout, tmp_vvout, tmp_uuin, tmp_vvout)
         uu_out => tmp_uuout

         call  ecritur(uu_out,npack,dateo,deet,npas,li,lj,nk,ip1,ip2,ip3,ctypvar,'UV  ',cetiket,cgrtyp,lg1,lg2,lg3,lg4)

         if (wdvent) then
            do j=1,lj
               do i=1,li
                  if (tmp_vvout(i,j).lt.0.0) then
                     tmp_vvout(i,j) = tmp_vvout(i,j) + 360.0
                  endif
               enddo
            enddo
            vv_out => tmp_vvout
            call ecritur(vv_out,npack,dateo,deet,npas,li,lj,nk,ip1,ip2,ip3,ctypvar,'WD  ',cetiket,cgrtyp,lg1,lg2,lg3,lg4)
         endif
      else
         if (cigtyp.ne.cgrtyp.or.ig1.ne.lg1.or.ig2.ne.lg2.or.ig3.ne.lg3.or.ig4.ne.lg4.or.li.ne.ni.or.lj.ne.nj) then
            ssw = .true.
            ier = ezuvint(tmp_uuout, tmp_vvout, tmp_uuin, tmp_vvout)
            uu_out => tmp_uuout
            vv_out => tmp_vvout
         else
            deallocate(tmp_uuout)
            deallocate(tmp_vvout)
            uu_out => tmp_uuin
            vv_out => tmp_vvout
            if (message) then
               call app_log(APP_WARNING,'uvecteur_masque: No horizontal interpolation')
            endif
         endif
         call ecritur(uu_out,npack,dateo,deet,npas,li,lj,nk,ip1,ip2,ip3,ctypvar,cnom_uu,cetiket,cgrtyp,lg1,lg2,lg3,lg4)
         call ecritur(vv_out,npack,dateo,deet,npas,li,lj,nk,ip1,ip2,ip3,ctypvar,cnom_vv,cetiket,cgrtyp,lg1,lg2,lg3,lg4)
      endif
   else  !!! Masque present
!
      if (cigtyp /= cgrtyp.or.cigtyp == 'Z'.or.ig1 /= lg1.or.ig2 /= lg2.or.ig3 /= lg3.or.ig4 /= lg4.or.li /= ni.or.lj /= nj) then
         gdin = ezqkdef(ni, nj, cigtyp, ig1, ig2, ig3, ig4, iunit)
         ier = ezdefset(gdout, gdin)
         ier = fstprm(key_mask, dateo_mask, deet_mask, npas_mask, ni_mask, nj_mask, nk_mask, &
                  nbits_mask, datyp_mask, ip1_mask, ip2_mask, ip3_mask, &
                  ctypvar_mask, cnomvar_mask, cetiket_mask, &
                  cigtyp_mask, ig1_mask, ig2_mask, ig3_mask, ig4_mask, cswa_mask, clng_mask, &
                  cdltf_mask, cubc_mask, datev_mask, extra2_mask, extra3_mask)
         ier = fstluk(masque_in, key_mask, ni,nj,nk)
         ier = ezuvint_mdm(tmp_uuout, tmp_vvout, masque_out, tmp_uuin, tmp_vvin, masque_in)
         uu_out => tmp_uuout
         vv_out => tmp_vvout
         tmpmsk => masque_out
      else
         uu_out => tmp_uuin
         vv_out => tmp_vvin
         tmpmsk => masque_in
         if (message) then
            write(app_msg,662) cnom_uu
 662           format(2x,'uvecteur_masque: No horizontal interpolation CHAMP=',a4)
            call app_log(APP_WARNING,app_msg)
         endif
      endif

      call ecritur(uu_out,npack,dateo,deet,npas,li,lj,nk, ip1, ip2, ip3, &
         ctypvar, cnom_uu, cetiket, cgrtyp, lg1, lg2, lg3, lg4)
      call ecritur(vv_out,npack,dateo,deet,npas,li,lj,nk, ip1, ip2, ip3, &
         ctypvar, cnom_vv, cetiket, cgrtyp, lg1, lg2, lg3, lg4)

      ! On regarde si le masque existe dans le fichier de sortie

      iun_out = 2
      key_mask_out = fstinf(iun_out, ni_mask, nj_mask, nk_mask, datev_mask, cetiket_mask, ip1_mask, ip2_mask, ip3_mask, &
         ctypvar_mask, cnomvar_mask)

      write_masque = .true.
      if (key_mask_out >= 0) then
         write_masque = .false.
      endif

      if (masque_present.and.write_masque) then
         call iecritur(tmpmsk,-nbits_mask,dateo_mask,deet_mask,npas_mask,li,lj, nk,&
            ip1_mask,ip2_mask, ip3_mask, ctypvar_mask, cnomvar_mask, cetiket_mask, &
            cgrtyp, lg1, lg2, lg3, lg4)
         if (allocated(masque_out)) then
            deallocate(masque_out, masque_in)
         endif
      endif
      deallocate(tmp_uuout, tmp_uuin, tmp_vvout, tmp_vvin)

   endif

!
   if (nunv > 0) then
      write(6,666)
 666     format(' AUCUNE INTERPOLATION SUR VARIABLE PAIRE CHAMP(TOUT,TOUT)')
      write(6,668)
 668     format(' ON DOIT UTILISER LE NOM DE LA VARIABLE EX: CHAMP(UU,TOUT)')
      write(6,669)
 669  format(' ATTENTION L INTERPOLATION DES VECTEURS SERA SCALAIRE (!!!)')
!
   endif
   return
   end

