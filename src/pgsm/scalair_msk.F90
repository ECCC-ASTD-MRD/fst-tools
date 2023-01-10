!**s/p scalair  interpolation horizontale d un champ
!               defini par l usager
   subroutine scalair_msk(key, liste, done_liste, len_liste)
      use app
      implicit none

      integer :: key,len_liste
      integer, dimension(len_liste) :: liste
      logical, dimension(len_liste) :: done_liste
#include "defin.cdk90"
      external ecritur,pgsmluk,fstinf,fstsui,memoir,fstprm,qaaqr,fstcvt, &
         fstsel,symetri,imprime,itrouve,messags,pgsmabt
      external cvtifr
      external liraxez
      integer  pgsmluk, fstinf, fstsui, fstprm, fstcvt, fstsel, fstinl, fstluk

      integer ezgdef_fmem, ezqkdef, ezsint, ezsint_mdm, ezdefset, fst_get_mask_key, key_masq
      logical skip

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


   character *12 cetiket
   character *4 cnom,cnomvar,cnomx
   character *2 ctypvar
   character *1 cigtyp

   integer nniv, i, nunv, itrouve, ii
   integer niveaux(512), deet, ig1, ig2, ig3, ig4, iheur
   integer dateo, datev, nbits, datyp, ip1, ip2, ip3
   integer iprs, irec, iunit, ne, ni, nj, nk, total_keys, nrecs
   integer cnbits, cdatyp, cswa, clng, cdltf, cubc, extra1, extra2, extra3
   logical, save :: unefoys = .true.

   integer dateo_masq, deet_masq, npas_masq, ni_masq, nj_masq, nk_masq
   integer nbits_masq, datyp_masq, ip1_masq, ip2_masq, ip3_masq
   integer ig1_masq, ig2_masq, ig3_masq, ig4_masq, cswa_masq, clng_masq, cdltf_masq, cubc_masq
   integer datev_masq, extra2_masq, extra3_masq
   logical sym, symetri

   character(len=4)  :: cnomvar_masq
   character(len=2)  :: ctypvar_masq
   character(len=12) :: cetiket_masq
   character(len=1)  :: cigtyp_masq

   real fbidon
   real, dimension(:,:), allocatable, target ::  fld, fld_out
   integer, dimension(:,:), allocatable, target :: masq, masq_out, masq_zones

   real, dimension(:,:), pointer :: tmpout
   integer, dimension(:,:), pointer :: tmpmsk

   logical masque_present, masque_done

   nunv=0
   iunit=1
   masque_present = .false.
   masque_done = .false.
   ctypvar_masq = '@@'

   ier = fstprm(key, dateo, deet, npas, ni, nj, nk, nbits, datyp, ip1, ip2, ip3, ctypvar, cnomvar, cetiket, &
            cigtyp, ig1, ig2, ig3, ig4, cswa, clng, cdltf, cubc, datev, extra2, extra3)
   allocate(fld(ni,nj), fld_out(li,lj))

   ier = fst_get_mask_key(key_masq, key, 0, iunit)

   if (key_masq >= 0) then
      masque_present = .true.
      allocate(masq(ni,nj), masq_out(li,lj))
   else
      masque_present = .false.
   endif

   key = pgsmluk(fld, key, ni,nj,nk,cnomvar,cigtyp)

   if (cdatyp  ==  2 .or. cdatyp  ==  4) then
      call cvtifr(fld, fld, ni, nj)
   endif

   if (printen)  call imprime(cnomvar,tmpif1,ni,nj)
   if (ig1 /= 0) sym = symetri(cnomvar)
!
!  on ne fait pas d'interpolation si igtyp=grtyp  ig1=lg1  ig2=lg2
!  ig3=lg3  ig4=lg4
!
   if (.not.masque_present) then
      if (cigtyp /= cgrtyp.or.cigtyp == 'Z'.or.ig1 /= lg1.or.ig2 /= lg2.or.ig3 /= lg3.or.ig4 /= lg4.or.li /= ni.or.lj /= nj) then
         gdin = ezqkdef(ni, nj, cigtyp, ig1, ig2, ig3, ig4, iunit)
         ier = ezdefset(gdout, gdin)
         ier = ezsint(fld_out, fld)
         tmpout => fld_out
      else
         tmpout => fld
         if (message) then
            write(app_msg,660) cnom
            call app_log(APP_INFO,app_msg)
         endif
         
 660           format(2x,'scalair_msk: No horizonal interpolation CHAMP=',a2)
      endif
!     ecrire sur fichier approprie(std,ms,seq)
      if (cnomx == cnomqr) then
         call qaaqr(tmpif2,li,lj,tmplat)
         cnomvar=cnomqr
      endif
      call ecritur(tmpout,npack,dateo,deet,npas,li,lj,nk,ip1,ip2, ip3, ctypvar, cnomvar, cetiket, cgrtyp, lg1, lg2, lg3, lg4)
      deallocate(fld_out)
   else
      if (cigtyp /= cgrtyp.or.cigtyp == 'Z'.or.ig1 /= lg1.or.ig2 /= lg2.or.ig3 /= lg3.or.ig4 /= lg4.or.li /= ni.or.lj /= nj) then
         gdin = ezqkdef(ni, nj, cigtyp, ig1, ig2, ig3, ig4, iunit)
         ier = ezdefset(gdout, gdin)
         ier = fstprm(key_masq, dateo_masq, deet_masq, npas_masq, ni_masq, nj_masq, nk_masq, &
                  nbits_masq, datyp_masq, ip1_masq, ip2_masq, ip3_masq, &
                  ctypvar_masq, cnomvar_masq, cetiket_masq, &
                  cigtyp_masq, ig1_masq, ig2_masq, ig3_masq, ig4_masq, cswa_masq, clng_masq, &
                  cdltf_masq, cubc_masq, datev_masq, extra2_masq, extra3_masq)
         ier = fstluk(masq, key_masq, ni,nj,nk)
         ier = ezsint_mdm(fld_out, masq_out, fld, masq)
         tmpout => fld_out
         tmpmsk => masq_out
      else
         tmpout => fld
         tmpmsk => masq
         if (message) then
            write(app_msg,660) cnom
            call app_log(APP_INFO,app_msg)
         endif
      endif
!     ecrire sur fichier approprie(std,ms,seq)
      if (cnomx == cnomqr) then
         call qaaqr(tmpif2,li,lj,tmplat)
         cnomvar=cnomqr
      endif
      call ecritur(tmpout,npack,dateo,deet,npas,li,lj,nk, ip1, ip2, ip3, &
         ctypvar, cnomvar, cetiket, cgrtyp, lg1, lg2, lg3, lg4)
         masque_done = .false.
         do i=1,len_liste
            if (liste(i) == key_masq) then
               if (done_liste(i)) then
                  masque_done = .true.
               endif
               exit
            endif
         enddo
      if (masque_present.and..not.masque_done) then
         if (unefoys) then
            allocate(masq_zones(li,lj))
            call ezget_mask_zones(masq_zones, masq)
            nk = 1
            call iecritur(masq_zones,-16,dateo_masq,deet_masq,npas_masq,li,lj, nk,&
               ip1_masq,ip2_masq, ip3_masq, '@Z', cnomvar_masq, cetiket_masq, &
               cgrtyp, lg1, lg2, lg3, lg4)
            unefoys = .false.
            deallocate(masq_zones)
         endif
         call iecritur(tmpmsk,-nbits_masq,dateo_masq,deet_masq,npas_masq,li,lj, nk,&
            ip1_masq,ip2_masq, ip3_masq, ctypvar_masq, cnomvar_masq, cetiket_masq, &
            cgrtyp, lg1, lg2, lg3, lg4)
         do i=1,len_liste
            if (liste(i) == key_masq) then
               done_liste(i) = .true.
               exit
            endif
         enddo
         if (allocated(masq_out)) then
            deallocate(masq_out)
         endif
      endif
      deallocate(fld_out)

   endif
   deallocate(fld)

!
   if (nunv > 0) then
      call app_log(APP_WARNING,'scalair_msk: No interpolation on variable pair CHAMP(TOUT, TOUT), you have to use a variable name ex: CHAMP(UU, TOUT)')
      call app_log(APP_WARNING,'scalair_msk: Vector interpolation will be scalar')
   endif
   return
   end