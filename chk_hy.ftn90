      subroutine chk_hy(lu_in, lu_out)
      implicit none
      
      integer lu_in, lu_out
      external fstinl,fstprm, fstecr, fstluk, fstinf
      integer fstinl, fstprm, fstecr, fstluk, fstinf
      integer liste(256)
      logical rewrit
      
      character *12 cetiket
      character *4 cnomvar,cnomx
      character *2 ctypvar
      character *1 cigtyp
      
      integer dateo,datev,i
      integer deet,ig1,ig2,ig3,ig4
      integer irec,irec_out,ip1,ip2,ip3,ni,nj,nk,nrecs,ier,npas
      integer nbits,cdatyp,cswa,clng,cdltf,cubc,extra1,extra2,extra3
      real hydata(4096)

      irec=fstinl(lu_in,ni,nj,nk,-1,'            ',-1,-1,-1,'  ','HY  ', liste, nrecs, 256)

      rewrit = .true.
      do i=1,nrecs
         irec = liste(i)
         ier=fstprm(irec, dateo,deet,npas,ni, nj, nk, nbits,cdatyp,         ip1,ip2,ip3,ctypvar,cnomvar,cetiket,         cigtyp,ig1,ig2, ig3, ig4, cswa, clng, cdltf, cubc,          datev, extra2, extra3)
         irec_out=fstinf(lu_out,ni,nj,nk,dateo,cetiket,ip1,ip2,ip3,ctypvar,cnomvar)
	 if (irec_out < 0) then
	    ier=fstluk(hydata, irec, ni, nj, nk)
	    ier = fstecr(hydata,hydata,-nbits,lu_out,dateo,deet,npas,            ni,nj,nk,ip1,ip2,ip3,ctypvar,cnomvar,cetiket,            cigtyp,ig1,ig2,ig3,ig4,cdatyp,rewrit )
	 endif 
      enddo
         

      return
      end
