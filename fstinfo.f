      program fstinfo
c
      implicit none
c
      integer pnier,pnfstsrc,pnnis,pnnjs,pnnks
      integer fnom,fstouv,fstfrm,fstinf,pnkey,fstsui
c
      integer pndateo,pndatprinto,pndatev,pndatprintv,pntimeo
      integer pntimev,pndeet,pnnpas,pnnbits
      integer pndatyp, pnip1,pnip2,pnip3
      integer pnig1,pnig2,pnig3,pnig4,pnswa,pnlng,pndltf,pnubc,pnextra1
      integer pnextra2,pnextra3
c
      integer fstprm,pnseqout
c
      real*8 prtmp8
c
      character*1 ptgrtyp,ptdel
      character*2 pttypvar
      character*4 ptnomvar
      character*12 ptetiket
c
      character*8 ptcle(11)
      character*128 ptvar(11), ptdefvar(11)
c
      data ptcle /'izfst.','datev.','etiket.','ip1.','ip2.','ip3.',
     $     'typvar.','nomvar.','otxt.','del.','champs.'/
      data ptvar /'bidon','-1',' ','-1','-1','-1',' ',' ','fstlist',':',
     %'0'/ 
      data ptdefvar /'bidon','-1',' ','-1','-1','-1',' ',' ','fstlist','
     #','1'/ 
      data pnfstsrc /11/
c
      call ccard(ptcle,ptdefvar,ptvar,11,-1)
c
c     ------ Liste de champs -----
      if (ptvar(11).eq.'1') then
c
         write(*,'(27(/,1x,a))')
     $        '1 nomvar',
     $        '2 typvar',
     $        '3 ip1',
     $        '4 ip2',
     $        '5 ip3',
     $        '6  ni',
     $        '7  nj',
     $        '8  nk',
     $        '9 etiket',
     $        '10  date d origine',
     $        '11 date de validite',
     $        '12 deet',
     $        '13  npas',
     $        '14 grtyp',
     $        '15 ig1',
     $        '16 ig2',
     $        '17 ig3',
     $        '18 ig4',
     $        '19  datyp',
     $        '20  nbits',
     $        '21 swa',
     $        '22 lng',
     $        '23 dltf',
     $        '24 ubc',
     $        '25 extra1',
     $        '26 extra2',
     $        '27 extra3'
         stop
      endif
c
c     ------ Initialisation des clefs de recherche -----
c
      read(ptvar(2),*) pndatev
      read(ptvar(4),*) pnip1
      read(ptvar(5),*) pnip2
      read(ptvar(6),*) pnip3
c
      ptetiket=ptvar(3)
      pttypvar=ptvar(7)
      ptnomvar=ptvar(8)
      ptdel=ptvar(10)
c
c     ------ Ouverture des fichiers ------
c
      pnseqout = 21
      open(pnseqout,FILE=ptvar(9),ACCESS='SEQUENTIAL')
c
      pnier =  fnom  (pnfstsrc,ptvar(1),'RND',0)
      pnier =  fstouv(pnfstsrc,'RND')
c
      pnkey=fstinf(pnfstsrc,pnnis,pnnjs,pnnks,pndatev,
     $     ptetiket, pnip1, pnip2, pnip3, pttypvar,
     $     ptnomvar)
c
 100  if (pnkey.ge.0) then
         pnier=fstprm(pnkey,pndateo,pndeet,pnnpas,pnnis,pnnjs,pnnks,pnnb
     %its,
     $        pndatyp,pnip1,pnip2,pnip3,pttypvar,ptnomvar,ptetiket,ptgrt
     %yp,pnig1,pnig2,
     $        pnig3,pnig4,pnswa,pnlng,pndltf,pnubc,pnextra1,pnextra2,pne
     %xtra3)
c
         prtmp8=pndeet
         prtmp8=prtmp8*pnnpas/3600.
c
         call incdatr(pndatev,pndateo,prtmp8)
c
         call newdate(pndateo,pndatprinto,pntimeo,-3)
         call newdate(pndatev,pndatprintv,pntimev,-3)
c
         write (pnseqout,1000) ptnomvar,ptdel,pttypvar,ptdel,pnip1,ptdel
     &        ,pnip2,ptdel,pnip3,ptdel,pnnis,ptdel,pnnjs,ptdel,pnnks
     &        ,ptdel,ptetiket,ptdel,pndatprinto,pntimeo,ptdel
     &        ,pndatprintv,pntimev,ptdel,pndeet,ptdel,pnnpas,ptdel
     &        ,ptgrtyp,ptdel,pnig1,ptdel,pnig2,ptdel,pnig3,ptdel,pnig4
     &        ,ptdel,pndatyp,ptdel,pnnbits,ptdel,pnswa,ptdel,pnlng,ptdel
     &        ,pndltf,ptdel,pnubc,ptdel,pnextra1,ptdel,pnextra2,ptdel
     &        ,pnextra3
c
         pnkey=fstsui(pnfstsrc,pnnis,pnnjs,pnnks)
         goto 100
       endif
c
      pnier =  fstfrm(pnfstsrc)
      close(pnseqout)
c
 1000 format(2(a,a),6(i10,a),1(a,a),2(i8.8,i8.8,a),2(i10,a),1(a,a),12(i1
     %0,a),i10)
c
      stop
      end
