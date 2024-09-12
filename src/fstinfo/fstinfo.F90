      program fstinfo
      use app
      use rmn_fst24

      implicit none
#include "fst-tools_build_info.h"


      integer pndatprinto,pndatev,pndatprintv,pntimeo,pntimev
      integer pnip1,pnip2,pnip3

      integer pnseqout,ilen
      integer wkoffit,ikind
      external qqexit,ccard,wkoffit

      logical :: success
      type(fst_file)   :: source
      type(fst_record) :: record
      type(fst_query)  :: query

      character(len=1)  :: ptdel
      character(len=2)  :: pttypvar
      character(len=4)  :: ptnomvar
      character(len=12) :: ptetiket

      character(len=8) :: cltimev
      character(len=8) :: ptcle(13)
      character(len=4096) :: ptvar(13), ptdefvar(13)

      data ptcle /'izfst.','datev.','vdatev.','etiket.','ip1.','ip2.','ip3.','typvar.','nomvar.','otxt.','del.','champs.','meta.'/
      data ptvar /'bidon','-1','-1',' ','-1','-1','-1',' ',' ','fstlist',':','0','0'/
      data ptdefvar /'bidon','-1','-1',' ','-1','-1','-1',' ',' ','fstlist',':','1','1'/

      call ccard(ptcle,ptdefvar,ptvar,13,-1)

!     ------ Liste de champs -----
      if (ptvar(12).eq.'1') then

         write(*,'(27(/,1x,a))')     &
              '1 nomvar',            &
              '2 typvar',            &   
              '3 ip1',               &
              '4 ip2',               &
              '5 ip3',               &
              '6 ni',                &
              '7 nj',                &
              '8 nk',                &
              '9 etiket',            &
              '10 date d origine',   & 
              '11 date de validite', &
              '12 deet',             &
              '13 npas',             &
              '14 grtyp',            &
              '15 ig1',              &
              '16 ig2',              &
              '17 ig3',              &
              '18 ig4',              &
              '19 datyp',            &
              '20 nbits',            &
              '21 swa',              &
              '22 lng',              &
              '23 dltf',             &
              '24 ubc',              &
              '25 extra1',           &
              '26 extra2',           &
              '27 extra3',           &
              '28 metadata' 
         stop
      endif

      app_ptr=app_init(0,'fstinfo',VERSION,'',BUILD_TIMESTAMP)
      call app_logstream('stdout')
      call app_start()

      ikind = wkoffit(ptvar(1))
!      write(*,*) 'IKIND ====== ',ikind
      if(ikind .ne. 39 .and. ikind .ne. 33 .and. ikind .ne. 34 .and. ikind .ne. 1) then
         call app_log(APP_ERROR,'File is not RPN')                  
         app_status=app_end(-1)
         call qqexit(app_status)
      endif

!     ------ Initialisation des clefs de recherche -----

      if (ptvar(2) .ne. '-1') then
        read(ptvar(2),*) pndatev
      elseif(ptvar(3) .ne. '-1') then
        read(ptvar(3)(1:8),*) pndatprintv
        read(ptvar(3)(9:),'(a8)') cltimev
        ilen = len_trim(cltimev)
        cltimev = '00000000'
        read(ptvar(3)(9:9+ilen),'(a)') cltimev(1:ilen)
        read(cltimev,*) pntimev
        call newdate(pndatev,pndatprintv,pntimev,3)
      else
        pndatev = -1
      endif
      read(ptvar(5),*) pnip1
      read(ptvar(6),*) pnip2
      read(ptvar(7),*) pnip3

      ptetiket=ptvar(4)
      pttypvar=ptvar(8)
      ptnomvar=ptvar(9)
      ptdel=ptvar(11)
 
!     ------ Ouverture des fichiers ------

      pnseqout = 21
      open(pnseqout,FILE=ptvar(10),ACCESS='SEQUENTIAL')

      success = source%open(trim(ptvar(1)),'STD+R/O+REMOTE')
      query = source%new_query(datev=pndatev, etiket=ptetiket, ip1=pnip1, ip2=pnip2, ip3=pnip3, typvar=pttypvar, nomvar=ptnomvar)

      do while(query%find_next(record))
 
         call newdate(record%dateo,pndatprinto,pntimeo,-3)
         call newdate(record%datev,pndatprintv,pntimev,-3)

         if (ptvar(13).eq.'1') then
            success = record%read_metadata()  
            write (pnseqout,1001) record%nomvar,ptdel,record%typvar,ptdel,record%ip1,ptdel,record%ip2,ptdel,record%ip3 &
                  ,ptdel,record%ni,ptdel,record%nj,ptdel,record%nk,ptdel,record%etiket,ptdel,pndatprinto,pntimeo        &
                  ,ptdel,pndatprintv,pntimev,ptdel,record%deet,ptdel,record%npas                                        &
                  ,ptdel,record%grtyp,ptdel,record%ig1,ptdel,record%ig2,ptdel,record%ig3,ptdel,record%ig4               &
                  ,ptdel,record%data_type,ptdel,record%data_bits,ptdel,0,ptdel,0                                        &
                  ,ptdel,0,ptdel,0,ptdel,record%datev,ptdel,0,ptdel,0,ptdel,record%metadata%stringify(JSON_C_TO_STRING_SPACED)
         else
            write (pnseqout,1000) record%nomvar,ptdel,record%typvar,ptdel,record%ip1,ptdel,record%ip2,ptdel,record%ip3 &
                  ,ptdel,record%ni,ptdel,record%nj,ptdel,record%nk,ptdel,record%etiket,ptdel,pndatprinto,pntimeo        &
                  ,ptdel,pndatprintv,pntimev,ptdel,record%deet,ptdel,record%npas                                        &
                  ,ptdel,record%grtyp,ptdel,record%ig1,ptdel,record%ig2,ptdel,record%ig3,ptdel,record%ig4               &
                  ,ptdel,record%data_type,ptdel,record%data_bits,ptdel,0,ptdel,0                                        &
                  ,ptdel,0,ptdel,0,ptdel,record%datev,ptdel,0,ptdel,0
         endif
      end do
      success = source%close()

 1000 format(2(a,a),6(i10,a),1(a,a),2(i8.8,i8.8,a),2(i10,a),1(a,a),13(i10,a))
 1001 format(2(a,a),6(i10,a),1(a,a),2(i8.8,i8.8,a),2(i10,a),1(a,a),13(i10,a),a)

      app_status=app_end(-1);
      call qqexit(app_status)
      stop
      end
