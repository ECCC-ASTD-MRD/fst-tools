      program woir

*Revision 98.11 - M. Lepine - reload, detection mauvais type de fichier (xdfopn)
*         98.12 - M. Lepine - utilisation de ccard_arg
*         98.13 - M. Lepine - flush des buffers de stdout dans exfin
*         98.14 - M. Lepine - reload librmn_008
*         98.15 - M. Lepine - reload librmnbeta, correction impression dans convip

      parameter (ncle=4)
      integer fnom,fstouv,fstvoi,fstfrm,exdb,exfin
      external fnom,fstouv,fstvoi,fstnbr,ccard,c_init_appl_var_table
      character *8192 ccard_arg
      external exdb,exfin,ccard_arg

      character * 8 cles(ncle)
      character * 128 val(ncle), def(ncle)
      data cles / 'IMENT:','SEQ','STYLE','HELP' /
      data def / 'scrap','SEQ' ,'NINJNK+DATEV+LEVEL+IP1+GRIDINFO',
     %           'MOREHELP'/
      data val / 'scrap','RND' ,'NINJNK+DATEO+IP1+IG1234',' '/

      call c_init_appl_var_table()
      ipos = -1
      call ccard (cles,def,val,ncle,ipos)

      IF (val(4) .eq. 'MOREHELP') THEN
         print *,"*** VOIR CALLING SEQUENCE ***"
         print *
         print *,'-IMENT [scrap:scrap]'
         print *,'-SEQ [RND:SEQ]'
         print *,
     %'-STYLE [NINJNK DATEO IP1 IG123:NINJNK DATEV LEVEL IP1 GRIDINFO]'
         print *,'   List of possible items for STYLE argument:'
         print *,'         NINJNK: display ni nj nk dimensions'
         print *,"          DATEO: display origin date"     
         print *,'     DATESTAMPO: display origin datetimestamp',
     %           ' for the nostalgics'
         print *,'          DATEV: display valid date and stamp'
         print *,'          LEVEL: display vertical level'
         print *,'            IP1: display coded IP1 value'
         print *,'       GRIDINFO: display decoded grid information'
         print *,'         IG1234: display IG1 IG2 IG3 IG4 values'
         print *
         print *,'     The following items suppress variable printout'
         print *,'         NONOMV: suppress NOMV information'
         print *,'         NOTYPV: suppress TYPV information'
         print *,'         NOETIQ: suppress ETIQUETTE information'
         print *,'         NOIP23: suppress IP2, IP3 information'
         print *,'         NODEET: suppress DEET information'
         print *,'         NONPAS: suppress NPAS information'
         print *,'          NODTY: suppress DTY information'
         print *
         print *,'   Example #1: -style "ninjnk datev level"'       
         print *,'   Example #2: -style datev+level+ip1+notypv'
      else
         ier = exdb('VOIR','V98.14','NON')
c         print *,'Debug+ ccard_arg(cles(1)) = ',
c     %   trim(ccard_arg(cles(1)))
         ier = fnom(10,trim(ccard_arg(cles(1))),
     %             'STD+R/O+REMOTE'//val(2),0)
         if (ier .ge. 0) then
            N = fstouv(10,VAL(2))
            ier = fstvoi(10,val(3))
            ier = fstfrm(10)
            ier = exfin('VOIR','O.K.','NON')
         else
            ier = exfin('VOIR','<< ERREUR >>','NON')
         endif
      endif
      stop
      end
      
      character *128 function product_id_tag()
      product_id_tag='$Id$'
      return
      end
