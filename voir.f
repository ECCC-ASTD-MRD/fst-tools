      program woir

      parameter (ncle=3)
      integer fnom,fstouv,fstvoi,fstfrm,exdb,exfin
      external fnom,fstouv,fstvoi,fstnbr,ccard
      external exdb,exfin

      character * 8 cles(ncle)
      character * 128 val(ncle), def(ncle)
      data cles / 'IMENT.','SEQ','STYLE' /
      data def / 'scrap','SEQ' ,'NEWSTYLE'/
      data val / 'scrap','RND' ,'OLDSTYLE'/

      ier = exdb('VOIR','V98.03','NON')
      ipos = -1
      call ccard (cles,def,val,ncle,ipos)

*      ier = fnom(10,val(1),'STD+R/O+'//val(2),0)
      ier = fnom(10,val(1),'STD'//val(2),0)
      if (ier .ge. 0) then
         N = fstouv(10,VAL(2))
         ier = fstvoi(10,val(3))
         ier = fstfrm(10)
         ier = exfin('VOIR','O.K.','NON')
      else
         ier = exfin('VOIR','<< ERREUR >>','NON')
      endif
      stop
      end
