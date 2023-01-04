!
!**S/P SORTI   IDENTIFICATION DU FICHIER..OUVRIR FICHIER...RESERVER MEMOIRE
!
   subroutine sorti( modx, norecs, jwrit)
      use app
      implicit none

      external fnom,fstnbr,fsteof,fstouv,pgsmabt,fstvoi,fstapp,messags
      external fstabt, fstlnk, exfin, pgsmof
      integer  fnom,fstnbr,fsteof,fstouv,fstvoi,fstapp,exfin,pgsmof
!
!AUTEUR P. SARRAZIN DEC 81 DRPN DORVAL P.Q. CANADA
!
!LANGAGE RATFOR
!
!OBJET(SORTI)
!          VERIFIER LA VALEUR DE MODX  RESERVER MEMOIRE, OUVRIR FICHIER
!            MODX=1 FICHIER STANDARD
!                 2 FICHIER ACCES DIRECT (READMS)
!                 3 FICHIER SEQUENTIEL
!                 4 FICHIER SEQUENTIEL AVEC PARAMETRES DE FSTECR
!                 5 Fichier sequentiel ascii (sortie(formatee))
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!ARGUMENTS
!  IN      MODX      1=FICHIER STANDARD
!                    2=FICHIER DIRECT (READMS)
!                    3=FICHIER SEQUENTIEL
!
!  IN     NOENRG     NOMBRE D'ENREGISTREMENT DANS FICHIER
!  IN      JWRIT     -1=REECRIRE SUR FICHIER OU ECRIRE A LA FIN(MS) SORTI(MS,500,A)
!                    -1=ECRIRE SUR FICHIER UN RECORD SANS DETRUIRE UN
!                       RECORD PAREIL   SORTI(STD,500,A)
!                    +1=REECRIRE SUR FICHIER FATAL SI RECORD PAS LA.   SORTI(MS,500,R)
!                    +1=REMPLACE UN RECORD SI DEJA EXISTANT DETRUIT   SORTI(STD,500,R)
!
!IMPLICITES
!
!MESSAGES
!         DEUXIEME APPEL A LA DIRECTIVE SORTIE APPEL IGNORE
!         MAUVAIS APPEL A DIRECTIVE SORTIE FICHIER STD
!         MAUVAISE DIRECTIVE (SORTIE) FICHIER MS
!         DIRECTIVE ENREG=0, INITIALISER A 1
!         MAUVAIS APPEL A SORTIE FICHIER SEQ
!         TYPE DE FICHIER INCONNU
!         MAUVAISE DIRECTIVE MODE DIFFERENT DE (STD,MS,SEQ)
!
!
!----------------------------------------------------------------------
!
!
#include "lnkflds.cdk90"
#include "llccmm.cdk90"
#include "accum.cdk90"
#include "charac.cdk90"
#include "dates.cdk90"
#include "indptr.cdk90"
#include "enrege.cdk90"
#include "voir.cdk90"
!
      integer i,jwrit,modx,norecs
      common/relu/dejalue
      logical dejalue
      data dejalue/.false./
!
!   DATE DE LA RUN OPERATIONNELLE UTILISEE PAR L'USAGER DIRECTIVE OPDAT=OUI
!
      if (lfn(idx_date).ne. 'NON') then
         if (seldat) userdate = jdate
         if (seldat) then
            write(app_msg,*)'sorti: Origin date = valid date = ',userdate
            call app_log(APP_INFO,app_msg)
         endif
      endif
!
      if (dejalue)  then
         if (message) then
            call app_log(APP_WARNING,'sorti: Second call to directive SORTI ignored')
            return
         endif
      endif
      dejalue=.true.
!
      mode = modx
      noenrg = norecs
!
!     NBR1 = MAX0(2,FSTNBR(1))
!
!      ier = fstnbr(1)
!      if (ier .lt. 0) call fstabt
!
!   LE FICHIER D ENTRE NE PEUT ETRE  FICHIER STANDARD SEQUENTIEL
!
      iset = 0
!
!  RESERVER MEMOIR POUR FICHIER D ENTRE
!
!
      if (mode.eq.1)  then
         if (noenrg.eq.1) then
            ier=fnom(lnkdiun(idx_ozsrt),lfn(idx_ozsrt),'STD+SEQ+FTN',0)
         else if (noenrg.eq.0) then
            ier=fnom(lnkdiun(idx_ozsrt),lfn(idx_ozsrt),'STD+SEQ+REMOTE',0)
         else
            ier = fnom(lnkdiun(idx_ozsrt),lfn(idx_ozsrt),'STD+RND+REMOTE',0)

            if (nsort.eq.2) then
               iwrit=+1
            else if (nsort.eq.3) then
               iwrit=jwrit
            else
               call app_log(APP_ERROR,'sorti: Wrong call to directive SORTI std file')
               call pgsmabt
            endif
         endif
         if (ier .lt. 0) then
            call app_log(APP_WARNING,'sorti: Problem openning output file')
	         call pgsmabt
	      endif
      else if (mode.eq.2) then
         call app_log(APP_ERROR,'sorti: "MS" files are not supported in this version of PGSM')
         call pgsmabt
      else if (mode.eq.3.or.mode.eq.4) then
         ier = fnom(lnkdiun(idx_ozsrt),lfn(idx_ozsrt),'SEQ+FTN+UNF',0)

         if (nsort.ne.1)  then
            call app_log(APP_ERROR,'sorti: Wrong call to directive SORTI seq file')
            call pgsmabt
         endif
      else if (mode.eq.5) then
         if (jwrit.eq.-1) then
            ier = pgsmof(lnkdiun(idx_ozsrt),lfn(idx_ozsrt))
!            ier = fnom(2,lfn(41),'SEQ+FMT+APPEND',0)
         else
            ier = pgsmof(lnkdiun(idx_ozsrt),lfn(idx_ozsrt))
            ier = fnom(lnkdiun(idx_ozsrt),lfn(idx_ozsrt),'SEQ+FMT+R/W',0)
         endif
      else
         call app_log(APP_ERROR,'sorti: Unknown file type, wrong directive or mode different from STD,MS,SEQ')
         return
      endif
      if (ier .lt. 0) then
         call app_log(APP_ERROR,'sorti: Problem openning output file')
	      call pgsmabt
      endif
!
!
      if (mode.eq.2) then
         call app_log(APP_ERROR,'sorti: "MS" files are not supported anymore on CYBER-910-920')
         call pgsmabt
      endif

      if (mode.eq.1)  then
         if (noenrg.eq.1)  then
            ier = fstouv(lnkdiun(idx_ozsrt), 'SEQ+FTN')
         else if (noenrg.eq.0) then
            ier = fstouv(lnkdiun(idx_ozsrt), 'SEQ')
            if (jwrit.eq.-1) then
               ier = fstapp(lnkdiun(idx_ozsrt),' ')
               ier = fsteof(lnkdiun(idx_ozsrt))
            endif
         else
            ier = fstouv(lnkdiun(idx_ozsrt), 'RND')
         endif
         if (ier .lt. 0) then
            call app_log(APP_ERROR,'sorti: Problem openning output file')
	         call pgsmabt
         endif
      endif
!
!   OUVRIR FICHIER D'ENTREE STANDARD

      if (inputmod.eq.SEQUENTIEL) then
         ier = fstouv(lnkdiun(1), 'SEQ')
      else
      ntotal_keys = 0
      do i=1,niun
         ier = fstouv(lnkdiun(i), 'RND+R/O+OLD')
         ntotal_keys = ntotal_keys + fstnbr(lnkdiun(i))
         if (ier .lt. 0) then
            write(app_msg,*) 'sorti: File ,lfn(i), is not standard random'
            call app_log(APP_ERROR,app_msg)
            call pgsmabt
         endif
      enddo

      call fstlnk(lnkdiun,niun)
      endif

      if (voire) then
         if (message) then
            do i=1,niun
               ier = fstvoi(lnkdiun(i), 'RND')
            enddo
         endif
      endif
!
      return
      end

      subroutine pgsm_get_nfstkeys(nkeys)
      implicit none
#include "lnkflds.cdk90"
      integer nkeys

      nkeys = ntotal_keys

      return
      end
