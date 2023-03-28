!**s/p pgsmabt  sortie pas trop brutale en cas d'erreur
!
   subroutine pgsmabt
      use app
      implicit none
      
      external abort,fclos,fstfrm,messags,exfin
      integer exfin,fstfrm
!
!auteur   p. sarrazin  rpn mars 1983
!
!langage  ratfor
!
!objet(pgsmabt)
!         faire une sortie sans dommage pour les fichiers
!         ouverts en cas d'erreur fatale dans pgsm
!
#include "indptr.cdk90"
#include "lnkflds.cdk90"
!
      integer ier
      app_status=app_end(13)
!
      ier = fstfrm(lnkdiun(1))
      if (mode.eq.1)ier = fstfrm(lnkdiun(idx_ozsrt))
#if defined (SGI) || defined (NEC)
      if (mode.eq.2) then
         call app_log(APP_ERROR,'pgsmabt: "MS" files are not supported in the version of PGSM')
      endif
#endif
      call qqexit(13)

      end

