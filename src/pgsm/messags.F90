!
!**S/P MESSAGS  IMPRIME MESSAGE SUR UNE PAGE COMPLETE
   subroutine messags(ni) 
      use app     
      implicit none
!
!LANGAGE RATFOR
!
!OBJET(MESSAGS)
!          IMPRIME UN MESSAGE A CHAQUE FOIS UNE GRILLE GAUSSIENNE
!          N'A PAS UN NOMBRE DE LONGITUDE PAIRES
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!ARGUMENTS
!  IN     NI  - NOMBRE DE LONGITUDES DANS LE RECORD DU FICHIER D'ENTRE
!     
      integer ni

      write(app_msg,*) 'messags: The number of longitude has to be even for a gaussian grid, #LONG=',ni
      call app_log(APP_WARNING,app_msg)
!     
      return
      end
      
