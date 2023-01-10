!**   S/P ITROUVE VERIFIER DANS LA LISTE(NOMBRE) SI IVARIA EXISTE
!     
      integer function itrouve(liste,nombre,ivaria)
  
!AUTEUR P. SARRAZIN RPN DORVAL FEV 81
!
!LANGAGE RATFOR
!
!OBJET(ITROUVE)
!         VERIFIER SI IVARIA EXISTE DANS LISTE SI OUI ITROUVE >0
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!ARGUMENTS
!  IN     LISTE =TABLE DE DONNEES
!  IN     NOMBRE=NOMBRE DE DONNEES DANS LISTE
!  IN     IVARIA=ITEM A VERIFIER DANS LISTE
!
! --------------------------------------------------------------------
!
  
   implicit none
!
!
      integer liste(1),nombre,ivaria,ntr
      
      itrouve=0  
!  #  defense contre index de zero 
      if (nombre.le.0)  return  
      
      do ntr=1,nombre
         if (ivaria.eq.liste(ntr)) itrouve=ntr
      enddo
      
      return 
      end
      
