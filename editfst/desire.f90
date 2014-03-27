!** S/P DESIRE - EXTRACTION DES ARGUMENTS D'UNE DIRECTIVE "DESIRE"

      SUBROUTINE DESIRE(TC, NV, LBL, DATE, IP1, IP2, IP3)
      use configuration
      IMPLICIT NONE 
  
      INTEGER, intent(IN) ::  DATE(10), IP1(20), IP2(10), IP3(10), TC(10), NV(10), LBL(20)
!     AUTEUR YVON R. BOURASSA JAN 86
!              "  "      "    OCT 90 VERSION QLXINS
!              "  "      "    FEV 91 BUG DECODING ETIKET
!Revision 003   M. Lepine - mars 98 - extensions pour fstd98
!Revision 004   M. Lepine - juil 01 - possibilite d'appel a convip
!Revision 004   M. Lepine - fev  02 - verification du maximum de 10 elements
!Revision 005   M. Valin  - fev  14 - nouveau traitement IP1/2/3, intent
!     LANGUAGE FTN90
!  
!ARGUMENTS
! ENTRE   TC   -  1 A 10 TYPES DE CHAMPS ( 1 CARACTERE )
!   "     NV   -  1 A 10 NOMS DE VARIABLES ( 1 @A2 CARACTERES )
!   "     LBL  -  1 A 10 ETIQUETTES ( 1 A 12 CARACTERES )
!   "     DATE -  1 A 10 DATES OU INTERVALLE AVEC SAUT
!   "     IP1  -  1 A 10 IP1    "      "      "    "
!   "     IP2  -  1 A 10 IP2    "      "      "    "
!   "     IP3  -  1 A 10 IP3    "      "      "    "
!#include "maxprms.cdk"
!#include "desrs.cdk"
!#include "lin128.cdk"
!#include "logiq.cdk"
!#include "fiches.cdk"
!#include "char.cdk"
!
!MODULES
      EXTERNAL FSTCVT, ARGDIMS, ARGDOPE, IOPDATM, JULHR, EXDES, HOLACAR
      include 'excdes.inc'
!*  
      INTEGER  FSTCVT, ARGDIMS, ARGDOPE, IOPDATM, I, J, D, LIS(10)
!      DATA     LIS/10*0/
      integer  newip1(10), newip2(10), newip3(10), nip1, nip2, nip3
      integer :: status
      integer excdes_de

      D = -1   ! desire si D==-1,  exclure si D==0 (voir entry plus bas)
      excdes_de = EXCDES_DESIRE
   10 IF(NREQ .EQ. NMD) THEN
!         IF(DIAG .OR. DEBUG)
         PRINT*,'** LE MAXIMUM DE',NMD,' REQUETES DEJA ATEINT **' 
         RETURN
      ENDIF
      if ((argdims(1) .gt. 10) .or. (argdims(2) .gt. 10) .or.   &
          (argdims(3) .gt. 10) .or. (argdims(4) .gt. 10) .or.   &
          (argdims(5) .gt. 10) .or. (argdims(6) .gt. 10) .or.   &
          (argdims(7) .gt. 10)) then
         PRINT*, '** MAXIMUM DE 10 ELEMENTS POUR UN ARGUMENT DE SELECTION **' 
         PRINT *,'** SEULS LES 10 PREMIERS ELEMENTS SERONT TRAITES        **'
         PRINT *,'** UTILISER UNE DIRECTIVE DESIRE/EXCLURE SUPPLEMENTAIRE **'
      endif
  
!     COMPTER LES DIRECTIVES DESIRE/EXCLURE
      IF(D .EQ. 0) NEXC = NEXC + 1   ! compteur pour "exclure"
      NREQ = NREQ+1                  ! nombre de requetes
  
!     INDICATEUR QUE LA REQUETE NREQ N'EST PAS SATISFAITE
      SATISF(NREQ) = 0
!     INDICATEUR QUE LA REQUETE NREQ EST DESIRE/EXCLURE
      DESEXC(NREQ) = D
  
      DO 20 J=1,4
      DO 20 I=1,11
   20    REQ(I,J,NREQ) = 0
      REQE(NREQ) = 0
      REQN(NREQ) = 0
      REQT(NREQ) = 0
  
      IF( DEBUG ) PRINT*,'REQUETE # ',NREQ
      GO TO(110,90,70,60,50,40,30) NP    ! type, nom, etiket, date, ip1, ip2, ip3
   30 IF(IP3(1) .NE. -1) THEN         ! traiter IP3
!         if (argdims(7) .gt. 1) then
           call ip_to_newip(ip3,newip3,argdims(7),nip3)  ! transformer les paires p/kind en ip
           CALL EXDES(newip3,  nip3, 3)
           status = Select_ip3(nreq, excdes_de, newip3, nip3)
!         else
!           CALL EXDES(IP3,  ARGDIMS(7), 3)
!         endif
         IF( DEBUG ) PRINT*,'IP3 =',(REQ(I,3,NREQ),I=1,11)
      ENDIF
   40 IF(IP2(1) .NE. -1) THEN         ! traiter IP2
!         if (argdims(6) .gt. 1) then
           call ip_to_newip(ip2,newip2,argdims(6),nip2)  ! transformer les paires p/kind en ip
           CALL EXDES(newip2,  nip2, 2)
           status = Select_ip2(nreq, excdes_de, newip2, nip2)
!         else
!           CALL EXDES(IP2,  ARGDIMS(6), 2)
!         endif
         IF( DEBUG ) PRINT*,'IP2 =',(REQ(I,2,NREQ),I=1,11)
      ENDIF
   50 IF(IP1(1) .NE. -1) THEN         ! traiter IP1
!         if (argdims(5) .gt. 1) then
            call ip_to_newip(ip1,newip1,argdims(5),nip1)  ! transformer les paires p/kind en ip
            CALL EXDES(newip1, nip1, 1)
           status = Select_ip1(nreq, excdes_de, newip1, nip1)
!         else
!            CALL EXDES(IP1,  ARGDIMS(5), 1)
!         endif
         IF( DEBUG ) PRINT*,'IP1 =',(REQ(I,1,NREQ),I=1,11)
      ENDIF
   60 IF(DATE(1) .NE. -1) THEN         ! traiter DATE
         CALL EXDES(DATE, ARGDIMS(4), 4)
         status = Select_date(nreq,excdes_de,date,ARGDIMS(4))
!        il va falloir arranger les choses pour mettre delta s'il y en a un en secondes
!        et rendre setper coherent avec tout ca (on oublie les secondes juliennes)
!        si date(1) ==  -4   (COMMUNE)
!         si jours(4) ==  0   OUCH !!
!         si jours(4) ==  1   status = Select_date(nreq,excdes_de,jours(1),1)
!         si jours(4) ==- 1   status = Select_date(nreq,excdes_de,(/ jours(1), "@", jours(2),"delta",jours(3) /), 5)
         IF( DEBUG ) PRINT*,'DAT =',(REQ(I,4,NREQ),I=1,11)
      ENDIF
   70 IF(LBL(1) .NE. -1) THEN         ! traiter ETIKET
         lis = 0
         REQE(NREQ) = ARGDOPE(3, LIS, 10)  ! nombre de strings + table de localisation de readlx
         CALL HOLACAR(ETIS(1,NREQ), LIS, REQE(NREQ), LBL, 12)
         IF( DEBUG ) PRINT*,'ETIKET = ',(ETIS(J,NREQ),J=1,REQE(NREQ))
         status = Select_etiquette(nreq,excdes_de,etis(1,nreq),REQE(NREQ),12)
      ENDIF
   90 IF(NV(1) .NE.-1) THEN         ! traiter NOMVAR
         REQN(NREQ) = ARGDIMS(2)
         DO 100 J=1, ARGDIMS(2)
            I = FSTCVT(NV(J), -1, -1, -1, NOMS(J,NREQ), TYP, ETI, GTY, .TRUE.)
  100       CONTINUE
         IF( DEBUG ) PRINT*,'NOMVAR = ',(NOMS(J,NREQ),J=1,ARGDIMS(2))
         status = Select_nomvar(nreq,excdes_de,noms(1,nreq),REQN(NREQ),4)
      ENDIF
  110 IF(TC(1) .NE. -1) THEN         ! traiter TYPVAR
         REQT(NREQ) = ARGDIMS(1)
         DO 120 J=1, ARGDIMS(1)
            I = FSTCVT(-1, TC(J), -1, -1, NOM, TYPS(J,NREQ), ETI, GTY, .TRUE.)
  120       CONTINUE
         IF( DEBUG ) PRINT*,'TYPVAR = ',(TYPS(J,NREQ),J=1,ARGDIMS(1))
         status = Select_typvar(nreq,excdes_de,typs(1,nreq),REQT(NREQ),2)
      ENDIF
  
!     APPLIQUER LES CRITERES SUPPLEMENTAIRES AU BESOIN
      IF( SCRI ) THEN
         SUP(8,NREQ) = 1
         SUP(1,NREQ) = NIS
         SUP(2,NREQ) = NJS
         SUP(3,NREQ) = NKS
         SUP(4,NREQ) = IG1S
         SUP(5,NREQ) = IG2S
         SUP(6,NREQ) = IG3S
         SUP(7,NREQ) = IG4S
         GTYS(NREQ)  = GTYPS
         status =  Select_suppl(nreq,excdes_de,nis,njs,nks,ig1s,ig2s,ig3s,ig4s,gtyps)
      ENDIF
!      call Dump_Request_table()
      RETURN
  
!     POUR CHOISIR LES CHAMPS NON VOULUS
      ENTRY EXCLURE(TC, NV, LBL, DATE, IP1, IP2, IP3)
      D = 0
      excdes_de = EXCDES_EXCLURE
      GO TO 10
  
      contains
!** S/P ip_to_newip - conversion des paires valeur/kind en codes ip
! transformer la liste ip pouvant contenir des paires valeur/kind
! en liste de ip entiers
! des valeurs negatives dans ip comme -1,-2,-3 (@,delta,tous) restent inchangees
      subroutine ip_to_newip(ip,newip,nip,nnewip)
!     use convert_ip123
      use ISO_C_BINDING
      implicit none
      include 'convert_ip123.inc'
      integer, intent(IN) :: nip
      integer, intent(OUT) :: nnewip
      integer, intent(IN), dimension(nip) :: ip
      integer, intent(OUT), dimension(*) :: newip
!
!AUTEUR M. Valin - fev 2014 (d'apres ancien sous-programme ip1_to_newip1)
!
!LANGUAGE Fortran 90
!  
!ARGUMENTS
! Entree  ip     -  liste de niveaux encodes ou non
!   "     nip    -  dimension de ip
! Sortie  newip  -  liste des niveaux encodes (entiers)
!   "     nnewip -  nombre de valeurs dans newip
!
!
!*  
      integer :: i, kindp
      character(len=12) :: dummy
      real :: p

      nnewip = 0
      i = 1
      do while (i <= nip)
        nnewip = nnewip + 1
        newip(nnewip) = ip(i) ! copier ip -> newip (sous reserve de correction si "p,kind")
        if(i+1 <= nip) then  ! paire possible
          if(ip(i+1)>=-1031 .and. ip(i+1)<=-1000) then ! paire, car ip(i+1) est un code -(1000+kind)
             p = transfer(ip(i),p)   ! transferer les bits de ip  a p (remplace equivalence)
             kindp = -(ip(i+1)+1000) ! symbole pour kind = -(1000+kind) dans directives
             call convip_plus(newip(nnewip),p,kindp,1,dummy,.false.)  ! p,kind -> ip
             if(newip(nnewip)<0) then
               WRITE(6,*)'*** Editfst *** ERREUR:  erreur de conversion  P,kind =',p,kindp
               call qqexit(20)
             endif
             i = i + 1  ! sauter kind
          endif  ! paire possible
        endif
        i = i + 1
      enddo
      return
      end subroutine

      end subroutine
