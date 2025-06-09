!** S/P DESIRE - EXTRACTION DES ARGUMENTS D'UNE DIRECTIVE "DESIRE"

module desire_exclure_common
   use app
   use configuration
   use iso_c_binding
   implicit none
   include 'rmn/excdes.inc'
contains

   SUBROUTINE desire_exclure(TC, NV, LBL, DATE, IP1, IP2, IP3, request_type)
      !     AUTEUR YVON R. BOURASSA JAN 86
      use rmn_fst98, only: fstcvt_to_char
      IMPLICIT NONE

      integer, dimension(NML), intent(in) :: TC   !< 1 A NML TYPES DE CHAMPS ( 1 CARACTERE )
      integer, dimension(NML), intent(in) :: NV   !< 1 A NML NOMS DE VARIABLES ( 1 @A2 CARACTERES )
      integer, dimension(30),  intent(in) :: LBL  !< 1 A 10 ETIQUETTES ( 1 A 12 CARACTERES ) (4 CARACTERES/ENTIER)
      integer, dimension(NML), intent(in) :: DATE !< 1 A NML DATES OU INTERVALLE AVEC SAUT
      integer, dimension(NML), intent(in) :: IP1  !< 1 A NML IP1 OU INTERVALLE AVEC SAUT
      integer, dimension(NML), intent(in) :: IP2  !< 1 A NML IP2 OU INTERVALLE AVEC SAUT
      integer, dimension(NML), intent(in) :: IP3  !< 1 A NML IP3 OU INTERVALLE AVEC SAUT
      integer, intent(in) :: request_type !< ID of the request type (desire/exclure) (found in excdes.inc)

      EXTERNAL ARGDIMS, ARGDOPE, JULHR, HOLACAR

      INTEGER  ARGDIMS, ARGDOPE, I, J, LIS(10)
      ! DATA     LIS/10*0/
      integer  newip1(NML), newip2(NML), newip3(NML), nip1, nip2, nip3
      integer :: status
      integer :: lima(7)
      character(len=6) :: limc(7)

      limc = ["TYPVAR","NOMVAR","ETIKET","DATE  ","IP1   ","IP2   ","IP3   "]
      max_requetes_exdes =  min(NMD,max_requetes_exdes)
   10 IF(NREQ .EQ. max_requetes_exdes) THEN
         write(app_msg,*) 'desire: Reached maximum ',max_requetes_exdes,' requests'
         call app_log(APP_ERROR,'app_msg')
         RETURN
      ENDIF
      lima = [NML, NML, 10, NML, NML, NML, NML]
      do i = 1, 7
        lima(i) = min(lima(i),max_nlist_exdes)
      enddo
      do i = 1, 7
        if (argdims(i) .gt. lima(i)) then
           write(app_msg,*) 'desire: Reached maximum ',lima(i),' elements for selection argument ',limc(i),', use more DESIRE/EXCLURE directives'
           call app_log(APP_ERROR,app_msg)
        endif
      enddo

      ! COMPTER LES DIRECTIVES DESIRE/EXCLURE
      IF(request_type == EXCDES_EXCLURE) NEXC = NEXC + 1   ! compteur pour "exclure"
      NREQ = NREQ+1                  ! nombre de requetes

      ! INDICATEUR QUE LA REQUETE NREQ N'EST PAS SATISFAITE
      SATISF(NREQ) = 0
      ! INDICATEUR QUE LA REQUETE NREQ EST DESIRE/EXCLURE
      DESEXC(NREQ) = request_type

      DO 20 J=1,4              ! mise a zero de la requete NREQ dans la table de requetes
      DO 20 I=1,11
   20    REQ(I,J,NREQ) = 0
      REQE(NREQ) = 0
      REQN(NREQ) = 0
      REQT(NREQ) = 0

      write(app_msg,*) 'desire: REQUETE # ',NREQ
      call app_log(APP_DEBUG,app_msg)
      GO TO(110,90,70,60,50,40,30) NP    ! type, nom, etiket, date, ip1, ip2, ip3
   30 IF(IP3(1) .NE. -1) THEN         ! traiter IP3
         call ip_to_newip(ip3,newip3,min(lima(7),argdims(7)),nip3)  ! transformer les paires p/kind en ip
         status = Select_ip3(nreq, request_type, newip3, nip3)   ! selecteur ip1 fstd98
         write(app_msg,*) 'desire: IP3 =',(REQ(I,3,NREQ),I=1,11)
         call app_log(APP_DEBUG,app_msg)
      ENDIF
   40 IF(IP2(1) .NE. -1) THEN         ! traiter IP2
         call ip_to_newip(ip2,newip2,min(lima(6),argdims(6)),nip2)  ! transformer les paires p/kind en ip
         status = Select_ip2(nreq, request_type, newip2, nip2)   ! selecteur ip2 fstd98
         write(app_msg,*) 'desire: IP2 =',(REQ(I,2,NREQ),I=1,11)
         call app_log(APP_DEBUG,app_msg)
      ENDIF
   50 IF(IP1(1) .NE. -1) THEN         ! traiter IP1
         call ip_to_newip(ip1,newip1,min(lima(5),argdims(5)),nip1)  ! transformer les paires p/kind en ip
         status = Select_ip1(nreq, request_type, newip1, nip1)   ! selecteur ip3 fstd98
         write(app_msg,*) 'desire: IP1 =',(REQ(I,1,NREQ),I=1,11)
         call app_log(APP_DEBUG,app_msg)
      ENDIF
   60 IF(DATE(1) .NE. -1) THEN         ! traiter DATE, on s'occupe de 'COMMUNE' ici
         IF(date(1)==-4) then   ! COMMUNE, on simule date1 @ date2 DELTA nheures
!           IF(jours(4) == 0)  OUCH !! periode pas initialisee
           IF(jours(4) == 1) THEN ! juste une date1
             status = Select_date(nreq,request_type,(/jours(1)/),1)
           ELSE
             IF(jours(3) == 0) THEN  ! pas de delta
               status = Select_date(nreq,request_type,(/jours(1),-2,jours(2)/),3)
             ELSE                    ! intervalle et delta
               status = Select_date(nreq,request_type,(/jours(1),-2,jours(2),-3,jours(3)/),5)
             ENDIF
           ENDIF
         else  ! directive date normale, sans COMMUNE
           status = Select_date(nreq,request_type,date,min(lima(4),ARGDIMS(4)))
           write(app_msg,*) 'desire: calling Select_date with',date(1:ARGDIMS(4))
           call app_log(APP_DEBUG,app_msg)
         endif
         write(app_msg,*) 'desire: DAT =',(REQ(I,4,NREQ),I=1,11)
         call app_log(APP_DEBUG,app_msg)
      ENDIF
   70 IF(LBL(1) .NE. -1) THEN         ! traiter ETIKET
         LIS = 0
         REQE(NREQ) = ARGDOPE(3, LIS, 10)  ! nombre de strings + table de localisation de readlx
         CALL HOLACAR(ETIS(1,NREQ), LIS, REQE(NREQ), LBL, 12)
         write(app_msg,*) 'desire: ETIKET = ',(ETIS(J,NREQ),J=1,REQE(NREQ))
         call app_log(APP_DEBUG,app_msg)
         status = Select_etiquette(nreq,request_type,etis(1,nreq),REQE(NREQ),12)
      ENDIF
   90 IF(NV(1) .NE.-1) THEN         ! traiter NOMVAR
         REQN(NREQ) = min(lima(2),ARGDIMS(2))
         DO 100 J=1, ARGDIMS(2)
            I = fstcvt_to_char(NV(J), -1, [-1, -1, -1], -1, NOMS(J,NREQ), TYP, ETI, GTY)
  100       CONTINUE
         write(app_msg,*) 'desire: NOMVAR = ',(NOMS(J,NREQ),J=1,ARGDIMS(2))
         call app_log(APP_DEBUG,app_msg)
         status = Select_nomvar(nreq,request_type,noms(1,nreq),REQN(NREQ),4)
      ELSE
        REQN(NREQ) = 1
        noms(1,nreq) = '    '
        status = Select_nomvar(nreq,request_type,noms(1,nreq),REQN(NREQ),4)
      ENDIF
  110 IF(TC(1) .NE. -1) THEN         ! traiter TYPVAR
         REQT(NREQ) = min(lima(1),ARGDIMS(1))
         DO 120 J=1, ARGDIMS(1)
            I = fstcvt_to_char(-1, TC(J), [-1, -1, -1], -1, NOM, TYPS(J,NREQ), ETI, GTY)
  120       CONTINUE
         write(app_msg,*) 'desire: TYPVAR = ',(TYPS(J,NREQ),J=1,ARGDIMS(1))
         call app_log(APP_DEBUG,app_msg)
         status = Select_typvar(nreq,request_type,typs(1,nreq),REQT(NREQ),2)
      ENDIF

      ! AJOUTER LES CRITERES SUPPLEMENTAIRES AU BESOIN
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
         status =  Select_suppl(nreq,request_type,nis,njs,nks,ig1s,ig2s,ig3s,ig4s,gtyps)
      ENDIF
      IF( DEBUG )  call Dump_Request_table()
      RETURN

   contains

      ! ** S/P ip_to_newip - conversion des paires valeur/kind en codes ip
      ! transformer la liste ip pouvant contenir des paires valeur/kind
      ! en liste de ip entiers
      ! des valeurs negatives dans ip comme -1,-2,-3 (@,delta,tous) restent inchangees
      subroutine ip_to_newip(ip,newip,nip,nnewip)
      use ISO_C_BINDING
      implicit none
      include 'rmn/convert_ip123.inc'
      integer, intent(IN) :: nip
      integer, intent(OUT) :: nnewip
      integer, intent(IN), dimension(nip) :: ip
      integer, intent(OUT), dimension(*) :: newip

      ! AUTEUR M. Valin - fev 2014 (d'apres ancien sous-programme ip1_to_newip1)
      ! ARGUMENTS
      ! Entree  ip     -  liste de niveaux (ip [entier] ou paire [reel,code_de_type entier])
      ! "     nip    -  dimension de ip
      ! Sortie  newip  -  liste des niveaux encodes (ip entiers)
      ! "     nnewip -  nombre de valeurs dans newip

      ! *
      integer :: i, kindp, dummyip
      character(len=12) :: dummy
      real :: p, dummyp
      logical :: initdone
      data initdone /.false./
      save initdone


      dummyip = 0
      dummyp = 0.
      kindp =0
      if (.not. initdone) then
         call convip_plus(dummyip,dummyp,kindp,0,dummy,.false.)   ! initialisation de convip au mode newstyle
         initdone = .true.
      endif

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
               WRITE(app_msg,*) 'desire: DESIRE/EXCLURE conversion error P,kind =',p,kindp
               call app_log(APP_ERROR,app_msg)
               call qqexit(20)
             endif
             i = i + 1  ! sauter kind
          endif ! paire
        endif  ! paire possible
        i = i + 1       ! sauter valeur
      enddo
      return
      end subroutine

      end subroutine

end module desire_exclure_common

subroutine desire(TC, NV, LBL, DATE, IP1, IP2, IP3)
   use desire_exclure_common
   implicit none
   integer, dimension(NML), intent(in) :: TC   !< 1 A NML TYPES DE CHAMPS ( 1 CARACTERE )
   integer, dimension(NML), intent(in) :: NV   !< 1 A NML NOMS DE VARIABLES ( 1 @A2 CARACTERES )
   integer, dimension(30),  intent(in) :: LBL  !< 1 A 10 ETIQUETTES ( 1 A 12 CARACTERES ) (4 CARACTERES/ENTIER)
   integer, dimension(NML), intent(in) :: DATE !< 1 A NML DATES OU INTERVALLE AVEC SAUT
   integer, dimension(NML), intent(in) :: IP1  !< 1 A NML IP1 OU INTERVALLE AVEC SAUT
   integer, dimension(NML), intent(in) :: IP2  !< 1 A NML IP2 OU INTERVALLE AVEC SAUT
   integer, dimension(NML), intent(in) :: IP3  !< 1 A NML IP3 OU INTERVALLE AVEC SAUT
   call desire_exclure(TC, NV, LBL, DATE, IP1, IP2, IP3, EXCDES_DESIRE)
end subroutine desire

subroutine exclure(TC, NV, LBL, DATE, IP1, IP2, IP3)
   use desire_exclure_common
   implicit none
   integer, dimension(NML), intent(in) :: TC   !< 1 A NML TYPES DE CHAMPS ( 1 CARACTERE )
   integer, dimension(NML), intent(in) :: NV   !< 1 A NML NOMS DE VARIABLES ( 1 @A2 CARACTERES )
   integer, dimension(30),  intent(in) :: LBL  !< 1 A 10 ETIQUETTES ( 1 A 12 CARACTERES ) (4 CARACTERES/ENTIER)
   integer, dimension(NML), intent(in) :: DATE !< 1 A NML DATES OU INTERVALLE AVEC SAUT
   integer, dimension(NML), intent(in) :: IP1  !< 1 A NML IP1 OU INTERVALLE AVEC SAUT
   integer, dimension(NML), intent(in) :: IP2  !< 1 A NML IP2 OU INTERVALLE AVEC SAUT
   integer, dimension(NML), intent(in) :: IP3  !< 1 A NML IP3 OU INTERVALLE AVEC SAUT
   call desire_exclure(TC, NV, LBL, DATE, IP1, IP2, IP3, EXCDES_EXCLURE)
end subroutine exclure
