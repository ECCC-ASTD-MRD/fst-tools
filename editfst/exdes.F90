!/* EDITFST - Collection of useful routines in C and FORTRAN
! * Copyright (C) 1975-2014  Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
!** S/R EXDES DECODE LA DATE, LES IP1, IP2 ET IP3 DE LA CARTE
!             DESIRE/EXCLURE
!
      SUBROUTINE EXDES(CLE, N, M)  ! appele uniquement pour IP1/2/3 et DATE
      use configuration
      IMPLICIT NONE 
  
      INTEGER    N, M, CLE(N)
  
!ARGUMENTS
! ENTRE   CLE  -  ARGUMENTS DE L'APPEL A DESIRE (IP1,IP2,IP3,DATE)
!   "     N    -  DIMENSION DE CLE
!   "     M    -  PARAMETRE A DECODER 1=IP1 2=IP2 3=IP3 4=DATE
  
!AUTEURs
!VERSION ORIGINALE  - Y. BOURASSA SEP 90
!REVISION 001         "     "     OCT 90 VERSION QLXINS
!         002         "     "     MAR 92 BUG DANS REQUETE AVEC SAUT
!                                        MAX MIN DIFF. POUR DATE IP1-2-3
!         003         "     "     AVR 92 TEST SUR DATE JULIENNE
!         004         "     "            ALLOUE RANGE INVERSE 
!         005         "     "     MAI 95 ENLEVVE REFERENCES A 1950
!         006         M. Lepine   Mar 98 Traitement des dates > 2000
!         007         M. Lepine   Sep 98 Bug fix stamp limite superieure
!         008         M. Lepine - Mai 02 Code de traitement des IP1 en valeurs reelle
!         009         M. Lepine - Nov 05 Remplacement de fstabt par qqexit
!         010         M. Valin  - Fev 14 nouveau traitement IP1/2/3, simplification, detection d'erreurs
!     LANGUAGE FTN77
!
!MODULES
      EXTERNAL JULSEC, qqexit
!
!*
!#include "maxprms.cdk"
!#include "desrs.cdk"
!      NREQ vient de desrs.cdk
!#include "fiches.cdk"
  
!      INTEGER  I, J, BOT(4), TOP(4), temp, deltas, kind, p_int
      INTEGER  I, J, BOT(4), TOP(4), temp, deltas
!      integer *8 J8, CLE8(10), kind_8, p_int8, ior
      integer *8 J8
!      intrinsic ior
!      Real    p
!      equivalence (p_int,p)
      CHARACTER *128 string
      EXTERNAL convip
      SAVE     BOT, TOP
!     LIMITE INFERIEURE = 0 POUR IP1-2-3 & JAN 01 1900 00Z POUR DATE
      DATA     BOT/0,0,0,010100000/
!     LIMITE SUPERIEURE POUR IP1-2-3     & JAN 01 2219 00Z POUR DATE
      DATA     TOP/Z'FFFFFFF', Z'FFFFFFF', Z'FFFFFFF', 2008728800/

!      p_int8 = Z'80000000'
!      kind_8 = 31
!      top(1) = ior (p_int8, ishft(kind_8,32))   ! valeur maximale pour IP1 encode

      IF (M .EQ. 4) THEN
         DELTAS = 3600   ! Facteur multiplicatif pour passage en secondes
      ELSE
         DELTAS = 1
      ENDIF
!
#if defined(NEVER_TRUE)
      DO i = 1,N    !  tout ce code sera supprime
         IF ((M .eq. 1) .and. (cle(i) .ne. -2) .and. (cle(i) .ne. -3)) then   ! -2 '@' , -3 'DELTA'
             CALL convip(cle(i),p,kind,-1,string,.false.)  ! convertir IP1 en Kind et Level dans un integer*8
!             print *,'Debug+ i=',i,' cle(i)=',cle(i),' p=',p
             p_int8 = Z'80000000'
             if (p .lt. 0) then
                p = abs(p)
                p_int8 = p_int8 - p_int
             else
                p_int8 = p_int8 + P_int
             endif
             kind_8 = kind
             CLE8(i) = ior (p_int8, ishft(kind_8,32))
!      write(*,777) cle(i),kind,p,cle8(i)
 777  format('Debug+ cle(i)=',i10,' kind=',i2,' p =',e10.4,' cle8(i) =',z16.16)
         ELSE
            CLE8(i) = CLE(i)
         ENDIF
      ENDDO
#endif
!     FAUT-IL PRENDRE LA DATE DE LA PERIODE?
      IF(M.EQ.4 .AND. CLE(1).EQ.-4) THEN
         IF(JOURS(4) .EQ. 0) THEN
            WRITE(6,*)' PAS RENCONTRE DE DIRECTIVE PERIODE' 
            call qqexit(20)
         ELSE
            REQ(11,M,NREQ) = JOURS(4)
            DO 10 I=1,3
               REQ(I,M,NREQ) = JOURS(I) 
   10          CONTINUE
            RETURN
         ENDIF
      ENDIF
  
      IF(N.EQ.1 .OR. ((CLE(1).NE.-2).AND.(CLE(2).NE.-2))) THEN
!        REQUETE = LISTE DE NOMBRES POSITIFS, pas de @ en position 1 ou 2
         REQ(11,M,NREQ) = N    ! nombre de valeurs dans la liste
         DO 20 J=1,N
            if(CLE(J)<0) then
              WRITE(6,*)'*** Editfst *** ERREUR:  IP1/IP2/IP3/DATE < 0 '
              call qqexit(20)
            endif
            REQ(J,M,NREQ) = CLE(J)
   20    CONTINUE
!         print *,N,' requests copied, m=',m
      ELSE
         REQ( 1,M,NREQ) = BOT(M)                       ! minimum par defaut
         REQ( 2,M,NREQ) = TOP(M)                       ! maximum par defaut
!         if (M .eq. 1) REQ(2,M,NREQ) = ishft(32,32)   ! plus gros entier selon Kind du IP1
         REQ( 3,M,NREQ) = 1                            ! valeur delta (dates seulement)
         REQ(11,M,NREQ) = -1                           ! indiquer que ce n'est pas une liste de valeurs
         IF(CLE(1) .EQ. -2 ) THEN  ! [@, ....... ]
!           REQUETE DE TYPE [@,-----]
            REQ(2,M,NREQ) = CLE(2)                    ! valeur max explicite
            IF(N .EQ. 3) THEN
!              REQUETE DE TYPE [@,-----,-- ]
               REQ(3,M,NREQ) = CLE(3)                 ! recuperer delta
            ELSEIF(N .EQ. 4) THEN
!              REQUETE DE TYPE [@,-----,DELTA,--]
               REQ(3,M,NREQ) = CLE(4) * DELTAS        ! recuperer delta et appliquer facteur
            ENDIF
         ELSE
!           REQUETE DE TYPE [-----,@]
            REQ(1,M,NREQ) = CLE(1)                    ! valeur min explicite
            IF(N.GT.2 .AND. CLE(3).NE.-3) THEN
!              CHARGE REQUETE DE TYPE [-----,@,-----]
               REQ(2,M,NREQ) = CLE(3)
            ENDIF
            IF(N .GT. 3) THEN
               IF(CLE(3) .EQ. -3) THEN
!                 REQUETE DE TYPE [-----,@,DELTA,--]
                  REQ(3,M,NREQ) = CLE(4) * DELTAS      ! recuperer delta et appliquer facteur
               ELSEIF(N .EQ. 4) THEN
!                 REQUETE DE TYPE [-----,@,-----,--]
                  REQ(3,M,NREQ) = CLE(4)               ! recuperer delta
               ELSE
!                 REQUETE DE TYPE [-----,@,-----,DELTA,--]
                  REQ(3,M,NREQ) = CLE(5) * DELTAS   ! pour mettre le tout en secondes
               ENDIF
            ENDIF
         ENDIF  
         if ((m .ne. 4) .and. (req(3,M,NREQ) .ne. 1)) then
            WRITE(6,*)'*** Editfst *** ERREUR: Delta different de 1 non valide pour IP1/IP2/IP3'
            call qqexit(20)
         endif
      ENDIF
  
      IF(M .EQ. 4) THEN
         I = REQ(11,M,NREQ)
         IF(I .EQ. -1) I=2
         DO 40 J=1,I
            temp = REQ(J,M,NREQ)
            CALL JULSEC(REQ(J,M,NREQ), temp)
   40       CONTINUE
      ENDIF

      IF(REQ(11,M,NREQ) .EQ. -1) THEN
         J8 = MAX(REQ(1,M,NREQ), REQ(2,M,NREQ))
         REQ(1,M,NREQ) = MIN(REQ(1,M,NREQ), REQ(2,M,NREQ))
         REQ(2,M,NREQ) = J8
      ENDIF
  
      RETURN
      END 
