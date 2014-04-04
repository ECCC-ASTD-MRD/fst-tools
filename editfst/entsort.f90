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
!**S/P ENTSORT   METTRE DES ENTIERS EN ORDRE ASCENDANT
!
      SUBROUTINE ENTSORT(ENTIERS, NBRE) 
  
      IMPLICIT NONE 
  
      INTEGER    NBRE, ENTIERS(NBRE)
!
!AUTEURS  VERSION ORIGINALE  M. VALIN DRPN DORVAL P. Q. CANADA
!         REVISION 001       Y. BOURASSA (RATFOR @AFTN 77) JUL 83
!
!LANGAGE FTN 77
!
!OBJET(ENTSORT)
!            METTRE DES ENTIERS EN ORDRE ASCENDANT
!
!ARGUMENTS
! IN/OUT  - ENTIERS - RETOURNE LES ENTIERS EN ORDRE ASCENDANT
! IN/OUT  - NBRE    - NOMBRE D'ENTIERS RETOURNES
!
!*
      INTEGER X, I, J, K
  
      IF(NBRE .GT. 1) THEN
  
         DO 20 I = 1, NBRE-1
            K = I
            DO 10 J = I+1, NBRE
               IF(ENTIERS(K) .GT. ENTIERS(J))  K=J
   10          CONTINUE
            IF(K .NE. I) THEN 
               X          = ENTIERS(K)
               ENTIERS(K) = ENTIERS(I)
               ENTIERS(I) = X 
            ENDIF
   20       CONTINUE
  
!        ELIMINATION DES NOMBRES EGAUX DE LA LISTE
         I = 1
         DO 30 J=2,NBRE
            IF(ENTIERS(I) .NE. ENTIERS(J)) THEN
               I = I+1
               IF(I .NE. J) ENTIERS(I) = ENTIERS(J)
            ENDIF
   30       CONTINUE
  
         NBRE = I
  
      ENDIF
  
      RETURN
      END 
