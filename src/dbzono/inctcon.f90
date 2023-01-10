!S/R INCTCON  -  INITIALISE LE COMMON 'CTESCON'.
! 
      SUBROUTINE INCTCON
! 
      use app
      IMPLICIT NONE 
! 
!Author
!          N. Brunet  (Jan91)
!
!Revision 001 G.Pellerin adapation au debaleur
!         002 G.Pellerin (Avril 2000) Compilation en FORTRAN90
!
!Object 
!          to initialize the variables in common block CTESCON by 
!          reading the file "CONSTANTES".
!
!Arguments
!          None.
!
!PARAMETRES 
!     NBRE - NOMBRE DE CONSTANTES DANS LE FICHIER 
      INTEGER NBRE
      PARAMETER(NBRE=31)
! 
!IMPLICITES 
#include "ctescon.cdk"
! 
!MODULES
      EXTERNAL CONSTNT
! 
!---------------------------------------------------------------------- 
      INTEGER FLAG, I 
      REAL TEMP1(NBRE)
! 
      EQUIVALENCE (TEMP1(1),CPD)
! 
!   DATA INIT/.FALSE./
! 
      CHARACTER *10 NOM1(NBRE)
! 
      DATA NOM1/ 'CPD', 'CPV', 'RGASD', 'RGASV', 'TRPL',           &
                'TCDK', 'RAUW', 'EPS1', 'EPS2', 'DELTA',           &
                'CAPPA', 'TGL', 'CONSOL', 'GRAV', 'RAYT',          &
                'STEFAN', 'PI', 'OMEGA',                           &
                'KNAMS', 'STLO', 'KARMAN', 'RIC', 'CHLC', 'CHLF',  &
                'T1S', 'T2S', 'AW', 'BW', 'AI', 'BI', 'SLP'/ 
! 
!---------------------------------------------------------------------- 
! 
      INIT=.FALSE.
      DO 10 I=1,NBRE
         CALL CONSTNT(TEMP1(I),FLAG,NOM1(I),0)
         IF(FLAG.EQ.0)THEN
            WRITE(app_msg,600) NOM1(I) 
            call app_log(APP_ERROR,app_msg)
600         FORMAT('LA CONSTANTE',2X,A10,1X,'N EXISTE PAS')
            app_status=app_end(-1)
            error stop 'CTE ABS.' 
         ELSE 
!           WRITE(6,610)NOM1(I),TEMP1(I)
610         FORMAT(1X,'VALEUR DE',1X,A10,2X,'=',1X,E15.7) 
         END IF 
10    CONTINUE

      INIT=.TRUE. 

      RETURN
      END 
