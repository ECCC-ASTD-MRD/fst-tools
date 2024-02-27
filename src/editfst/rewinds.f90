!** S/R REWINDS
!     REWIND LE FICHIER SOURCE AU DEBUT 
      SUBROUTINE REWINDS( DSN, TIPE )
      use configuration
      use app
      IMPLICIT NONE 
      INTEGER  DSN(*), TIPE(*)
  
!ARGUMENTS
!  ENTRE    - DSN   - DATASET NAME DU FICHIER A REBOBINER.
!    "      - TIPE  - TYPE DU FICHIER.
!
!AUTEURS
!VERSION ORIGINALE Y. BOURASSA OCT 90
!REVISION 001      "      "    VERSION UNIX
!         002      "      "    MAR 92 ALLEL A LOW2UP POUR TYPE
!         003      "      "    MAI 92 ABORT SI FICHIER INEXISTANT
!         004      "      "     "  "  SKIP ABORT SI EN INTERACTIF 
!         005      M. Lepine   Nov 05 Remplacement de fstabt par qqexit
!         006       M. Valin    Mai 14 Remplacement des comdecks par un module
!
!LANGUAGE   - FTN77 
!
!MODULES
      EXTERNAL      ARGDIMS, OUVRES, LOW2UP, qqexit

      INTEGER       ARGDIMS, I
      CHARACTER*15  T
      CHARACTER*128 DN
  
!     DECODE LE TYPE DE FICHIER (DOIT ETRE SEQ.)
      IF(NP .EQ. 2) THEN
         WRITE(T, LIN128) (TIPE(I), I=1,ARGDIMS(2))
         CALL LOW2UP(T, T)
      ELSE
         T = 'SEQ'
      ENDIF
      IF(INDEX(T,'RND') .EQ. 0) THEN
         SSEQ = .TRUE.
         IF(INDEX(T,'FTN') .GT. 0) THEN
            SNOM = 'STD+SEQ+FTN'
         ELSE
            SNOM = 'STD+SEQ'
         ENDIF
      ELSE
         call app_log(APP_WARNING,'rewinds: No rewind possible')
         RETURN
      ENDIF

!     DECODE LE NOM DU FICHIER 
      IF(DSN(1) .NE. -1) THEN
         WRITE(DN, LIN128) (DSN(I), I=1,ARGDIMS(1))
         CALL OUVRES( DN )
      ENDIF
      IF( OUVS ) THEN
         I = sources(1)%rewind()
         call app_log(APP_DEBUG,'rewinds: No rewind possible')
      ELSE
         IF( INTERAC ) THEN
            WRITE(app_msg,*) 'rewinds: File ',DN,' not opened'
            call app_log(APP_WARNING,app_msg)
            RETURN
         ELSE
            CALL qqexit(60)
         ENDIF
      ENDIF

      RETURN
      END 



