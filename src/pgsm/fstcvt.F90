!.S FSTCVT FONCTION                                 .
!**   FONCTION FSTCVT  HOLLERITH A CARACTERE OU L'INVERSE
!     
      integer FUNCTION FSTCVT2( NOM, TYP, ETIK, GRTP, CNOM, CTYP,      CETIK, CGRTP, HOLACAR)
   implicit none
      
      INTEGER NOM, TYP, ETIK(3), GRTP
      CHARACTER *(*) CNOM
      CHARACTER *(*) CTYP, CGRTP
      CHARACTER *(*) CETIK
      LOGICAL HOLACAR

      integer carmot
!     
!     AUTEUR  P. SARRAZIN   NOV  1989
!     
!     REVISION   000 NOUVELLE VERSION NOVEMBRE 1989
!     
!     LANGAGE RATFOR
!     
!     OBJET(FSTCVT)
!     VARIABLE UTILISEE COMME HOLLERITH SERA TRANSFORME EN CARACTERE
!     OU L'INVERSE POUR NOMVAR,TYPVAR,GRID TYPE, ET ETIKET LE MOT
!     ETIKET AURA 2 LOCATIONS SUR LE SUN
!     
!     ARGUMENTS
!     IN OUT    NOM       HOLLERITH *2                       [NOMVAR]
!     IN OUT    TYP       HOLLERITH *1                       [TYPVAR]
!     IN OUT    ETIK      HOLLERITH *8   2 MOTS A4 POUR SUN  [ETIKET]
!     IN OUT    GRTP      HOLLERITH *1                       [GRTYP]
!     OUT IN    CNOM      CARACTERE *2
!     OUT IN    CTYP      CARACTERE *1
!     OUT IN    CETIK     CARACTERE *8
!     OUT IN    CGRTP     CARACTRE *1
!     IN        HOLACAR   LOGICAL .TRUE.  HOLLERITH A CARATERE
!     LOGICAL .FALSE. CARACTERE A HOLLERITH
!     
!     IMPLICITES
!     
!     
!     
      INTEGER I
      
      carmot = 4
!     
      FSTCVT2 = 0
      IF (HOLACAR) then
!     
!     TRANSFER STRING D'UNE LOCATION HOLLERITH EN CARACTERE
!     
         IF(NOM .eq. -1) then
            CNOM = '     '
         ELSE
            WRITE(CNOM,'(A4)') NOM
         endif
         
         
         IF(TYP .eq. -1) then
            CTYP = '  '
         ELSE
            WRITE(CTYP,'(A2)') TYP
         endif
         
         IF(GRTP .eq. -1) then 
            CGRTP = ' '
         ELSE
            WRITE(CGRTP,'(A1)') GRTP
         endif
         
         
         CETIK = '            '
         IF(ETIK(1) .eq. -1) then 
            CETIK = '            '
         ELSE
            WRITE(CETIK,'(3a4)')(ETIK(I),I=1,12/CARMOT)
         endif
      ELSE
!     
!     TRANSFER STRING D'UNE LOCATION CARACTERE EN HOLLERITH
!     
         READ(CNOM,'(A4)') NOM
         IF(CNOM .eq. '    ') then
            NOM = -1
         endif
!     
         READ(CTYP,'(A2)') TYP
         IF(CTYP .eq. '  ') then
            TYP = -1
         endif
!     
         READ(CGRTP,'(A1)') GRTP
         IF(CGRTP .eq. ' ') then
            GRTP = -1
         endif
!     
         READ(CETIK,'(3a4)') (ETIK(I),I=1,12/CARMOT)
         IF(CETIK .eq. '            ')   then
            ETIK(1) = -1
         endif
      endif
      
      RETURN
      END
      
