***PROGRAMME EDITFST - COPIE UNE PARTIE D'UN FICHIER STANDARD DANS UN 
*                      AUTRE FICHIER DTANDARD.
      PROGRAM EDITFST
*****************************************
*********    VERSION  'UNIX'    *********
*****************************************
      IMPLICIT NONE 
*
*         AUTEURS                                         DATE   VERSION
*         VERSION ORIGINALE (COPYSTD) C. THIBEAULT  -     FEV. 83
*         Y.BOURASSA/ M.VALIN (EDITSTD)                   JUL. 86 1.3 
*         BOURASSA         CORRECTION DANS SEQCOPY        FEV. 87 1.4 
*         VALIN            AJOUT DE FASTIO                FEV. 87 1.5 
*         BOURASSA         DIRECTIVE "CRITSUP"            OCT. 89 1.6 
*            "             DIRECTIVE "EXCLURE"            OCT. 89 1.6 
*            "             ACCEPTE "OPDATE"               JAN. 90 1.7 
*            "             NEWEDIT VERSION UNIX/CRAY      JUL. 90 1.8 
*            "             VERSION EDITFST (CRAY/UNIX)    NOV. 90 2.0 
*            "             QLXINS, FICHIERS FST..ETC. 
*            "             CORRECTION DANS COPISEQ        DEC. 90 2.1 
*            "             OPTION IMAGE                   JAN. 91 2.2 
*            "             ARGDOPE DES ETIKETS            MAR. 91 2.3 
*            "             LEGERE MODIF DANS PRINT        AVR. 91 2.31
*            "                "     "   SNOM-DNOM         MAI. 91 2.4  
*            "             DIRECTIVE ZAP                  JUL. 91 2.5
*            "             LINQUAGE DES FICHIERS SOURCES  OCT. 91 2.6
*            "             ACCELERATION DU TRIAGE         NOV. 91 2.7
*            "             QLXINX POUR NEC, OTE FRETOUR,
*                          CHANHE APPEL A CCARD & HOLACAR JAN. 92 2.8
*            "             FIX BUG 1e  ALLOCATION MEMOIRE JAN. 92 2.9
*            "             AJOUTE +OLD AU FICHIER SOURCE  MAR. 92 3.0
*                          ACCES A $IN VERSION CRAY       
*                          APPEL A LOW2UP APRES CCARD
*                          OUVRE SOURCES AVEC FSTNOL 
*            "             PERMET FICHIERS FTN SUR UNIX
*            "             NEUTRALISE LA CLE NPD          MAR. 92 3.1
*            "             FIX BUG DANS EXDES             AVR. 92 3.2
*            "             (SAUVDEZ=0) ZAP(-1) APRES COPIEMAI. 93 3.3
*                          QLXINX POUR TYPE=2    
*                          MODIF DANS EXDES (RANGE INVERSE) 
*                          FIX BUG DANS STDCIPI/WEOFILE 
*                          OUVRE LES 1 @ 15 INPUTS QUI EXTSTENT  
*                          ENDIQUE L'ETAT DU PGM. A LA SORTIE
*                          EVITE LES ABORTS EN MODE INTERACTIF
*            "             CORRECTION FASTIO              JAN. 94 3.4
*                          PASSAGE DE 15 FICHIERS D'ENTREE A 35    
*            "             BUG FIX DATA NUMERO D'UNITES   JUIL 94 3.5
*            "             BUG FIX FICHIER SEQ EXISTANT   JUIL 94 3.6
*            "             BUG FIX LIMITE FICHIER SEQ     AVR. 95 3.7
*         Y. Bourassa      ENLEVE REFERENCES A 1950       MAI  95 3.8
*         M. Lepine        Utilisation des waio "cmcarc"  mai  95 3.9
*         M. Lepine        Reload avec waio modifie tres legerement sept 95 4.0
*         M. Lepine        "work around" pour bug -i 0 nov. 95 4.1
*         M. Lepine        Reload avec librmnx32stack.a avril 97 v4.2
*         M. Lepine        Reload avec c_baseio.o (2 write)  avril 97 v4.3
*         M. Lepine        modif message dans c_baseio (2 write)  avril 97 v4.4
*         M. Lepine        Reload avec fstd89.o (bugfix fstecr) mai 97 v4.5
*         M. Lepine        Extensions pour fstd98 - mars 98 - v5.0
*                          ouverture des fichiers sources en R/O.
*         M. Lepine        Bug fix date stamp superieur exdes - sept 98 - v5.1
*         M. Lepine        Rechargement avec dernier release - v5.2
*         M. Lepine        Ajout de la clef nrecmin  - v5.3
*         M. Lepine        Reload, bug fix c_fnom  - mars 1999 - v5.4
*         M. Lepine        Menage des if defined - mars 2000 - v5.5
*         M. Lepine        Reload pour datev (fstd89) - oct 2000 - v5.6
*         M. Lepine        Possibilite d'appel a convip pour les ip1 - juil 2001 - v5.7
*
*LANGAGE  - FTN77
*
*OBJET(EDITFST)
*         - COPIE UN FICHIER STANDARD (RANDOM OU SEQUENTIEL) DANS
*           UN AUTRE FICHIER STANDARD (RANDOM OU SEQUENTIEL).
*           UTILISE CCARD POUR RAMASSER LES PARAMETRES SUR L'ENONCE
*           D'EXECUTION DU PROGRAMME (DOIT AVOIR LA FORME SUIVANTE)
*           EDITFST -s(noms des fichiers sources)
*                   -d (nom  du  fichier  destination)
*                   -i (nom  du  fichier  stdinp)
*                   -l (nom  du  fichier  stdout)
*                   -ss               ( s=sequentiel sqi)         
*                   -sf               ( s=sequentiel fortran)
*                   -ds               ( d=sequentiel sqi)
*                   -df               ( d=sequentiel fortran)
*                   -n                ( pas de boite)
*                   -e                ( reecrire un enregistrement dans d)
*                   -f                ( fast IO)
*                   -eof 0<entier<15  ( marquer d si sequentiel)
*                   -m inform         ( diagnostiques)
*                   -m debug          ( diagnostiques et debug)
*                   -t                ( changer le niveau de tolerence)
*                   -v                ( un voir de d)
*                   -nrecmin          ( nombre minimal d'enregistrement)
*           FICHIERS (25@39 = SOURCE) (3 = DESTINATION)
      INTEGER   NMR, NMS, NME, NMN, NMM, NMD
      PARAMETER(NMR=12, NMS=25, NME=20, NMN=40, NMM=10, NMD=20)
*               NMR = MAXIMUM DE REGIONS
*               NMS =    "     " SCORES 
*               NME =    "     " ETAPES 
*               NMN =    "     " NIVEAUX
*               NMM =    "     " MODELES
*               NMD =    "     " DESIRES/EXCLURES 
      COMMON/TAPES / MEOF, COPIES, NDS, NDD, EOF, CEOF, LEOF, LIMITE, 
     X               NFS,  NFSO,   SOURCES(35), NRECMIN
      INTEGER        MEOF, COPIES, NDS, NDD, EOF, CEOF, LEOF, LIMITE, 
     X               NFS,  NFSO,   SOURCES, NRECMIN
*     - MEOF       - LEVEL D'EOF LOGIQUE A NE PAS PASSER
*     - COPIES     - NOMBRE D'AJOUTS AU FICHIER DESTINATION EN USAGE
*     - NDS        - NOMBRE D'ENREGISTREMENT FICHIER SOURCE EN USAGE
*     - NDD        -    "           "           "    DESTYINATION EN USAGE
*     - EOF        - TERMINE COPIE SEQUENTIELLE PAR UNE MARQUE=EOF
*     - CEOF       - CONTROLE LES MARQUES DE FIN DE FICHIER (DESTINATION)
*                    =-1 COPIE TOUS   "    "  "   "    "
*                    = I ECRIT DES    "    "  "   "    "    DE NIVEAU I
*                    = 0 AUCUN TRANSFER DE MARQUE DE FIN DE FICHIER
*     - LEOF       - VALEUR DE LA DERNIERE MARQUE DE FIN DE FICHIER LOGIQUE
*                    RENCONTREE DANS SOURCE
*     - LIMITE     - NOMBRE MAXIMUM DE COPIES PERMIS PAR L'USAGER
*     - NFS        -    "   DE FICHIERS SOURCES SOURCES
*     - NFSO       -    "    "    "        "       "    OUVERTS
*     - SOURCES    - TABLEAU DES NUMERO DE FICHIER SOURCE
      COMMON/FICHES/ NP, FIXD, ECR, SSEQ, VS, OUVS, DSEQ, VD, OUVD
      INTEGER        NP
      LOGICAL            FIXD, ECR, SSEQ, VS, OUVS, DSEQ, VD, OUVD
*     - IST        - INDICATEUR DU PREMIER POINT DE LA MEMOIRE FLOTTANTE
*     - NP         - NOMBRE DE PARAMETRES DANS SUB. APPELEE PAR DIRECTIVE
*     - FIXD       - INDIQUE QUE LES ENREGISTREMENTS DU FICHIER SOURCE
*                    SONT TOUS VALIDE EN MEME TEMPS.
*     - ECR        - =.TRUE. POUR RECRIRE.
*     - SSEQ       -    "    FICHIER SOURCE EST SEQUENTIEL (.FALSE.RANDOM)
*     - DSEQ       -    "       "     DESTIN "       "             "
*     - VS         -    "    DESIRE UN VOIR DU FICHIER SOURCE
*     - VD         -    "       "    "    "   "   "    "    DESTINATION
*     - OUVS       -    "    FICHIER SOURCE EST OUVERT
*     - OUVD       -    "       "    DESTIN  "     "
      COMMON/LOGIQ/  SCRI, XPRES, ESAIS, DM1, DEBUG, SELEC, BOX, DIAG,
     x               INTERAC, ZA
      LOGICAL        SCRI, XPRES, ESAIS, DM1, DEBUG, SELEC, BOX, DIAG,
     x               INTERAC, ZA
*     - SCRI       - .TRUE. SI DES CRITERES SUPLEMENTAIRE EN FORCE
*     - XPRES      - .TRUE. POUR COPIE COMPLETE DE SOURCE A DESTINATION
*     - ESAIS      -    "   SI UNE TENTATIVE DE COPIE A ETE FAITE.
*     - DM1        -    "    " PAS DE DATES DANS LES DESIRES
*     - DEBUG      -    "    " EN MODE DEBUG
*     - SELEC      -    "    " FICHIER DE DIRECTIVE PRESENT 
*     - BOX        -    "    " LA CLE NOBOX PAS DANS LA SEQUENSE D'APPEL
*     - DIAG       -    "    " DIAGNOSTIQUES SERONT A IMPRIMER
*     - INTERC     -    "    " EN MODE INTERACTIF
*     - ZA         -    "    " ON VEUT MODIFIER UN ETIQUETTE A LA SORTIE
      COMMON/DESRS/  JOURS(4), NREQ, SAUV, DESEXC(NMD), SATISF(NMD),
     X               NEXC, REQ(11,4,NMD), SUP(8,NMD), NIS, NJS, NKS,
     X               IG1S, IG2S, IG3S, IG4S, REQN(NMD), REQT(NMD),
     X               REQE(NMD), Z1, Z2, Z3, ZD
      INTEGER        NREQ, SAUV, DESEXC, SATISF, NEXC, REQ, SUP, NIS, 
     X               NJS, NKS, IG1S, IG2S, IG3S, IG4S, REQN, REQT, REQE,
     X               JOURS, Z1, Z2, Z3, ZD
*     - JOURS      - PERIODE A UTILISER PAR DESIRE/EXCLURE
*     - NREQ       - NOMBRE DE DIRECTIVES DESIRE/EXCLURE RENCONTREES
*     - SAUV       -    "    "     "      A CONSERVER APRES COPIE
*     - NEXC       -    "    "     "      EXCLURE 
*     - DESEXC(I)  - =0 (POUR EXCLURE),    =-1 (POUR DESIRE)
*     - SATISF(I)  - 0 = DIRECTIVE INSATISFAITE
*     - REQ        - TABLEAU DES DESIRES/EXCLURES DE L'USAGER.
*     - SUP        - TABLEAU DES CLES SUPLEMENTAIRES.
*     - NIS        - CRITERE SUPLEMENTAIRE DE CELECTION # 1 
*     - NJS        -    "          "        "     "     # 2 
*     - NKS        -    "          "        "     "     # 3 
*     - IG1S       -    "          "        "     "     # 4 
*     - IG2S       -    "          "        "     "     # 5 
*     - IG3S       -    "          "        "     "     # 6 
*     - IG4S       -    "          "        "     "     # 7 
*     - REQN       - NOMBRE DE NOMVAR/REQUETE
*     - REQT       - NOMBRE DE TYPVAR/REQUETE
*     - REQE       - NOMBRE D'ETIKET/REQUETE
*     - Z1         - IP1  A OsCHANGER SI DIFFERENT DE -1
*     - Z2         - IP2  "    "     "     "      "  "
*     - Z3         - IP3  "    "     "     "      "  "
*     - ZD         - DATE "    "     "     "      "  "
*     - ZG1@ZG4    - IG1 A IG4 A ZAPPER
      COMMON /KEY/   KLE(60), DEF1(60), DEF2(60), PRINTR
      CHARACTER*8    KLE
      CHARACTER*128           DEF1,     DEF2,     PRINTR
*     - KLE        - CLES DE LA SEQUENCE D'APPEL
*     - DEF1       - VALEURE DES CLES PAR DEFAUT
*     - DEF2       -     "    "    "  SI PRESENTES
*     - PRINTR     - DN DU FICHIER DE SORTIE D'IMPRIMENTE
      COMMON /CHAR/  NS, ND, SNOM, DNOM, ZE, ETI, ETIS(10,NMD), 
     X               ZT, TYP, TYPS(10,NMD), GTY, GTYS(NMD), GTYPS,
     X               ZN, NOM, NOMS(10,NMD), ETAT
      CHARACTER *1   GTY, GTYS, GTYPS
      CHARACTER *2   ZT, TYP, TYPS
      CHARACTER *4   ZN, NOM, NOMS
      CHARACTER *6   ETAT
      CHARACTER *12  ZE, ETI, ETIS
      CHARACTER *128 NS, ND
      CHARACTER *15  SNOM, DNOM
*     - NS         - DN DU FICHIER SOURCE
*     - ND         -  "  "    "    DESTINATION
*     - SNOM       - TYPE  PASSE A FNOM FICHIER SOURCE
*     - DNOM       -   "     "   "   "     "    DESTINATION 
*     - ZE         - CARACTERES DE L'ETIQUETTE A ZAPPER SI DIFFERENT DE '????????'
*     - ZG         - TYPE DE GRILLE A ZAPPER
*     - ETI        - ETIQUETTE TEMPORAIRE
*     - ETIS       - ETIQUETTES DES DESIRES/EXCLURES
*     - ZT         - CARACTERE DU TYPEVAR A ZAPPER SI DIFFERENT DE '?'.
*     - TYP        - TYPE DE GRILLE
*     - TYPS       - TYPE DE GRILLEDES DES DESIRES/EXCLURES 
*     - GTY        - TYPE DE GRILLE (SOURCE)
*     - GTYS       - TYPE DE GRILLE DES CRITERES SUPLEMENTAIRES
*     - GTYPS      - TYPE DE GRILLE DES CRITERES SUPLEMENTAIRES
*     - ZN         - CARACTERES DU NOMVAR A ZAPPER SI DIFFERENT '??'
*     - NOM        - NOM DE VARIABLE
*     - NOMS       - NOMS DE VARIABLE DES DESIRES/EXCLURES
*     - ETAT       - INDIQUE L'ETAT DU PGM. DANS LA BOITE A LA FIN
      EXTERNAL    FERMED, SELECT, FNOM, CCARD, OUVRES, SAUVDEZ, FERMES,
     X            FSTOPC, FSTOPL, EXDB, EXFIN, OUVRED, STDCOPI, MEMOIRH
      EXTERNAL    QQEXIT
      INTEGER     FSTOPC, FSTOPL, EXDB, EXFIN, OUVRED, FNOM, I
      integer junk
      LOGICAL     FASTIO
      DATA KLE /'NNN', 'D.', 'EOF', 'SSEQ', 'DSEQ', 'VD', 'NOBOX',
     X          'DIAG', 'ECR', 'I.', 'L.', 'K', 'M', 'T', 'C', 'SS',
     X          'DS', 'V', 'N', 'VS', 'E', 'F', 'SF', 'DF',
     X          'NRECMIN', 35*'S.'/
      DATA DEF1/'OUI', ' ', '0', 'NON', 'NON', 'NON', 'NON', 'NON', 
     X          'NON', '$IN', '$OUT', 'FATALE', 'ERRORS', 'FATALE',
     X          '-1',  'NON', 'NON', 'NON', 'NON', 'NON', 'NON', 'OUI',
     X          'NON', 'NON', '-1', 35*' '/
      DATA DEF2/'OUI', ' ', '0', 'OUI', 'OUI', 'OUI', 'OUI', 'OUI', 
     X          'OUI', '$IN', '$OUT', 'ERRORS', 'INFORM', 'ERRORS',
     X          '-1',  'OUI', 'OUI', 'OUI', 'OUI', 'OUI', 'OUI', 'OUI',
     X          'OUI', 'OUI', '-1', 35*' '/
      DATA VS, OUVS, OUVD, FIXD, DEBUG, XPRES, SCRI, ESAIS, INTERAC, ZA
     X    /10 * .FALSE./
      DATA JOURS, DESEXC, SATISF,  NS,    ETAT
     X    /  4*0,  NMD*0,  NMD*0, ' ', 'NORMAL'/
      DATA NREQ, NEXC, CEOF, SAUV, MEOF, LEOF, NFSO
     X    /   0,    0,    0,    0,    1,    0,    0/
      DATA NFS, NIS, NJS, NKS, GTYPS, IG1S, IG2S, IG3S, IG4S
     X    /  0,  -1,  -1,  -1,   ' ',   -1,   -1,   -1,   -1/
      DATA SOURCES /25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,
     X              40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,
     X              55,56,57,58,59/
      SAUV = 0
      CALL SAUVDEZ
*     EXTRACTION DES CLES DE LA SEQUENCE D'APPEL. 
      I    = -111  
      CALL CCARD(KLE, DEF2, DEF1, 59, I)
      READ(DEF1(3), '(I2)') EOF
      READ(DEF1(15),'(I8)') LIMITE
      READ(DEF1(25),'(I8)') NRECMIN
      PRINTR = DEF1(11)
      ND     = DEF1(2)
      VD     = (DEF1(6) .EQ.'OUI')  .OR.  (DEF1(18).EQ.'OUI') 
      VS     = (DEF1(20).EQ.'OUI')
      BOX    = (DEF1(7) .EQ.'NON')  .AND. (DEF1(19).EQ.'NON') 
      ECR    = (DEF1(9) .EQ.'OUI')  .OR.  (DEF1(21).EQ.'OUI') 
      SELEC  = (DEF1(10).NE.'NON')  .AND. (DEF1(10).NE.'NIL') .AND.
     X         (DEF1(10).NE.'0')
*
*     Contourner le bug du -i 0 en ouvrant l'unite 5 sur /dev/null
*
      IF (.NOT. SELEC) THEN
	SELEC = .TRUE.
	DEF1(10) = '/dev/null'
      ENDIF
      DEBUG  = (DEF1(13)  .EQ.'DEBUGS')
      FASTIO = (DEF1(22).EQ.'OUI') 
      DIAG   = (DEF1(8) .EQ.'OUI')  .OR.  (DEF1(13).EQ.'INFORM') 
      IF((DEF1(16).NE.'NON') .OR. (DEF1(4).NE.'NON') .OR.
     X   (DEF1(23).NE.'NON') ) THEN
         IF(DEF1(23) .NE.'NON') THEN
            SNOM = 'STD+SEQ+R/O+OLD'
         ELSE
            SNOM = 'STD+SEQ+OLD+R/O'
         ENDIF
      ELSE
         SNOM = 'STD+RND+OLD+R/O'
      ENDIF
      IF((DEF1(17).NE.'NON') .OR. (DEF1(5).NE.'NON') .OR.
     X   (DEF1(24).NE.'NON') ) THEN
         IF(DEF1(24) .NE.'NON') THEN
            DNOM = 'STD+SEQ+FTN'
         ELSE
            DNOM = 'STD+SEQ'
         ENDIF
      ELSE
         DNOM = 'STD+RND'
      ENDIF
      I = FNOM(6, PRINTR, 'SEQ', 0)
*     IMPRIME L'INDICATIF DE DEBUT DU PROGRAMME.
      IF( BOX ) THEN
         I = EXDB('EDITFST','V5.7','NON')
      ELSE
         WRITE(6,*)'***   E D I T F S T   V5.7   ***'
      ENDIF
      IF( DIAG ) THEN
         I = FSTOPC('MSGLVL', 'INFORM', .FALSE.)
      ELSE
         I = FSTOPC('MSGLVL', DEF1(13), .FALSE.)
      ENDIF
      IF(DEF1(14) .NE. 'FATALE') THEN
         I = FSTOPC('TOLRNC', DEF1(14), .FALSE.)
      ELSE
         I = FSTOPC('TOLRNC', DEF1(12), .FALSE.)
      ENDIF
      I = FSTOPL('FASTIO', FASTIO, .FALSE.)
      I = FSTOPL('IMAGE',  .TRUE., .FALSE.)
*     COMPTEE LES FICHIERS SOURCES
      DO 10 I=1,35
         IF(DEF1(I+25) .NE. ' ') NFS = NFS+1
 10   CONTINUE
      IF(NFS .GT. 0) THEN
         CALL OUVRES( DEF1(26) )
         IF(NFSO .EQ. 0) THEN
            PRINT*,' *********************************'
            PRINT*,'***         PAS DE COPIE        ***'
            PRINT*,'***    FICHIER SOURCE INCONNU   ***'
            PRINT*,' *********************************'
            ETAT = 'ABORT'
            GO TO 30
         ENDIF
      ENDIF
*     OUVRE LE FICHIER DESTINATION
      IF(DEF1(2) .NE. ' ') I = OUVRED( DEF1(2) )
*     LIT UN ENSEMBLE DE DIRECTIVES?
      IF( SELEC ) THEN
         I = FNOM(5, DEF1(10), 'SEQ', 0)
         IF(DEF1(10) .EQ. '$IN') THEN
*           mode interactif, attend les directives
            INTERAC = .TRUE.
            PRINT*,'DONNEZ VOS DIRECTIVES S.V.P.'
            PRINT*,'TAPPEZ  END  POUR INDIQUER LA FIN DES DIRECTIVES'
         ENDIF
         CALL SELECT 
      ENDIF
*     SI PAS DE DIRECTIVES STDCOPI OU SEQCOPI ALORS
      IF( .NOT.ESAIS ) THEN
         IF( OUVS .AND. OUVD ) THEN
            NP = 1
            CALL STDCOPI( -1,-1,-1,-1 )
         ELSE
            PRINT*,               ' *********************************'
            PRINT*,               '***         PAS DE COPIE        ***'
            IF(.NOT. OUVS) PRINT*,'***    FICHIER SOURCE INCONNU   ***'
            IF(.NOT. OUVD) PRINT*,'*** FICHIER DESTINATION INCONNU ***'
            PRINT*,               ' *********************************'
            ETAT = 'ABORT'
         ENDIF
      ENDIF
*     TOUT EST TERMINE , FERME LES FICHIERS
      CALL FERMES
      CALL FERMED
*     IMPRIME L'INDICATIF DE FIN DU PGM.
   30 IF( BOX ) THEN
         I = EXFIN('EDITFST', ETAT, 'NON')
      ELSE
         IF(ETAT .EQ. 'ABORT') THEN
            WRITE(6,*)'***   E D I T I O N   A V O R T E   ***'
         ELSE
            WRITE(6,*)'***   E D I T I O N   T E R M I N E   ***'
         ENDIF
      ENDIF
      IF(ETAT .EQ. 'ABORT') CALL QQEXIT(50)
      STOP
      END 
