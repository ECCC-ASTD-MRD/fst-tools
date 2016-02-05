
*

*
*  MACRO ICHAMP(MOT,FIN,LG)   EXTRAIT UN CHAMP D'UN MOT
*
*   MOT   MOT QUI CONTIENT LE CHAMP
*   FIN   NUMERO DU DERNIER BIT (A DROITE) DU CHAMP, EN NUMEROTATION
*         GAUCHE > DROITE (LE BIT 0 EST A DROITE DU MOT).
*   LG    LONGUEUR, EN BITS, DU CHAMP
*

*
*  MACRO IUNPAK(BASE, BITPOS, LG)  OBTENIR UN CHAMP D'UN TABLEAU
*
*  BASE    TABLEAU CONTENANT LE CHAMP A EXTRAIRE
*  BITPOS  POSITION DU BIT DE DROITE DU CHAMP A EXTRAIRE
*          LE BIT 0 EST LE BIT SIGNE DU PREMIER MOT DU TABLEAU.
*  LG      EST LE NOMBRE DE BITS QU'OCCUPE LE CHAMP. (MAX 32 BITS)
*

*
*
*  MACRO GETBIT(BASE, BITPOS, LG)  OBTENIR UN CHAMP D'UN TABLEAU
*
*  BASE    TABLEAU CONTENANT LE CHAMP A EXTRAIRE
*  BITPOS  POSITION DU BIT DE DROITE DU CHAMP A EXTRAIRE
*          LE BIT 0 EST LE BIT SIGNE DU PREMIER MOT DU TABLEAU.
*  LG      EST LE NOMBRE DE BITS QU'OCCUPE LE CHAMP. (MAX 32 BITS)
*

*
*
*  MACRO INSERT(TABL,KWA,BITPOS,LONG)  INSERER UN CHAMP DANS UN TABLEAU
*
*  TABL    TABLEAU QUI CONTIENDRA LE CHAMP APRES INSERTION
*  KWA     MOT QUI CONTIENT LE CHAMP A INSERER JUSTIFIE A DROITE
*  BITPOS  POSITION DU DERNIER BIT (A DROITE) DU CHAMP A INSERER
*          LE BIT 0 EST LE BIT SIGNE DU PREMIER MOT DU TABLEAU
*  LONG    LONGUEUR EN BIT DU CHAMP A INSERER (PAS PLUS DE 32 BITS)
*

*
*  MACRO PUTBIT(TABL,KWA,BITPOS,LONG)  INSERER UN CHAMP DANS UN TABLEAU
*
*  TABL    TABLEAU QUI CONTIENDRA LE CHAMP APRES INSERTION
*  KWA     MOT QUI CONTIENT LE CHAMP A INSERER JUSTIFIE A DROITE
*  BITPOS  POSITION DU DERNIER BIT (A DROITE) DU CHAMP A INSERER
*          LE BIT 0 EST LE BIT SIGNE DU PREMIER MOT DU TABLEAU
*  LONG    LONGUEUR EN BIT DU CHAMP A INSERER (PAS PLUS DE 32 BITS)
*

*
*  MACRO CLRBIT(TABL,BITPOS,LONG)  METTRE A ZERO UN CHAMP DANS UN TABLEAU
*
*  TABL    TABLEAU
*  BITPOS  POSITION DU DERNIER BIT (A DROITE) DU CHAMP A NETTOYER
*          LE BIT 0 EST LE BIT SIGNE DU PREMIER MOT DU TABLEAU
*  LONG    LONGUEUR EN BIT DU CHAMP A NETTOYER (PAS PLUS DE 32 BITS)
*

*
*  MACRO PUTBITC(TABL,KWA,BITPOS,LONG)  INSERER UN CHAMP DANS UN TABLEAU
*                                       AVEC NETTOYAGE PRELIMINAIRE
*  TABL    TABLEAU QUI CONTIENDRA LE CHAMP APRES INSERTION
*  KWA     MOT QUI CONTIENT LE CHAMP A INSERER JUSTIFIE A DROITE
*  BITPOS  POSITION DU DERNIER BIT (A DROITE) DU CHAMP A INSERER
*          LE BIT 0 EST LE BIT SIGNE DU PREMIER MOT DU TABLEAU
*  LONG    LONGUEUR EN BIT DU CHAMP A INSERER (PAS PLUS DE 32 BITS)
*

*
* MACRO QQQELM       -   EXTRAIRE UN ELEMENT  DE NBITS A PARTIR
*                        DU BIT DERBIT , A PARTIR DU MOT COURANT WI
*                        D'UN VECTOR BUF
*
*APPEL:
*   ELEMENT = QQQELM(BUF,WI,DERBIT,NBITS)
*

*
*MACRO QQQXPUT      -    INSERER UN ELEMENT DE NBIT A PARTIR DE DERBIT
*                        DANS UN BUFFER, A PARTIR DU MOT COURANT WI
*
* APPEL:      QQQXPUT(BUF,WI,ELEMENT,DERBIT,NBIT)
      PROGRAM REFLEX
*

      IMPLICIT NONE

*
*AUTEUR           J. CAVEEN NOVEMBRE 1990
*
*OBJET(REFLEX)
*       RECUPERATION ET FUSION LOGIQUE D'ENREGISTREMENTS XDF.
*       PROGRAMME SERVANT A RECUPERER TOUS LES ENREGISTREMENTS
*       VALIDES DE UN OU PLUSIEURS (JUSQU'A 10) FICHIERS XDF
*       AFIN DE LES REGROUPER SUR UN SEUL FICHIER DE SORTIE.
*       ON RECONSTRUIT L'ENTETE DU FICHIER AINSI QUE LES PAGES
*       DE REPERTOIRE AU FUR ET A MESURE.
*
*MODIFICATION:  J. CAVEEN, SEPTEMBRE 1992
*
*       AJOUT DE NOUVELLES CLEFS PERMETTANT DE CONTROLER LE NIVEAU
*       DE TOLERANCE ET D'EMISSION DES MESSAGES.
*            -ERRTOLR NIVEAU  : FORCE LA TOLERANCE POUR LES ERREURS DE
*                               SEVERITE INFERIEURE A NIVEAU
*            -MSGLVL  NIVEAU  : FORCE L'IMPRESSION DES MESSAGES DE SEVERITE
*                               NIVEAU ET PLUS
*
*       AJOUT DE NOUVELLES FONCTIONS ET NOUVELLES CLEFS:
*
*            -RSTR   -   PREMET DE RESTAURER UN FICHIER A SON ETAT ORIGINAL
*            -STATS  -   PERMET D'OBTENIR CERTAINES STATISTIQUES D'UN FICHIER
*                        ET DE SAVOIR SI LE FICHIER EST ENDOMMAGE OU NON.
*
* J.Caveen, mars 1995
*        -ajout des appels a la fonction existe afin de d'eviter
*         la creation de fichiers vides.
*        -change definition des clefs afin de forcer ccard
*               a ne pas les mettre en majuscules
*              -change fonctionnement afin d'ignorer la presence de fichiers
*               vides
*       M. Lepine, avril 1997 - Rechargement avec lirbmnx32stack.a
*       M. Lepine, avril 1998 - Rechargement avec lirbmnbeta32stack.a
*       M. Lepine, Oct 1998 - Rechargement avec dernier release de rmnlib (2.4)
*       M. Lepine, Oct 2001 - Rechargement (bug fix realloc xdfuse) (2.5)
*       M. Lepine, Jan 2003 - Rechargement avec rmn_x (2.6)
*       M. Lepine, Mars 2004 - option nsplit pour subdiviser l'output sur plusieurs fichiers
*       M. Lepine, Fevrier 2008 - Rechargement avec librmn_009 (2.8)
*       M. Lepine, Fevrier 2010 - Terminer avec un code d erreur si le fichier intrant est inexistant (2.9)
*       D. Bouhemhem, Nov 2013 - Rechargement avec librmn_013 (3.0)
*       M. Lepine, Mars 2014 - Rechargement avec librmn_014 (3.1)
*       D. BouhemhemM, Dec. 2014 - Rechargement avec librmn_015.1 (3.2)
*       M. Lepine, Fev. 2015 - Rechargement avec librmn_015.2 (3.3)
*       M. Lepine, Mars 2015 - Retourner un code d'erreur facultatif si le fichier est endommage (3.4)
*
*MODULES
      INTEGER EXDB, EXFIN, FNOM, longueur
      EXTERNAL EXDB, EXFIN, FNOM, CCARD, longueur
      INTEGER XDFSTA, XDFCLS, WASIZE
      EXTERNAL XDFSTA, XDFCLS, WASIZE
      INTEGER XDFUSE, XDFIMP, QDFRSTR, QDFDIAG, XDFOPT
      EXTERNAL XDFUSE, XDFIMP, QDFRSTR, QDFDIAG, XDFOPT
**

      INTEGER TOLERE
      COMMON /XDFTLR/ TOLERE
*
*     TOLERE  NIVEAU DE TOLERANCE POUR LES ERREURS

      INTEGER MESSLV
      COMMON /XDFMSL/ MESSLV
*
*     MESSLV  NIVEAU DE TOLERANCE POUR LES MESSAGES

      CHARACTER*8 LISTE(18), PROG, STATUS
      character(len=16) :: RELEASE
      CHARACTER*256 DEF(18), VAL(18)
      LOGICAL ERREXIT
      INTEGER I, IER, IER2, ISTAMP, FICHIN, FICHOUT ,IPOS, N1024,
     % LNGR
      INTEGER NSPLIT,nf
      INTEGER PRIDEF(2, 100), AUXDEF(2, 100), STAT(12)
      CHARACTER*4 CVRSN, CAPPL, suffix
      DATA PROG, STATUS /'REFLEX','O.K.'/
      DATA LISTE /'IXENT.','IXENT.','IXENT.','IXENT.','IXENT.',
     %'IXENT.','IXENT.','IXENT.','IXENT.','IXENT.','OXSRT.','DATE',
     %'STATS','RSTR','ERRTOLR','MSGLVL','NSPLIT','ERREXIT'/
      DATA DEF   /' '   ,' '   ,' '   ,' '   ,' '   ,' ',' '   ,' '
     %   ,' '   ,' '   ,' ','OPRUN','OUI','OUI','ERROR','ERROR','1',
     %   'OUI'/
      DATA VAL   /' '   ,' '   ,' '   ,' '   ,' '   ,' ',' '   ,' '
     %   ,' '   ,' '   ,' ','NON','NON','NON','ERROR','ERROR','1',
     %   'NON'/

      include 'version.inc'
     
*
*        RECUPERATION DES PARAMETRES D'APPEL
*
      ERREXIT = .false.
      IPOS = -1
      CALL CCARD(LISTE,DEF,VAL,17,IPOS)
*       DEBUT D'EXECUTION

      ISTAMP = EXDB(PROG,RELEASE,VAL(12))
*
*       INITIALISER LES NIVEAUX DE TOLERANCE ET DE MESSAGES
*
      if (val(18) .eq. 'OUI') ERREXIT = .true.
      IER = XDFOPT('ERRTOLR',VAL(15),-1)
      IER = XDFOPT('MSGLVL',VAL(16),-1)
      read(val(17),'(I)') nsplit
      if (nsplit .gt. 1) ier = XDFOPT('STRIPING',' ',nsplit)
c      print *,'Debug+ apres appels a XDFOPT'
*
*       VERIFIER LA PRESENCE D'UN FICHIER DE SORTIE
*

      IF((VAL(11).NE. ' '))THEN
         FICHOUT = 20
         if (nsplit .eq. 1) then
           IER = FNOM(FICHOUT,VAL(11),'RND',0)
         else
           do nf = 1,nsplit
             write(suffix,'(a,i3.3)') '_',nf
             ier = fnom(fichout+nf-1,
     %                  VAL(11)(1:longueur(val(11)))//suffix,'RND',0)
           enddo
         endif
c      print *,'Debug+ apres fnom fichier out'
*
*           ON RESTAURE LE FICHIER
*

         IF((VAL(14).EQ. 'OUI'))THEN
            FICHIN = 10
            IER = FNOM(FICHIN,VAL(1),'RND+R/O',0)
            IF((IER.EQ. 0))THEN
               IER2 = XDFSTA(FICHIN,STAT,12,PRIDEF, 100,AUXDEF, 100,
     %         CVRSN,CAPPL)
            ELSE
              WRITE(6,5000) VAL(1)
              STATUS = 'ERREUR'
              GOTO 5555
            ENDIF
            IF(( IER.EQ. 0 .AND. IER2.EQ. 0))THEN
               IER = QDFRSTR(FICHIN,FICHOUT)
               IF((IER.LT. 0))THEN
                  STATUS = 'ERREUR'
                  GOTO 5555
               ENDIF
            ELSE
               WRITE(6,5000) VAL(1)
            ENDIF
         ELSE

*
*              ON FAIT LA RECUPERATION STANDARD
*
c      print *,'Debug+ recuperation standard'
            FICHIN = 10
            DO 23010 I = 1,10
               IF((VAL(I).NE. ' '))THEN
                  IER = FNOM(FICHIN,VAL(I),'RND+R/O',0)
c      print *,'Debug+ apres fnom fichier in'
                  IF((IER.EQ. 0))THEN
c      print *,'Debug+ appel a XDFSTA fichier in'
                     IER2 = XDFSTA(FICHIN,STAT,12,PRIDEF, 100,AUXDEF
     %               , 100,CVRSN,CAPPL)
c      print *,'Debug+ apres XDFSTA'
                  ELSE
                    WRITE(6,5000) VAL(I)
                    STATUS = 'ERREUR'
                    GOTO 5555
                  ENDIF
                  IF(( IER.EQ. 0 .AND. IER2.EQ. 0))THEN
c      print *,'Debug+ appel a XDFUSE'
                     IER = XDFUSE(FICHIN,FICHOUT)
c      print *,'Debug+ apres XDFUSE'
                     IF((IER.LT. 0))THEN
                        STATUS= 'ERREUR'
                        GO TO 5555
                     ENDIF
                  ELSE
                     WRITE(6,5000) VAL(I)
                  ENDIF
               ENDIF
               FICHIN = FICHIN + 1

*               FERMER LE FICHIER DE SORTIE
23010       CONTINUE

            do nf = 1,nsplit
              ier = xdfcls(fichout+nf-1)
            enddo

*           ECRIRE LES STATISTIQUES DU FICHIER DE SORTIE
*
         ENDIF
         IF(((INDEX(VAL(16),'TRIVIAL').NE. 0).OR.   (INDEX(VAL(16),
     %   'INFORMATIF').NE. 0)))THEN
            do nf = 1,nsplit
              IER = XDFSTA(FICHOUT+nf-1,STAT,12,PRIDEF, 100,AUXDEF, 100,
     %        CVRSN,CAPPL)
              IER = XDFIMP(FICHOUT+nf-1,STAT,12,PRIDEF,AUXDEF,
     %        CVRSN,CAPPL)
            enddo
         ENDIF
      ELSE
         IF((VAL(13).EQ. 'OUI'))THEN

*
*           ON DEMANDE LES STATISTIQUES DES FICHIERS D'ENTREE
*
            FICHIN = 10
            DO 23024 I = 1,10
               IF((VAL(I).NE. ' '))THEN
                  IER = FNOM(FICHIN,VAL(I),'RND+R/O',0)
                  IF(( IER.EQ. 0))THEN
                     IER = QDFDIAG(FICHIN)
                     if ((IER .lt. 0) .and. ERREXIT) status = 'ERREUR'
                  ELSE
                     WRITE(6,5000) VAL(I)
                     STATUS = 'ERREUR'
                     GOTO 5555
                  ENDIF
               ENDIF
               FICHIN = FICHIN + 1
23024       CONTINUE
         ELSE
            WRITE(6,*) ' ON DOIT SPECIFIER UN FICHIER DE SORTIE OU '
     %      ,'UTILISER LA CLEF -STATS A L''APPEL'
            STATUS = 'ERREUR'
         ENDIF
      ENDIF
5555  CONTINUE
      ISTAMP = EXFIN(PROG,STATUS,VAL(12))
      IF (status == 'ERREUR') call qqexit(1)
5000  FORMAT(' ERREUR, FICHIER VIDE OU INEXISTANT: ',A)
      STOP
      END
      
      character *128 function product_id_tag()
      product_id_tag='$Id: reflex.f 46 2015-02-25 21:21:49Z armnlib $'
      return
      end
