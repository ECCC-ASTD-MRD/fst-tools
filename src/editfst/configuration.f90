module configuration
!     internal variables used all over editfst
!
      use rmn_fst24
      implicit none
      integer, private :: i
      integer, PARAMETER :: NMR=12, NMS=25, NME=20, NMN=40, NMM=10, NMD=50, NML=50
      character(len=*), parameter :: LIN128 = '(32A4)'
      logical, save :: strict_mode = .false.
!
! NOTE:
!     le nombre maximum de valeurs dans une liste de desiderata
!     est limite (hardcoded) a NML=10 ( a corriger eventuellement)
!
! GTYS, TYPS, NOMS, ETIS : tables utilisee pour stocker les requetes
!     GTYS sert pour critere supplementaire type de grille
!
      character(len=1)   , save :: GTY, GTYPS=' ', GTYS(NMD)
      character(len=2)   , save :: ZT, TYP, TYPS(NML,NMD)
      character(len=4)   , save :: ZN, NOM, NOMS(NML,NMD)
      character(len=6)   , save :: ETAT='NORMAL'
      character(len=12)  , save :: ZE, ETI, ETIS(NML,NMD)
      character(len=4096), save :: NS=' ', ND
      character(len=15)  , save :: SNOM, DNOM

      integer, parameter :: NCCARDKEYS=147   ! dimension for program options (processed by ccard)
      character(len=8), dimension(NCCARDKEYS), save ::    &
      kle = (/ 'NNN     ', 'D:      ', 'EOF     ', 'SSEQ    ', 'DSEQ    ', 'VD      ', &
               'NOBOX   ', 'DIAG    ', 'ECR     ', 'I.      ', 'L.      ', 'K       ', &
               'M       ', 'T       ', 'C       ', 'SS      ', 'DS      ', 'V       ', &
               'N       ', 'VS      ', 'E       ', 'F       ', 'SF      ', 'DF      ', &
               'NRECMIN ',('S:      ',i = 1,120),  'DRYRUN  ', 'STRICT  ' /),                      &
      def1b=(/ 'OUI     ', '        ', '0       ', 'NON     ', 'NON     ', 'NON     ', &
               'NON     ', 'NON     ', 'NON     ', '$IN     ', '$OUT    ', 'FATALE  ', &
               'INFO    ', 'FATALE  ', '-1      ', 'NON     ', 'NON     ', 'NON     ', &
               'NON     ', 'NON     ', 'NON     ', 'OUI     ', 'NON     ', 'NON     ', &
               '-1      ',('        ',i = 1,120),  '        ', 'NON     ' /),                       &
      def2b=(/ 'OUI     ', '        ', '0       ', 'OUI     ', 'OUI     ', 'OUI     ', &
               'OUI     ', 'OUI     ', 'OUI     ', '$IN     ', '$OUT    ', 'ERRORS  ', &
               'INFO    ', 'ERRORS  ', '-1      ', 'OUI     ', 'OUI     ', 'OUI     ', &
               'OUI     ', 'OUI     ', 'OUI     ', 'OUI     ', 'OUI     ', 'OUI     ', &
               '-1      ',('        ',i = 1,120),  'DRYRUN  ', 'OUI     ' /)
!
! REQ : table utilisee pour stocker les requetes
      integer, save :: JOURS(4),REQ(11,4,NMD)
! si REQ(11,...) = -1
!     REQ(1,...)  valeur de depart
!     REQ(2,...)  valeur de fin
!     REQ(3,...)  delta
! sinon  REQ(11,...) = nombre de valeurs a utiliser
!     REQ(1:REQ(11,...),....) liste des valeurs recherchees (maximum de 10 valeurs)
!     req(,1,) IP1
!     req(,2,) IP2
!     req(,3,) IP3
!     req(,4,) DATE
!
! SUP : table utilisee pour les parametres supplementaires
!     sup(8,...) == 1 s'il y en a d'associes a la requete
!     sup(1:7,...) ni.nj.nk.ig1,ig2,ig3,ig4
!
! REQN, REQE, REQT : population de NOMS, ETIS, TYPS
!     DESEXC : flag desire/exclure associe a la requete
!     SATISF : nb de requetes satisfaites
!
      integer, save :: NREQ=0, SAUV=0, DESEXC(NMD), SATISF(NMD),                    &
                       NEXC=0, SUP(8,NMD), NIS=-1, NJS=-1, NKS=-1,                  &
                       IG1S=-1, IG2S=-1, IG3S=-1, IG4S=-1, REQN(NMD), REQT(NMD),    &
                       REQE(NMD), Z1, Z2, Z3, ZD
      integer, save :: NP  ! nb de parametres passes lors de l'appel courant a une directive (readlx)
      integer, save :: MEOF=1, COPIES, NDS, NDD, EOF, CEOF=0, LEOF=0, LIMITE, NFS=0,  NFSO=0, NRECMIN
      logical, save :: SCRI=.false., XPRES=.false., ESAIS=.false.,      &
                       DM1, DEBUG=.false., SELEC, BOX, DIAG,            &
                       INTERAC=.false., ZA=.false., DRYRUN=.false.,     &
                       FIXD=.false., ECR, SSEQ=.false., VS=.false.,     &
                       OUVS=.false., DSEQ, VD, OUVD=.false.
      integer, save :: max_requetes_exdes = 0
      integer, save :: max_nlist_exdes = 0

      type(fst_file), dimension(120) :: sources
      type(fst_file) :: destination
contains

subroutine config_init  ! initialisation des tableaux 
  req     = 0
  jours   = 0
  desexc  = 0   ! ni desire, ni exclure
  satisf  = 0
  sup = 0
end subroutine config_init

end module configuration
