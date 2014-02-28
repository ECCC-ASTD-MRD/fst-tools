module configuration
      implicit none
      integer, private :: i
      integer, parameter :: NCCARDKEYS=146
      integer, PARAMETER :: NMR=12, NMS=25, NME=20, NMN=40, NMM=10, NMD=20
      character(len=*), parameter :: LIN128 = '(32A4)'

!      COMMON /CHAR/  NS, ND, SNOM, DNOM, ZE, ETI, ETIS(10,NMD),       &
!                     ZT, TYP, TYPS(10,NMD), GTY, GTYS(NMD), GTYPS,    &
!                     ZN, NOM, NOMS(10,NMD), ETAT
      character *1   , save :: GTY, GTYPS=' ', GTYS(NMD)
      character *2   , save :: ZT, TYP, TYPS(10,NMD)
      character *4   , save :: ZN, NOM, NOMS(10,NMD)
      character *6   , save :: ETAT='NORMAL'
      character *12  , save :: ZE, ETI, ETIS(10,NMD)
      character *128 , save :: NS=' ', ND
      character *15  , save :: SNOM, DNOM

      character(len=4096) ,dimension(:), pointer, save:: DEF1,     DEF2
      character(len=4096), save :: PRINTR
      character(len=8), dimension(NCCARDKEYS), save ::    &
      def1b=(/ 'OUI     ', '        ', '0       ', 'NON     ', 'NON     ', 'NON     ', &
               'NON     ', 'NON     ', 'NON     ', '$IN     ', '$OUT    ', 'FATALE  ', &
               'ERRORS  ', 'FATALE  ', '-1      ', 'NON     ', 'NON     ', 'NON     ', &
               'NON     ', 'NON     ', 'NON     ', 'OUI     ', 'NON     ', 'NON     ', &
               '-1      ',('        ',i = 1,120),  '        '/),                       &
      def2b=(/ 'OUI     ', '        ', '0       ', 'OUI     ', 'OUI     ', 'OUI     ', &
               'OUI     ', 'OUI     ', 'OUI     ', '$IN     ', '$OUT    ', 'ERRORS  ', &
               'INFORM  ', 'ERRORS  ', '-1      ', 'OUI     ', 'OUI     ', 'OUI     ', &
               'OUI     ', 'OUI     ', 'OUI     ', 'OUI     ', 'OUI     ', 'OUI     ', &
               '-1      ',('        ',i = 1,120),  'DRYRUN  '/),                       &
      kle = (/ 'NNN     ', 'D:      ', 'EOF     ', 'SSEQ    ', 'DSEQ    ', 'VD      ', &
               'NOBOX   ', 'DIAG    ', 'ECR     ', 'I.      ', 'L.      ', 'K       ', &
               'M       ', 'T       ', 'C       ', 'SS      ', 'DS      ', 'V       ', &
               'N       ', 'VS      ', 'E       ', 'F       ', 'SF      ', 'DF      ', &
               'NRECMIN ',('S:      ',i = 1,120),  'DRYRUN  ' /)

      integer*8, save :: JOURS(4),REQ(11,4,NMD)
! si REQ(11,...) = -1
!     REQ(1,...)  valeur de depart
!     REQ(2,...)  valeur de fin
!     REQ(3,...)  delta
! sinon  REQ(11,...) = nombre de valeurs a utiliser
!     REQ(1:REQ(11,...),....) liste des valeurs recherchees
!     req(,1,) IP1
!     req(,2,) IP2
!     req(,3,) IP3
!     req(,4,) DATE

      integer, save ::  NREQ=0, SAUV=0, DESEXC(NMD), SATISF(NMD),                    &
                        NEXC=0, SUP(8,NMD), NIS=-1, NJS=-1, NKS=-1,                         &
                        IG1S=-1, IG2S=-1, IG3S=-1, IG4S=-1, REQN(NMD), REQT(NMD),            &
                        REQE(NMD), Z1, Z2, Z3, ZD
      integer, save ::  NP
      integer, save ::  MEOF=1, COPIES, NDS, NDD, EOF, CEOF=0, LEOF=0, LIMITE, NFS=0,  NFSO=0,   SOURCES(120), NRECMIN

      logical, save :: SCRI=.false., XPRES=.false., ESAIS=.false.,      &
                       DM1, DEBUG=.false., SELEC, BOX, DIAG,            &
                       INTERAC=.false., ZA=.false., DRYRUN=.false.,     &
                       FIXD=.false., ECR, SSEQ=.false., VS=.false.,             &
                       OUVS=.false., DSEQ, VD, OUVD=.false.
!      DATA KLE /'NNN', 'D:', 'EOF', 'SSEQ', 'DSEQ', 'VD', 'NOBOX',
!     X          'DIAG', 'ECR', 'I.', 'L.', 'K', 'M', 'T', 'C', 'SS',
!     X          'DS', 'V', 'N', 'VS', 'E', 'F', 'SF', 'DF',
!     X          'NRECMIN', 120*'S:','DRYRUN'/
!      DATA DEF1b/'OUI', ' ', '0', 'NON', 'NON', 'NON', 'NON', 'NON', 
!     X          'NON', '$IN', '$OUT', 'FATALE', 'ERRORS', 'FATALE',
!     X          '-1',  'NON', 'NON', 'NON', 'NON', 'NON', 'NON', 'OUI',
!     X          'NON', 'NON', '-1', 120*' ',' '/
!      DATA DEF2b/'OUI', ' ', '0', 'OUI', 'OUI', 'OUI', 'OUI', 'OUI', 
!     X          'OUI', '$IN', '$OUT', 'ERRORS', 'INFORM', 'ERRORS',
!     X          '-1',  'OUI', 'OUI', 'OUI', 'OUI', 'OUI', 'OUI', 'OUI',
!     X          'OUI', 'OUI', '-1', 120*' ','DRYRUN'/
!      DATA VS, OUVS, OUVD, FIXD, DEBUG, XPRES, SCRI, ESAIS, INTERAC, ZA, DRYRUN
!     X    /11 * .FALSE./
!      DATA JOURS, DESEXC, SATISF,  NS,    ETAT
!     X    /  4*0,  NMD*0,  NMD*0, ' ', 'NORMAL'/
!      DATA NREQ, NEXC, CEOF, SAUV, MEOF, LEOF, NFSO
!     X    /   0,    0,    0,    0,    1,    0,    0/
!      DATA NFS, NIS, NJS, NKS, GTYPS, IG1S, IG2S, IG3S, IG4S
!     X    /  0,  -1,  -1,  -1,   ' ',   -1,   -1,   -1,   -1/
contains
subroutine config_init
  req     = 0
  jours   = 0
  desexc  = 0
  satisf  = 0
  sources = -1
end subroutine config_init

end module configuration