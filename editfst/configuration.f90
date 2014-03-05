module configuration
!     internal variables used all over editfst
!
      implicit none
      integer, private :: i
      integer, PARAMETER :: NMR=12, NMS=25, NME=20, NMN=40, NMM=10, NMD=20
      character(len=*), parameter :: LIN128 = '(32A4)'
!
! NOTE:
!     le nombre maximum de valeurs dans une liste de desiderata
!     est limite (hardcoded) a 10 ( a corriger eventuellement)
!
      character *1   , save :: GTY, GTYPS=' ', GTYS(NMD)
      character *2   , save :: ZT, TYP, TYPS(10,NMD)
      character *4   , save :: ZN, NOM, NOMS(10,NMD)
      character *6   , save :: ETAT='NORMAL'
      character *12  , save :: ZE, ETI, ETIS(10,NMD)
      character *128 , save :: NS=' ', ND
      character *15  , save :: SNOM, DNOM

      integer, parameter :: NCCARDKEYS=146   ! dimension for program options (processed by ccard)
      character(len=8), dimension(NCCARDKEYS), save ::    &
      kle = (/ 'NNN     ', 'D:      ', 'EOF     ', 'SSEQ    ', 'DSEQ    ', 'VD      ', &
               'NOBOX   ', 'DIAG    ', 'ECR     ', 'I.      ', 'L.      ', 'K       ', &
               'M       ', 'T       ', 'C       ', 'SS      ', 'DS      ', 'V       ', &
               'N       ', 'VS      ', 'E       ', 'F       ', 'SF      ', 'DF      ', &
               'NRECMIN ',('S:      ',i = 1,120),  'DRYRUN  ' /),                      &
      def1b=(/ 'OUI     ', '        ', '0       ', 'NON     ', 'NON     ', 'NON     ', &
               'NON     ', 'NON     ', 'NON     ', '$IN     ', '$OUT    ', 'FATALE  ', &
               'ERRORS  ', 'FATALE  ', '-1      ', 'NON     ', 'NON     ', 'NON     ', &
               'NON     ', 'NON     ', 'NON     ', 'OUI     ', 'NON     ', 'NON     ', &
               '-1      ',('        ',i = 1,120),  '        '/),                       &
      def2b=(/ 'OUI     ', '        ', '0       ', 'OUI     ', 'OUI     ', 'OUI     ', &
               'OUI     ', 'OUI     ', 'OUI     ', '$IN     ', '$OUT    ', 'ERRORS  ', &
               'INFORM  ', 'ERRORS  ', '-1      ', 'OUI     ', 'OUI     ', 'OUI     ', &
               'OUI     ', 'OUI     ', 'OUI     ', 'OUI     ', 'OUI     ', 'OUI     ', &
               '-1      ',('        ',i = 1,120),  'DRYRUN  '/)
! table utilisee pour stocker les requetes
      integer*8, save :: JOURS(4),REQ(11,4,NMD)
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

      integer, save ::  NREQ=0, SAUV=0, DESEXC(NMD), SATISF(NMD),                    &
                        NEXC=0, SUP(8,NMD), NIS=-1, NJS=-1, NKS=-1,                  &
                        IG1S=-1, IG2S=-1, IG3S=-1, IG4S=-1, REQN(NMD), REQT(NMD),    &
                        REQE(NMD), Z1, Z2, Z3, ZD
      integer, save ::  NP
      integer, save ::  MEOF=1, COPIES, NDS, NDD, EOF, CEOF=0, LEOF=0, LIMITE, NFS=0,  NFSO=0,   SOURCES(120), NRECMIN

      logical, save :: SCRI=.false., XPRES=.false., ESAIS=.false.,      &
                       DM1, DEBUG=.false., SELEC, BOX, DIAG,            &
                       INTERAC=.false., ZA=.false., DRYRUN=.false.,     &
                       FIXD=.false., ECR, SSEQ=.false., VS=.false.,     &
                       OUVS=.false., DSEQ, VD, OUVD=.false.
contains
subroutine config_init  ! initialisation des tableaux 
  req     = 0
  jours   = 0
  desexc  = 0
  satisf  = 0
  sources = -1
end subroutine config_init

end module configuration