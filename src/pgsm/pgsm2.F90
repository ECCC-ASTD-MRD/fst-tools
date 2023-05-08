!**programme pgsm
!        programme general de sortie des modeles
!               programme utilitaire d'interpolation horizontale
!               interpoler  ajustement convectif,precip,epaisseurs
!               calcul du vent sqrt(u**2 + v**2)
!               interpolation des vecteurs u et v horizontalement
!               interpolation des 3 niveaux de nuages
!               interpolation horizontale de variables scalaires
!
!auteur    - p.sarrazin octobre 1980 drpn dorval p.q.  canada
!
!revision
!        4.0.1  - conversion au fichier standard 89  p. sarrazin
!                 modification avril 90 p. sarrazin dorval canada drpn
!
!        4.0.2  - leger nettoyage du code
!               - conversion des variables contenant des informations
!                 alphanumeriques de type hollerith a caractere
!               - elimination des macros "hcar" et "lcar"
!               - conversion appels a "lexins" par "qlxins"
!               - conversion pour cyber-910
!                 y. chartier -juillet-aout 90- drpn dorval quebec
!
!        5.0    - utilisation des nouveaux interpolateurs
!                 y. chartier - mai 1991 - drpn dorval quebec
!
!        5.1    - utilisation de fichiers d'entree lies avec "fstlnk"
!                 y. chartier - mai 1991 - drpn dorval quebec
!
!        5.2    - support des grilles source de type z
!        5.3    - conversion de RATFOR a FORTRAN
!                 Y. Chartier - aout 1995
!        5.4    - Optimisation des interpolateurs pour l'extension
!                 selective des grilles.
!        5.5    - Interpolation a partir de fichiers d'entree SQI
!                 Support des grilles Lambert
!                 Introduction de la librairie C gctpc
!        5.6    - Introduction des directives COORD et GRILLE(STATIONS)
!                 sortie(ASCII)
!        5.7    - Support des fichiers standards 98
!        6.0    - Introduction de ezscint comme interpolateur principal
!        6.8    - Support des grilles diese
!        6.9    - Support des grilles T (stereographiques generalisees)
!        7.8.2  - Reload avec librmn_015.1
!        7.8.3  - D. Bouhmemhem, Fev 2015, Reload avec librmn_015.2
!        7.8.4  - M. Valin, Avril 2015,fixed data statements, include version.inc
!        7.8.5  - M. Lepine, Nov 2015, verification des codes de retour de fnom
!
!langage   - fortran
!
!objet(pgsm)
!          interpolateur horizontal qui permet de faire des
!          interpolations (cubique,lineaire,voisin) d'une grille a
!          une autre ou d'une grille a un point
!          permet de faire des operations sur deux champs
!          (gros calculateur de poche)
!          fichier d'entre doit-etre format standard random
!          sorti standard random ou sequentiel - seq ms - random ms
!
!librairies
!          - rmnxlib.a
!
!fichiers
!         - tape1  - fichier d'entree  (standard)
!         - tape2  - fichier de sortie standard..direct(writms)...sequentiel
!         - tape3  - fichier de records positionels ('^^','>>')
!         - tape5  - fichier d'entree(directives)
!         - tape6  - fichier de sortie sur imprimante
!
!----------------------------------------------------------------------------
#include "defin.cdk90"
    subroutine pgsm
       use app
       implicit none
       
#include "fst-tools_build_info.h"
!
#include "lnkflds.cdk90"
#include "dates.cdk90"
#include "charac.cdk90"
#include "pairs.cdk90"
#include "accum.cdk90"
#include "chck.cdk90"
#include "grilles.cdk90"
#include "heures.cdk90"
#include "symnom.cdk90"
#include "llccmm.cdk90"
#include "convers.cdk90"
#include "champs.cdk90"
#include "lires.cdk90"
#include "ecrires.cdk90"
#include "enrege.cdk90"
#include "packin.cdk90"
#include "indptr.cdk90"
#include "voir.cdk90"
#include "nivos.cdk90"
#include "gdz.cdk90"
#include "champseq.cdk90"
#include "styles.cdk90"

! Source on C-Fortran Interop
!    https://gcc.gnu.org/onlinedocs/gfortran/Interoperability-with-C.html
! following snippet from
!    https://stackoverflow.com/questions/17845931/calling-c-function-subroutine-in-fortran-code
INTERFACE
SUBROUTINE chk_tmpdir() BIND(C)
END SUBROUTINE chk_tmpdir
END INTERFACE

    character *8 qlxcon(128),qlxlcon(4)
    integer      qlxval(128)
    integer      qlxlval(4)

    integer ezsetopt
    external ezsetopt, heure, champ, sorti, grille2, metsym, cmetsym, convs
    external    qqqintx, setxtrap, liren, lirsr, plmnmod, pluss
    external moinse, moinss, ecrits,moyene, operat, modul2e, modul2s
    external expon, racine,alogn, absolu, carre, outlalo, foise, foiss, divisee, divises, pgcoupe
    external moysrt, imprims,chmpdif, pairvct, messags, champ_seq,qqqecho
    external qqqform,qqqident,coord,qqqfilt

    external ccard,fnom,qlxins,qlxinx,readlx,fstfrm,fstvoi
    external fstnbr,fstunl,fstouv
    external fclos,lrsmde,lrsmds,fstopc,fstopl,qlxopt

    integer fnom,fstfrm,fstvoi,fstnbr,fstopc,fstopl, fstouv
    integer i,iopc,ipose,kend,nequiv,npex,nsetin,nsetex,nlirmds,nlirmde
    real dum
        integer, parameter :: str_A=transfer("A   ",1)
        integer, parameter :: str_P=transfer("P   ",1)
        integer, parameter :: str_GZ=transfer("GZ  ",1)
        integer, parameter :: str_TT=transfer("TT  ",1)
        integer, parameter :: str_QQ=transfer("QQ  ",1)
        integer, parameter :: str_QR=transfer("QR  ",1)
        integer, parameter :: str_DD=transfer("DD  ",1)
        integer, parameter :: str_PP=transfer("PP  ",1)
        integer, parameter :: str_CC=transfer("CC  ",1)
        integer, parameter :: str_WW=transfer("WW  ",1)
        integer, parameter :: str_ES=transfer("ES  ",1)
        integer, parameter :: str_DFGZ=transfer("DFGZ",1)
        integer, parameter :: str_DFST=transfer("DFST",1)
        integer, parameter :: str_DFPR=transfer("DFPR",1)
        integer, parameter :: str_UV=transfer("UV  ",1)
        integer, parameter :: str_VENT=transfer("VENT",1)
        integer, parameter :: str_NUAG=transfer("NUAG",1)
        integer, parameter :: str_F2=transfer("F2  ",1)
        integer, parameter :: str_PN=transfer("PN  ",1)
        integer, parameter :: str_P0=transfer("P0  ",1)
        integer, parameter :: str_TS=transfer("TS  ",1)
        integer, parameter :: str_TM=transfer("TM  ",1)
        integer, parameter :: str_MT=transfer("MT  ",1)
        integer, parameter :: str_WDUV=transfer("WDUV",1)

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    data listl/981*'IMENT:','OZSRT:','ISLL:','I.',    'L.',     'DATE.','MSGLVL.','ISENT:','IMPOS:','V'/
    data defo /981*'SCRAP', 'TAPE2', 'TAPE4','$INPUT','$OUTPUT','OPRUN','INFO   ','ISENT_SCRAP','IMPOS_SCRAP','OUI'/
    data lfn  /981*'SCRAP', 'TAPE2', 'TAPE4','$INPUT','$OUTPUT','NON',  'INFO   ','ISENT_SCRAP','IMPOS_SCRAP','NON'/

        data form/'(A8)'/

    data nheure,  heures, nnoms,  npack,  nhur, nomb, ichck         /0,  MXHEURE*-2, 0,      -16,    1,    0,    0/

    data nomss /256*'  '/

    data ecarts,     facts,     pose,     ixlat, ixlon       /256*0.0, 256*1.0, .false., 0, 0 /

       data nchamp,  ngr,  nsort,   nchmp,   icnt, nlalo         /  1,    0,     0,        1,      0,     0 /

    data valid, voire,   voirs, message,seldat       /.false.,.false., .false.,.true.,.false.  /

    data numero,  numdel,  iset,  nbrow,  ip4        / 1,           1,    -2,      0,    0    /

    data paire(1) /  'VENT    UU  VV  UV      ' /
    data paire(2) /  'UV      UU  VV  ??      ' /
    data paire(3) /  'VENTUVS US  VS  UV      ' /
    data paire(4) /  'UVS     US  VS  ??      ' /
    data paire(5) /  'WDUV    UU  VV  UV  WD  ' /
    data paire(6) /  'WDUD    UD  VD  UV  WD  ' /
    data paire(7) /  '!#@$!#@$>>  ^^  >>  ^^  ' /

    data unefois,once,vvent/.false.,.false.,.false./

    data cnomqq, cnomqr, cnommt /'QQ', 'QR', 'MT'/

    data printen,printsr,mtdone/.false.,.false.,.false./

     data nis,njs,nif,njf,ninc,njnc,if9/1,1,1000,1000,10,10,0/

      data niis,njjs,niif,njjf,niinc,njjnc/1,1,1000,1000,10,10/

      data if7,if8,npairuv,npair/0,0,4,7/

      data clatmin,clatmax,clonmin,clonmax,ncoords/-90.0, +90.0, 0.0, 360.0, 0/

    data qlxcon( 1) /'ZON'     /  qlxval( 1) /      1 /
    data qlxcon( 2) /'MER'     /  qlxval( 2) /      2 /
    data qlxcon( 3) /'TOUT'    /  qlxval( 3) /     -1 /
    data qlxcon( 4) /'ALL'     /  qlxval( 4) /     -1 /
    data qlxcon( 5) /'COMTEUR' /  qlxval( 5) /   4095 /
    data qlxcon( 6) /'IMPRIM'  /  qlxval( 6) / 999999 /
    data qlxcon( 7) /'STD'     /  qlxval( 7) /   gr_a /
    data qlxcon( 8) /'MS'      /  qlxval( 8) /      2 /
    data qlxcon( 9) /'SEQ'     /  qlxval( 9) /      3 /
    data qlxcon(10) /'R'       /  qlxval(10) /      1 /
    data qlxcon(11) /'A'       /  qlxval(11) /     -1 /
    data qlxcon(12) /'NORD'    /  qlxval(12) /      1 /
    data qlxcon(13) /'SUD'     /  qlxval(13) /      2 /
    data qlxcon(14) /'GLOBAL'  /  qlxval(14) /      0 /
    data qlxcon(15) /'LATLON'  /  qlxval(15) / gr_latlon /
    data qlxcon(16) /'PS'      /  qlxval(16) / gr_ps   /
    data qlxcon(17) /'TAPE4'   /  qlxval(17) / gr_tape4 /
    data qlxcon(18) /'GAUSS'   /  qlxval(18) / gr_g /
    data qlxcon(19) /'STDB'    /  qlxval(19) / gr_b /
    data qlxcon(20) /'TAPE1'   /  qlxval(20) / gr_tape1 /
    data qlxcon(21) /'TAPE2'   /  qlxval(21) / gr_tape2 /
    data qlxcon(22) /'XYLIS'   /  qlxval(22) / gr_xylis /
    data qlxcon(23) /'XYDIR'   /  qlxval(23) / gr_xydir /
    data qlxcon(24) /'LLDIR'   /  qlxval(24) / gr_lldir /
    data qlxcon(25) /'LLLIST'  /  qlxval(25) / gr_lllist/
    data qlxcon(26) /'ANAL'    /  qlxval(26) / str_A/
    data qlxcon(27) /'PREV'    /  qlxval(27) / str_P   /
    data qlxcon(28) /'Z'       /  qlxval(28) / str_GZ  /
    data qlxcon(29) /'T'       /  qlxval(29) / str_TT  /
    data qlxcon(30) /'Q'       /  qlxval(30) / str_QQ  /
    data qlxcon(31) /'QR'      /  qlxval(31) / str_QR  /
    data qlxcon(32) /'D'       /  qlxval(32) / str_DD  /
    data qlxcon(33) /'PP'      /  qlxval(33) / str_PP  /
    data qlxcon(34) /'CC'      /  qlxval(34) / str_CC  /
    data qlxcon(35) /'W'       /  qlxval(35) / str_WW  /
    data qlxcon(36) /'ES'      /  qlxval(36) / str_ES  /
    data qlxcon(37) /'EPAIS'   /  qlxval(37) / str_DFGZ/
    data qlxcon(38) /'MAC'     /  qlxval(38) / str_DFST/
    data qlxcon(39) /'PCP'     /  qlxval(39) / str_DFPR/
    data qlxcon(40) /'UV'      /  qlxval(40) / str_UV  /
    data qlxcon(41) /'VENT'    /  qlxval(41) / str_VENT/
    data qlxcon(42) /'NUAGES'  /  qlxval(42) / str_NUAG/
    data qlxcon(43) /'ECM'     /  qlxval(43) / str_F2  /
    data qlxcon(44) /'PNM'     /  qlxval(44) / str_PN  /
    data qlxcon(45) /'PSURF'   /  qlxval(45) / str_P0  /
    data qlxcon(46) /'TSRF'    /  qlxval(46) / str_TS  /
    data qlxcon(47) /'TMER'    /  qlxval(47) / str_TM  /
    data qlxcon(48) /'MT'      /  qlxval(48) / str_MT  /
    data qlxcon(49) /'VOISIN'  /  qlxval(49) /   100 /
    data qlxcon(50) /'LINEAIR' /  qlxval(50) /     1 /
    data qlxcon(51) /'CUBIQUE' /  qlxval(51) /     3 /
    data qlxcon(52) /'ABORT'   /  qlxval(52) /    13 /
    data qlxcon(53) /'MINIMUM' /  qlxval(53) /     5 /
    data qlxcon(54) /'MAXIMUM' /  qlxval(54) /     4 /
    data qlxcon(55) /'GEF'     /  qlxval(55) / gr_gem /
    data qlxcon(56) /'GEM'     /  qlxval(56) / gr_gef /
    data qlxcon(57) /'WAIT'    /  qlxval(57) /     0 /
    data qlxcon(58) /'GO'      /  qlxval(58) /     1 /
    data qlxcon(59) /'GRIB'    /  qlxval(59) / gr_grib/
    data qlxcon(60) /'FORMATEE'/  qlxval(60) /     5 /
    data qlxcon(61) /'STATIONS'/  qlxval(61) / gr_stations /
    data qlxcon(62) /'ADD'     /  qlxval(62) /     1 /
    data qlxcon(63) /'RESET'   /  qlxval(63) /     0 /
    data qlxcon(64) /'EST'     /  qlxval(64) /     3 /
    data qlxcon(65) /'OUEST'   /  qlxval(65) /     4 /
    data qlxcon(66) /'NONE'    /  qlxval(66) /     5 /
    data qlxcon(67) /'NOMVAR'  /  qlxval(67) /     1 /
    data qlxcon(68) /'TYPVAR'  /  qlxval(68) /     2 /
    data qlxcon(69) /'ETIKET'  /  qlxval(69) /     3 /
    data qlxcon(70) /'IP01'    /  qlxval(70) /     4 /
    data qlxcon(71) /'IP02'    /  qlxval(71) /     5 /
    data qlxcon(72) /'IP03'    /  qlxval(72) /     6 /
    data qlxcon(73) /'DATEO'   /  qlxval(73) /     7 /
    data qlxcon(74) /'DATEV'   /  qlxval(74) /     8 /
    data qlxcon(75) /'LAT'     /  qlxval(75) /    12 /
    data qlxcon(76) /'LON'     /  qlxval(76) /    13 /
    data qlxcon(77) /'NI'      /  qlxval(77) /     9 /
    data qlxcon(78) /'NJ'      /  qlxval(78) /    10 /
    data qlxcon(79) /'NK'      /  qlxval(79) /    11 /
    data qlxcon(80) /'WDUV'    /  qlxval(80) /str_WDUV/
    data qlxcon(81) /'ON'      /  qlxval(81) /     1 /
    data qlxcon(82) /'OFF'     /  qlxval(82) /     0 /
    data qlxcon(83) /'VERBOSE' /  qlxval(83) /     1 /
    data qlxcon(84) /'LECTURE' /  qlxval(84) /     1 /
    data qlxcon(85) /'ECRITURE'/  qlxval(85) /     2 /
    data qlxcon(86) /'SEQWPRM' /  qlxval(86) /     4 /
    data qlxcon(87) /'AUCUNE'  /  qlxval(87) /     0 /
    data qlxcon(88) /'COMME'   /  qlxval(88) / gr_comme /
    data qlxcon(89) /'LIKE'    /  qlxval(89) /    12 /
    data qlxcon(90) /'IP1A'    /  qlxval(90) / 65001 /
    data qlxcon(91) /'IP1B'    /  qlxval(91) / 65002 /
    data qlxcon(92) /'IP2A'    /  qlxval(92) / 65003 /
    data qlxcon(93) /'IP2B'    /  qlxval(93) / 65004 /
    data qlxcon(94) /'IP3A'    /  qlxval(94) / 65005 /
    data qlxcon(95) /'IP3B'    /  qlxval(95) / 65006 /
    data qlxcon(96) /'FENTREE' /  qlxval(96) /     1 /
    data qlxcon(97) /'FSORTIE' /  qlxval(97) /     2 /
    data qlxcon(98) /'LOCAL'   /  qlxval(98) /     1 /
    data qlxcon(99) /'IP1'     /  qlxval(99) /     4 /
    data qlxcon(100)/'IP3'     /  qlxval(100)/     6 /
    data qlxcon(101)/'STEREO'  /  qlxval(101)/ gr_stereo /
    data qlxcon(102)/'IPUN'    /  qlxval(102)/     4 /
    data qlxcon(103)/'IPDEUX'  /  qlxval(103)/     5 /
    data qlxcon(104)/'IPTROIS' /  qlxval(104)/     6 /
    data qlxcon(105)/'IPONE'   /  qlxval(105)/     4 /
    data qlxcon(106)/'IPTWO'   /  qlxval(106)/     5 /
    data qlxcon(107)/'IPTHREE' /  qlxval(107)/     6 /

!               KIND =0, p est en hauteur (m) par rapport au niveau de la mer
!               KIND =1, p est en sigma (0.0 -> 1.0)
!               KIND =2, p est en pression (mb)
!               KIND =3, p est un code arbitraire
!               KIND =4, p est en hauteur (M) par rapport au niveau du sol
!               KIND =5, p est en coordonnee hybride
!               KIND =6, p est en coordonnee theta
!               KIND =15, rererve (entiers)
!               KIND =21, p est en GalChen


    data qlxcon(108)/'METERS'  /  qlxval(108)/ -1000 /
    data qlxcon(109)/'SIGMA'   /  qlxval(109)/ -1001 /
    data qlxcon(110)/'MBAR'    /  qlxval(110)/ -1002 /
    data qlxcon(111)/'OTHER'   /  qlxval(111)/ -1003 /
    data qlxcon(112)/'METERAGL'/  qlxval(112)/ -1004 /
    data qlxcon(113)/'HYBRID'  /  qlxval(113)/ -1005 /
    data qlxcon(114)/'THETA'   /  qlxval(114)/ -1006 /
    data qlxcon(115)/'GALCHEN' /  qlxval(115)/ -1021 /
    data qlxcon(116)/'OLDSTYLE'/  qlxval(116)/ 3 /
    data qlxcon(117)/'NEWSTYLE'/  qlxval(117)/ 2 /

    data qlxcon(118)/'STAMP'   /  qlxval(118)/ 0 /
    data qlxcon(119)/'YMDHMS'  /  qlxval(119)/ 1 /
    data qlxcon(120)/'ISO8601' /  qlxval(120)/ 2 /
        data qlxcon(121)/'FAST'    /  qlxval(121)/ 1 /
        data qlxcon(122)/'BEST'    /  qlxval(122)/ 2 /
        data qlxcon(123)/'MOYENNE' /  qlxval(123)/ 4 /
        data qlxcon(124)/'GRIDAVG' /  qlxval(124)/ 4 /
        data qlxcon(125)/'SPHRAVG' /  qlxval(125)/ 5 /
        data qlxcon(126)/'EXCLUDE' /  qlxval(126)/ 31/
        data qlxcon(127)/'ORIGIN'  /  qlxval(127)/ 1023 /
        data qlxcon(128)/'RESV128' /  qlxval(128)/ 0 /

    data(qlxlcon(i),i=1,2)/'OUI', 'NON'/
    data(qlxlval(i),i=1,2)/1,0/

!        integer idx_ozsrt, idx_isll, idx_i, idx_l, idx_date, idx_msglvl, idx_isent, idx_impos, idx_v
   data idx_ozsrt  /982/  idx_isll  /983/  idx_i      /984/ idx_l /985/ idx_date /986/
   data idx_msglvl /987/  idx_isent /988/  idx_impos  /989/ idx_v /990/
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!           listl=position  iment(tape1 standard),isll(tape4 sequentiel)
!                 ozsrt(tape2 - standard - seq file - random ms)
!           defo=liste des defauts pour iment,isll,ozsrt,i,l
!           lfn=liste que l usager propose pour remplacer
!           6=nombre de lfn
!           nequiv=nombre d'equivalence output de ccard


    nequiv = -1
    lnkdiun = 0
    lnkdiun(1) = 1
    lnkdiun(idx_ozsrt) = 2
    call ccard(listl,defo,lfn,990,nequiv)
    ier = fnom(5,lfn(idx_i),'SEQ',0)
    ier = fnom(6,lfn(idx_l),'SEQ',0)


    ! imprime boite debut du programme
    app_ptr=app_init(0,'pgsm',PGSM_VERSION,'',BUILD_TIMESTAMP)
    call app_start()



! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    call qqqfilt(1,0,0,0)
    call qqqfilt(2,0,0,0)
        call chk_tmpdir

    if (lfn(1)(1:5).ne.'SCRAP'.and.lfn(idx_isent)(1:11).ne.'ISENT_SCRAP') then
      call app_log(APP_ERROR,'Cannot mix sequential and random files')
      app_status=app_end(13)
      call qqexit(13)
    endif

    if (lfn(idx_isll).ne.'TAPE4') then
        iun_isll=0
        ier = fnom(iun_isll,lfn(idx_isll)(1:5),'FMT+SEQ+R/O',0)
        isll_input = 1
    endif

    if (lfn(idx_isent)(1:11).ne.'ISENT_SCRAP') then
      inputmod = SEQUENTIEL
    else
      inputmod = RANDOM
    endif

    if (lfn(idx_impos)(1:11).ne.'IMPOS_SCRAP') then
      ier = fnom(lnkdiun(idx_impos),lfn(idx_impos),'RND+OLD+R/O',0)
      ier = fstouv(lnkdiun(idx_impos),'RND')
    endif

    if (lfn(idx_v).ne.'NON') then
      ier = ezsetopt('verbose', 'yes')
    endif


    if (inputmod.eq.RANDOM) then
      niun = 1
 100      if (lfn(niun) .ne.'SCRAP') then
        niun = niun+1
        goto 100
      endif

      niun = niun - 1
      if (niun .lt. 1) then
        call app_log(APP_ERROR,'No input files given as arguments')
        app_status=app_end(13)
        call qqexit(13)
      endif
      do i=1, niun
        ier = fnom(lnkdiun(i),lfn(i),'STD+RND+OLD+R/O+REMOTE',0)
        if (ier .lt. 0) then
          call app_log(APP_ERROR,'Problem opening file '//lfn(i))
          app_status=app_end(13)
          call qqexit(13)
        endif
      enddo
    else
      niun = 1
      ier = fnom(lnkdiun(1),lfn(idx_isent),'STD+SEQ+OLD+R/O+REMOTE',0)
      if (ier .lt. 0) then
        call app_log(APP_ERROR,'Problem opening file '//lfn(idx_isent))
        app_status=app_end(13)
        call qqexit(13)
      endif
    endif




    mtype =   MTYP
    maxnoms = MAXNOM

    call initseq



!  initialise les dictionnaires

    call qlxopt ('CARMOT', 4)
    call qlxinx (sorti,'SORTIE', nsort,0103,2)
!                        3 appels reconnus  1=sortie(std,noenrg) noenrg>=2
!                                           2=sortie(ms,noenrg,jwrit)
!                                           3=sortie(seq)


    call qlxinx (heure,'HEURE',nheure, 0140,2)
    call qlxinx (heure, 'IP2',nheure, 0140,2)
!                     2 appels  1=heure(00,12,24,25.....max20) minimum 1
!                               2=champ(mac,00,06) minimum 2 pour
!                               2=champ(pcp,00,06) minimum 2 pour
!                               accumulateur d"ajustement ou precipitation

    call qlxinx (qqqintx,'SETINTX',nsetin, 0101,2)

!                        appel - setintx(voisin) avec le plus proche
!                                setintx(lineair) interpolation lineaire
!                                setintx(cubique) interpolation cubique(defaut)

    call qlxinx (setxtrap,'EXTRAP',nsetex, 0101,2)

!                        appel - setintx(voisin) avec le plus proche
!                                setintx(lineair) interpolation lineaire
!                                setintx(cubique) interpolation cubique(defaut)

    call qlxinx (champ,'CHAMP',nchamp, 0131,2)
!                     appel - champ(z,niveau)  niveau=1000,850,.......
!                           - champ(t,niveau)  niveau=1000,850,.......
!                           - champ(q,niveau)  niveau=1000,850,.......
!                           - champ(d,niveau)  niveau=1000,850,.......
!                           - champ(w,niveau)  niveau=1000,850,.......
!                           - champ(es,niveau)  niveau=1000,850,.......
!                           - champ(uv,niveau)  niveau=1000,850,.......
!                           - champ(uvs)  pas de niveau vent de surface
!                           - champs(ventuvs) voir directive paires(.....
!                           - champ(vent,niveau) niveau=1000,850,.......
!                           - champ(nuage)  nuage bas,moyen,haut
!                                   rec 1=bas  rec 2= moyen  rec 3=haut
!                           - champ(ecm)  epaisseur de la couche limite
!                           - champ(pnm)  pression au niveau de la mer
!                           - champ(psurf)  pression a la surface
!                           - champ(ts)  temperature a la surface
!                           - champ(epais,niveau1,niveau2) niveau2 - niveau1
!                           - champ(mac,heure1,heure2)  heure2 - heure1
!                           - champ(pcp,heure1,heure2)  heure2 - heure1

    call qlxinx (chmpdif,'CHMPDIF',npar,0508,2)

!                 appel - chmpdif (noment,nomsrt,ip1tab,ip2tab,ip3tab)
!                    ex:  chmpdif ("gz","dz",[1000,500],12,0)
!                         z500mb - z1000mb  a 12hr
!                         fichier de sorti aura ip1=1000, ip2=500,ip3=12
!                    ex:  chmpdif ("gz","dz",1000,[6,12,18,24],0)
!                         z1000mb 6hr - z1000mb  a 12hr
!                         fichier de sorti aura ip1=1000, ip2=6, ip3=12
!                    ex:  chmpdif ("gz","dz",1000,6,[1,2,3,4])
!                         z1000mb 6hr ip3=1 - z1000mb  6hr ip3=2
!                         fichier de sorti aura ip1=1000, ip2=1, ip3=2

    call qlxinx (champ_seq,'CHAMPSEQ',npar,0303,2)

!                 appel - champseq(['GZ','TT','UU'],[1000,850,500],WAIT)
!                 appel - champ_seq(' ',[1000,850,500],WAIT)
!                 appel - champ_seq(['GZ','TT','UU'],-1,GO)

    call qlxinx (convs, 'CONV',ncon, 0305,2)

!                 appel - conv(nom, ecart, facteur, bas, haut) directive
!                         conv("ts", -273.16, 1.0,-280.0, -250.0)
!                         routine conver dans ecriture soustrait
!                         273.16 au champ et multiplit par 1.0
!                         enleve toutes les valeurs plus petites que -280
!                         enleve toutes les valeurs plus grandes que -250
!                         avant d ecrire le champ
    call qlxinx (grille2,'GRILLE',  ngr, 0109,2)
!          8 appels a grille    1=grille(std,nni,nnj,lg1)
!                                 std=standard lat lon
!                                 nni=nombre de pts est-ouest
!                                 nnj=nombre de pts nord-sud
!                                 lg1=0  global
!                                    =1  hem nord
!                                    =2  hem sud
!                               2=grille(latlon,nni,nnj,lat0,lon0,dlat,dlon)
!                                 latlon=grille lat lon
!                                 nni= nombre de pts est-ouest
!                                 nnj= nombre de pts nord-sud
!                                 lat0=premiere lat du coin degree
!                                 lon0=premiere lon du coin degree
!                                 dlat=espacement entre latitude  (degree)
!                                 dlon=espacement entre longitude (degree)

!                               3=grille(ps,nni,nnj,pi,pj,d60,dgrw)
!                                 ps  =polaire stereographique
!                                 nni =nombre pts est-ouest (dir i)
!                                 nnj =nombre de pts nord-sud (dir j)
!                                 pi  =position du pole nord(pi=26)
!                                 pj  = position du pole nord(pj=28)
!                                 d60 =distance en metres entre les pts
!                                      a 60 degrees nord (latitude)
!                                 drgw=angle entre l"axe x et greewich

!                               4=grille(tape4,nni,nnj,ip1,ip2,ip3)
!                                 tape4=fichier contenant nni*nnj(lat-lon)
!                                 nni  =nombre de pts est-ouest
!                                 nnj  =nombre de pts nord-sud
!                                 ip1  =definit par usager
!                                 ip2  =definit par usager
!                                 ip3  =definit par usager

!                               5=grille(stdb,nni,nnj,hem)
!                                 stdb=standard b
!                                 nni  =nombre de pts est-ouest
!                                 nnj  =nombre de pts nord-sud
!                                 hem  =hemisphere 0=global
!                                                  1=nord
!                                                  2=sud

!                               6=grille(gauss,nni,nnj,hem)
!                                 gauss=grille gaussienne lat-lon
!                                 nni  =nombre de pts est-ouest
!                                 nnj  =nombre de pts nord-sud
!                                 hem  =hemisphere 0=global
!                                                  1=nord
!                                                  2=sud

!                               7=grille(tape1,ip1,ip2,ip3,ip4,nord/sud)
!                                 tape1=lit sur fichier 1 lat-lon ou xy
!                                 ip1=valeur 0-32767
!                                 ip2=valeur 0-32767
!                                 ip3=valeur 0-4095
!                                 ip4=valeur "xydir" ou "llist"
!                                    =valeur "lldir" ou "xylis"

!                               8=grille(tape2,ip1,ip2,ip3,ip4,nord/sud)
!                                 tape2 lit sur fichier 2 lat-lon ou xy
!                                 ip1=valeur 0-32767
!                                 ip2=valeur 0-32767
!                                 ip3=valeur 0-4095
!                                 ip4=valeur "xydir" ou "llist"
!                                    =valeur "lldir" ou "xylis"

    call qlxinx (lrsmde,'LIRMODE',nlirmde,0708,2)
    call qlxinx (lrsmds,'LIRMODS',nlirmds,0708,2)

!                  lrsmde(nomvar,typvar,date,niveau,heure,ip3,etiquet)
!                  lrsmds(nomvar,typvar,date,niveau,heure,ip3,etiquet)

    call qlxinx (metsym,'METSYM',  nsym, 0202,2)

!                               metsym(z,oui)
!                               z  =geopotentiel "gz"
!                               oui=symetrique

    call qlxinx (outlalo,'OUTLALO', nlalo, 0108,2)
!     outlalo(ip1,ip2,ip3,nomlat,nomlon,grtyp,etiklat,etiklon)
!             ip1=valeur 0-32767
!             ip2=valeur 0-32767
!             ip3=valeur 0-4095
!             nomlat=nom du champ de latitude 2 car
!             nomlon=nom du champ de longitude 2 car
!             grtyp=type de grille
!             etiklat=nom de l'etiquette latitude
!             etiklon=nom de l'etiquette longitude

    call qlxinx (pairvct, "PAIRES",npairuv, 0305,2)
!      ex: paires("uv","uu","vv",0) vecteur "uu","vv" geographique
!                                     niveau donne par champ
!      ex: paires("ventuvs","us","vs","uv") vitesse du vent a la surface
!      ex: paires("uvs","us","vs",0) vecteurs du vent a la surface

   call qlxinx (pgcoupe,'MOYENT', nmoy, 0232,2)
   call qlxinx (moysrt,'MOYSRT', nmoy, 0232,2)
   call qlxinx (liren,'LIREE', nlire, 0708,2)
    call qlxinx (lirsr,'LIRES', nlire, 0708,2)
    call qlxinx(plmnmod,'PLUSE', najou, 0707,2)
    call qlxinx (pluss,'PLUSS', najou, 0707,2)
    call qlxinx (foise,'FOISE', multp, 0707,2)
    call qlxinx (foiss,'FOISS', multp, 0707,2)
    call qlxinx (divisee,'DIVE', multp, 0707,2)
    call qlxinx (divises,'DIVS', multp, 0707,2)
    call qlxinx (moinse,'MOINSE', nenle, 0707,2)
    call qlxinx (moinss,'MOINSS', nenle, 0707,2)
    call qlxinx (moyene,'MOYENE', nmoys, 0101,2)
   call qlxinx (ecrits,'ECRITS',  necrt, 0814,2)
   call qlxinx (modul2e,'MODUL2E', nmod, 0707,2)
   call qlxinx (modul2s,'MODUL2S', nmod, 0707,2)
   call qlxinx (racine,'RACINE', nraci, 0101,2)
   call qlxinx (operat, 'PFOIS', npfo, 0303,2)
   call qlxinx (expon, 'EXPON', npex, 0101,2)
   call qlxinx (alogn, 'ALOGN', npex, 0101,2)
   call qlxinx (absolu,  'ABSOLU', npex, 0101,2)
   call qlxinx (carre, 'CARRE', npex, 0101,2)

    call qlxinx (qqqecho, 'ECHO',   dum, 0101, 2)
    call qlxinx (qqqident,'IDENT',  npar, 0103, 2)
    call qlxinx (qqqform, 'FORMAT', dum, 0101, 2)
    call qlxinx (coord,   'COORD',  dum, 0202, 2)
    call qlxinx (qqqfilt, 'FILTRE', dum, 0204, 2)

      call qlxins (npack,  'COMPAC',  dum, 1, 1)
    call qlxins (message,'MESSAGE', dum, 1, 1)
    call qlxins (numdel, 'DELTA',   dum, 1, 1)
    call qlxins (typeent,'TYPEENT', dum, 1, 1)
    call qlxins (typesrt,'TYPESRT', dum, 1, 1)
    call qlxins ( voire, 'VOIRENT', dum, 1, 1)
    call qlxins ( voirs, 'VOIRSRT', dum, 1, 1)
   call qlxins ( pose,  'PAUSE',   dum, 1, 1)
    call qlxins ( userdate,  'DATE',    dum, 3, 1)
    call qlxins (seldat, 'OPDAT',   dum, 1, 1)
    call qlxins (printen,'PRINTEN', dum,7, 1)
    call qlxins (printsr,'PRINTSR', dum,7, 1)
    call qlxins (etikent,'ETIKENT', nwetike, 3, 1)
   call qlxins (masque, 'MASQUE',  dum, 1, 1)
    call qlxins (etiksrt,'ETIKSRT', nwetiks, 3, 1)
    call qlxins (numero, 'ENREG',   dum, 1, 1)
    call qlxins (ip2srt, 'IP2SRT',  dum, 1, 1)
    call qlxins (ip3ent, 'IP3ENT',  dum, 1, 1)
    call qlxins (ip3srt, 'IP3SRT',  dum, 1, 1)
    call qlxins (unefois,'UNEFOIS',  dum, 1, 1)
    call qlxins (once,   'ONCE',  dum, 1, 1)
    call qlxins (diese,  'DIESE',dum,1,1)
    call qlxins (ip1style, 'IP1STYLE', dum, 1, 1)
    call qlxins (dateform, 'DATEFORM', dum, 1, 1)
   call qlxins (compression_level, 'COMPRESS', dum, 1, 1)

    do i=1,127
       call qlxins(qlxval(i), qlxcon(i), dum, 1, 0)
    enddo

    do i=1,2
       call qlxins(qlxlval(i), qlxlcon(i), dum, 1, 0)
    enddo

!   defaut pour lire fichier d'entre

    typeent = -1
    etikent(1) = -1
    etikent(2) = -1

    ip3ent = -1
    userdate  = -1
    date2 = -1
    date3 = -1

    diese = 1
    ip1style = 2
    dateform = 1

!   defaut pour fichier de sorti

    ip3srt= -1
    ip2srt=-1
    etiksrt(1) = -1
    etiksrt(2) = -1
    etiksrt(3) = -1

    typesrt= -1
   compression_level = 0
   masque = 0


!    initialiser avec .true. champ symetrique

    nsym = 2
    call cmetsym('GZ',.true.)
    call cmetsym('TT',.true.)
    call cmetsym('DD',.true.)
    call cmetsym('WW',.true.)
    call cmetsym('ES',.true.)
    call cmetsym('F2',.true.)
    call cmetsym('PN',.true.)
    call cmetsym('PS',.true.)
    call cmetsym('TS',.true.)

    call cmetsym('QQ',.false.)


!    directives de l'usager



!    initialisation parametres de sortie pour fichier formate

    call initid


    iopc= app_loglevel(lfn(idx_msglvl))
    ier = fstopl('REDUCTION32',.true.,.false.)

    ipose= 0
    call readlx(5,kend,ipose)

!   initialise variable de printsr

    if (associated(tmplat)) deallocate(tmplat)
    if (associated(tmplon)) deallocate(tmplon)

    if (mode.eq.1) then
      call chk_hy(lnkdiun(1),lnkdiun(idx_ozsrt))
      call chk_toctoc(lnkdiun(1),lnkdiun(idx_ozsrt))
   endif
    iopc= app_loglevel('INFO')
    do i=1,niun
       ier = fstfrm(lnkdiun(i))
       call fclos(lnkdiun(i))
    enddo
!    call fstunl

    if (mode.eq.1) then
       if (voirs)  then
          if (message) then
             ier = fstvoi(lnkdiun(idx_ozsrt), 'RND')
          endif
       endif

      ier = fstfrm(lnkdiun(idx_ozsrt))
       call fclos(lnkdiun(idx_ozsrt))
    else
#if defined (unix)
         if (mode.eq.2) then
            call app_log(APP_WARNING,'"MS" Nfile type are not supported in this version of PGSM')
       endif
#endif

!    fermer fichier sequentiel

            if (mode.eq.3)  then
          call fclos(lnkdiun(idx_ozsrt))
       endif

            if (mode.eq.4)  then
          call pgsmcf(lnkdiun(idx_ozsrt))
       endif
    endif



!  fermer fichier 4 dans grille


!  imprime boite avec le temps d execution du pgm  pgsm

    if (ipose.gt.0) then
        app_status=app_end(13)
        call qqexit(13)
    else
       app_status=app_end(0)
    endif
    
    end
