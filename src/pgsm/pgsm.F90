!               interpoler  ajustement convectif, precip, epaisseurs
!               calcul du vent sqrt(u**2 + v**2)
!               interpolation des vecteurs u et v horizontalement
!               interpolation des 3 niveaux de nuages
!               interpolation horizontale de variables scalaires

!          interpolateur horizontal qui permet de faire des
!          interpolations (cubique, lineaire, voisin) d'une grille a
!          une autre ou d'une grille a un point
!          permet de faire des operations sur deux champs
!          (gros calculateur de poche)
!          fichier d'entre doit-etre format standard random
!          sorti standard random ou sequentiel - seq ms - random ms

!fichiers
!         - tape1  - fichier d'entree  (standard)
!         - tape2  - fichier de sortie standard..direct(writms)...sequentiel
!         - tape3  - fichier de records positionels ('^^', '>>')
!         - tape5  - fichier d'entree(directives)
!         - tape6  - fichier de sortie sur imprimante

!> Utilitaire d'interpolation horizontale
PROGRAM pgsm
    use app
    use rmn_fst24
    use files
    use packing, only : npack
    use pgsm_mod
    use accum, only : unefois, once
    use grilles, only : gr_a, gr_b, gr_g, gr_latlon, gr_ps, gr_tape1, gr_tape2, gr_tape4, gr_xylis, gr_xydir, gr_lldir, &
        gr_lllist, gr_gem, gr_gef, gr_grib, gr_stations, gr_comme, gr_stereo, masque, ngr
    use nivos, only : nmoy
    use heuress, only : nheure
    use ecrires
    use chck
    use symetry, only : metsym
    use champs, only : nchamp, npar
    use pairs, only : npairuv
    use convers, only : ncon
    use champseq, only : wait, go
    implicit none

#include "fst-tools_build_info.h"

    ! Source on C-Fortran Interop
    !    https://gcc.gnu.org/onlinedocs/gfortran/Interoperability-with-C.html
    ! following snippet from
    !    https://stackoverflow.com/questions/17845931/calling-c-function-subroutine-in-fortran-code


    INTERFACE
        SUBROUTINE chk_tmpdir() BIND(C)
        END SUBROUTINE chk_tmpdir
    END INTERFACE

    integer, parameter :: nbKeys = 128

    character(len = 8), dimension(nbKeys) :: qlxcon
    character(len = 8), dimension(4) :: qlxlcon
    integer, dimension(nbKeys) :: qlxval
    integer, dimension(4) :: qlxlval

    integer, external :: ezsetopt
    external heure, champ, sorti, grille2, convs
    external qqqintx, setxtrap, liren, lirsr, plmnmod, pluss
    external moinse, moinss, ecrits, moyene, operat, modul2e, modul2s
    external expon, racine, alogn, absolu, carre, outlalo, foise, foiss, divisee, divises, pgcoupe
    external moysrt, imprims, chmpdif, pairvct, messags, champ_seq, qqqecho
    external qqqform, qqqident, coord, qqqfilt

    external ccard, qlxins, qlxinx, readlx
    external lrsmde, lrsmds, qlxopt

    integer, external :: fstopc, fstopl
    integer, external :: fnom
    integer :: i, iopc, ipose, kend, nequiv, npex, nsetin, nsetex, nlirmds, nlirmde
    real :: dum
    integer :: idum
    logical :: ldum

    integer, parameter :: str_A = transfer("A   ", 1)
    integer, parameter :: str_P = transfer("P   ", 1)
    integer, parameter :: str_GZ = transfer("GZ  ", 1)
    integer, parameter :: str_TT = transfer("TT  ", 1)
    integer, parameter :: str_QQ = transfer("QQ  ", 1)
    integer, parameter :: str_QR = transfer("QR  ", 1)
    integer, parameter :: str_DD = transfer("DD  ", 1)
    integer, parameter :: str_PP = transfer("PP  ", 1)
    integer, parameter :: str_CC = transfer("CC  ", 1)
    integer, parameter :: str_WW = transfer("WW  ", 1)
    integer, parameter :: str_ES = transfer("ES  ", 1)
    integer, parameter :: str_DFGZ = transfer("DFGZ", 1)
    integer, parameter :: str_DFST = transfer("DFST", 1)
    integer, parameter :: str_DFPR = transfer("DFPR", 1)
    integer, parameter :: str_UV = transfer("UV  ", 1)
    integer, parameter :: str_VENT = transfer("VENT", 1)
    integer, parameter :: str_NUAG = transfer("NUAG", 1)
    integer, parameter :: str_F2 = transfer("F2  ", 1)
    integer, parameter :: str_PN = transfer("PN  ", 1)
    integer, parameter :: str_P0 = transfer("P0  ", 1)
    integer, parameter :: str_TS = transfer("TS  ", 1)
    integer, parameter :: str_TM = transfer("TM  ", 1)
    integer, parameter :: str_MT = transfer("MT  ", 1)
    integer, parameter :: str_WDUV = transfer("WDUV", 1)

    integer, parameter :: nentries = 988
    character(len = opt_len) :: defo(nentries)
    character(len = opt_len) :: listl(nentries)
    character(len = opt_len) :: lfn(nentries)

    integer, parameter :: idx_ozsrt = 982
    integer, parameter :: idx_isll = 983
    integer, parameter :: idx_i = 984
    integer, parameter :: idx_log = 985
    integer, parameter :: idx_msglvl = 986
    integer, parameter :: idx_isent = 987
    integer, parameter :: idx_verbose = 988

    data listl /981*'IMENT:', 'OZSRT:', 'ISLL:',     'I.',      'L.', 'MSGLVL.',      'ISENT:',   'V'/

    ! liste des defauts pour iment, isll, ozsrt, i, l
    data defo  /981*'SCRAP',   'TAPE2', 'TAPE4', '$INPUT', '$OUTPUT', 'INFO   ', 'ISENT_SCRAP', 'OUI'/

    ! lfn = liste que l usager propose pour remplacer
    data lfn   /981*'SCRAP',   'TAPE2', 'TAPE4', '$INPUT', '$OUTPUT', 'INFO   ', 'ISENT_SCRAP', 'NON'/

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
    data qlxcon(57) /'WAIT'    /  qlxval(57) /  wait /
    data qlxcon(58) /'GO'      /  qlxval(58) /    go /
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
    data qlxcon(96) /'FENTREE' /  qlxval(96) / fentree /
    data qlxcon(97) /'FSORTIE' /  qlxval(97) / fsortie /
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

    ! KIND = 0, p est en hauteur (m) par rapport au niveau de la mer
    ! KIND = 1, p est en sigma (0.0 -> 1.0)
    ! KIND = 2, p est en pression (mb)
    ! KIND = 3, p est un code arbitraire
    ! KIND = 4, p est en hauteur (M) par rapport au niveau du sol
    ! KIND = 5, p est en coordonnee hybride
    ! KIND = 6, p est en coordonnee theta
    ! KIND = 15, rererve (entiers)
    ! KIND = 21, p est en GalChen

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

    data(qlxlcon(i), i = 1, 2) /'OUI', 'NON'/
    data(qlxlval(i), i = 1, 2) /1, 0/

    integer, parameter :: iun_isll = 0

    ! listl = position  iment(tape1 standard), isll(tape4 sequentiel)
    !       ozsrt(tape2 - standard - seq file - random ms)
    ! 6 = nombre de lfn

    ! nombre d'equivalence output de ccard
    nequiv = -1
    ! lnkdiun = 0
    !> \todo What does lnkdiun(1) represent?
    ! lnkdiun(1) = 1
    ! lnkdiun(idx_ozsrt) = 2
    CALL ccard(listl, defo, lfn, nentries, nequiv)

    ier = fnom(5, lfn(idx_i), 'SEQ', 0)
    ier = fnom(6, lfn(idx_log), 'SEQ', 0)

    outputFilePath = lfn(idx_ozsrt)

    ! Imprimer boite debut du programme
    app_ptr = app_init(0, 'pgsm', PGSM_VERSION, '', BUILD_TIMESTAMP)
    CALL app_logstream(lfn(idx_log))
    CALL app_start()

    CALL qqqfilt(1, 0, 0, 0)
    CALL qqqfilt(2, 0, 0, 0)
    CALL chk_tmpdir

    IF (lfn(idx_isent)(1:11) /= 'ISENT_SCRAP') THEN
        inputMode = SEQUENTIEL
    ELSE
        inputMode = RANDOM
    ENDIF

    IF (lfn(1)(1:5) /= 'SCRAP' .and. inputMode == SEQUENTIEL) THEN
        CALL app_log(APP_ERROR, 'Cannot mix sequential and random files')
        app_status = app_end(13)
        CALL qqexit(13)
    ENDIF

    IF (lfn(idx_isll) /= 'TAPE4') THEN
        ! iun_isll = 0, Unit number 0 = stderr
        ! It makes no sense to set the standard error to read only
        ier = fnom(iun_isll, lfn(idx_isll)(1:5), 'FMT+SEQ+R/O', 0)
    ENDIF

    IF (lfn(idx_verbose) /= 'NON') THEN
        ier = ezsetopt('verbose', 'yes')
    ENDIF

    nInput = 1
    DO WHILE (lfn(nInput) /= 'SCRAP')
        nInput = nInput + 1
    END DO
    nInput = nInput - 1

    IF (nInput < 1) THEN
        CALL app_log(APP_ERROR, 'No input files given as arguments')
        app_status = app_end(13)
        CALL qqexit(13)
    ENDIF

    IF (inputMode /= RANDOM .and. nInput > 1) THEN
        CALL app_log(APP_ERROR, 'Only one input file can be provided in sequential mode')
        app_status = app_end(13)
        CALL qqexit(13)
    ENDIF

    allocate(inputFilePaths(nInput))
    do i = 1, nInput
        inputFilePaths(i) = lfn(i)
    end do

    ! Initialiser les dictionnaires
    CALL qlxopt ('CARMOT', 4)

    ! 3 appels reconnus :
    !   1 = sortie(std, noenrg) noenrg>=2
    !   2 = sortie(ms, noenrg, jwrit)
    !   3 = sortie(seq)
    CALL qlxinx(sorti, 'SORTIE', nsort, 0103, 2)

    ! 1 = heure(00, 12, 24, 25 ... max20) minimum 1
    ! 2 = champ(mac, 00, 06) minimum 2 pour
    ! 2 = champ(pcp, 00, 06) minimum 2 pour
    ! accumulateur d"ajustement ou precipitation
    CALL qlxinx (heure, 'HEURE', nheure, 0140, 2)
    CALL qlxinx (heure, 'IP2', nheure, 0140, 2)

    ! setintx(voisin) avec le plus proche
    ! setintx(lineair) interpolation lineaire
    ! setintx(cubique) interpolation cubique(defaut)
    CALL qlxinx (qqqintx, 'SETINTX', nsetin, 0101, 2)

    CALL qlxinx (setxtrap, 'EXTRAP', nsetex, 0101, 2)

    ! - champ(z, niveau)  niveau = 1000, 850, .......
    ! - champ(t, niveau)  niveau = 1000, 850, .......
    ! - champ(q, niveau)  niveau = 1000, 850, .......
    ! - champ(d, niveau)  niveau = 1000, 850, .......
    ! - champ(w, niveau)  niveau = 1000, 850, .......
    ! - champ(es, niveau)  niveau = 1000, 850, .......
    ! - champ(uv, niveau)  niveau = 1000, 850, .......
    ! - champ(uvs)  pas de niveau vent de surface
    ! - champs(ventuvs) voir directive paires(.....
    ! - champ(vent, niveau) niveau = 1000, 850, .......
    ! - champ(nuage)  nuage bas, moyen, haut
    !         rec 1 = bas  rec 2 = moyen  rec 3 = haut
    ! - champ(ecm)  epaisseur de la couche limite
    ! - champ(pnm)  pression au niveau de la mer
    ! - champ(psurf)  pression a la surface
    ! - champ(ts)  temperature a la surface
    ! - champ(epais, niveau1, niveau2) niveau2 - niveau1
    ! - champ(mac, heure1, heure2)  heure2 - heure1
    ! - champ(pcp, heure1, heure2)  heure2 - heure1
    CALL qlxinx (champ, 'CHAMP', nchamp, 0131, 2)

    ! appel - chmpdif (noment, nomsrt, ip1tab, ip2tab, ip3tab)
    !    ex:  chmpdif ("gz", "dz", [1000, 500], 12, 0)
    !         z500mb - z1000mb  a 12hr
    !         fichier de sorti aura ip1 = 1000, ip2 = 500, ip3 = 12
    !    ex:  chmpdif ("gz", "dz", 1000, [6, 12, 18, 24], 0)
    !         z1000mb 6hr - z1000mb  a 12hr
    !         fichier de sorti aura ip1 = 1000, ip2 = 6, ip3 = 12
    !    ex:  chmpdif ("gz", "dz", 1000, 6, [1, 2, 3, 4])
    !         z1000mb 6hr ip3 = 1 - z1000mb  6hr ip3 = 2
    !         fichier de sorti aura ip1 = 1000, ip2 = 1, ip3 = 2
    CALL qlxinx (chmpdif, 'CHMPDIF', npar, 0508, 2)

    CALL qlxinx (champ_seq, 'CHAMPSEQ', npar, 0303, 2)
    ! appel - champseq(['GZ', 'TT', 'UU'], [1000, 850, 500], WAIT)
    ! appel - champ_seq(' ', [1000, 850, 500], WAIT)
    ! appel - champ_seq(['GZ', 'TT', 'UU'], -1, GO)

    CALL qlxinx (convs, 'CONV', ncon, 0305, 2)
    ! appel - conv(nom, ecart, facteur, bas, haut) directive
    !         conv("ts", -273.16, 1.0, -280.0, -250.0)
    !         routine conver dans ecriture soustrait
    !         273.16 au champ et multiplit par 1.0
    !         enleve toutes les valeurs plus petites que -280
    !         enleve toutes les valeurs plus grandes que -250
    !         avant d ecrire le champ
    CALL qlxinx (grille2, 'GRILLE',  ngr, 0109, 2)
    ! 8 appels a grille    1 = grille(std, nni, nnj, lg1)
    !                        std = standard lat lon
    !                        nni = nombre de pts est-ouest
    !                        nnj = nombre de pts nord-sud
    !                        lg1 = 0  global
    !                            = 1  hem nord
    !                            = 2  hem sud
    !                      2 = grille(latlon, nni, nnj, lat0, lon0, dlat, dlon)
    !                        latlon = grille lat lon
    !                        nni = nombre de pts est-ouest
    !                        nnj = nombre de pts nord-sud
    !                        lat0 = premiere lat du coin degree
    !                        lon0 = premiere lon du coin degree
    !                        dlat = espacement entre latitude  (degree)
    !                        dlon = espacement entre longitude (degree)

    !                      3 = grille(ps, nni, nnj, pi, pj, d60, dgrw)
    !                        ps = polaire stereographique
    !                        nni = nombre pts est-ouest (dir i)
    !                        nnj = nombre de pts nord-sud (dir j)
    !                        pi = position du pole nord(pi = 26)
    !                        pj = position du pole nord(pj = 28)
    !                        d60 = distance en metres entre les pts
    !                             a 60 degrees nord (latitude)
    !                        drgw = angle entre l"axe x et greewich

    !                      4 = grille(tape4, nni, nnj, ip1, ip2, ip3)
    !                        tape4 = fichier contenant nni*nnj(lat-lon)
    !                        nni = nombre de pts est-ouest
    !                        nnj = nombre de pts nord-sud
    !                        ip1 = definit par usager
    !                        ip2 = definit par usager
    !                        ip3 = definit par usager

    !                      5 = grille(stdb, nni, nnj, hem)
    !                        stdb = standard b
    !                        nni = nombre de pts est-ouest
    !                        nnj = nombre de pts nord-sud
    !                        hem = hemisphere 0 = global
    !                                         1 = nord
    !                                         2 = sud

    !                      6 = grille(gauss, nni, nnj, hem)
    !                        gauss = grille gaussienne lat-lon
    !                        nni = nombre de pts est-ouest
    !                        nnj = nombre de pts nord-sud
    !                        hem = hemisphere 0 = global
    !                                         1 = nord
    !                                         2 = sud

    !                      7 = grille(tape1, ip1, ip2, ip3, ip4, nord/sud)
    !                        tape1 = lit sur fichier 1 lat-lon ou xy
    !                        ip1 = valeur 0-32767
    !                        ip2 = valeur 0-32767
    !                        ip3 = valeur 0-4095
    !                        ip4 = valeur "xydir" ou "llist"
    !                            = valeur "lldir" ou "xylis"

    !                      8 = grille(tape2, ip1, ip2, ip3, ip4, nord/sud)
    !                        tape2 lit sur fichier 2 lat-lon ou xy
    !                        ip1 = valeur 0-32767
    !                        ip2 = valeur 0-32767
    !                        ip3 = valeur 0-4095
    !                        ip4 = valeur "xydir" ou "llist"
    !                            = valeur "lldir" ou "xylis"

    ! lrsmde(nomvar, typvar, date, niveau, heure, ip3, etiquet)
    CALL qlxinx (lrsmde, 'LIRMODE', nlirmde, 0708, 2)
    ! lrsmds(nomvar, typvar, date, niveau, heure, ip3, etiquet)
    CALL qlxinx (lrsmds, 'LIRMODS', nlirmds, 0708, 2)

    ! metsym(z, oui)
    ! z = geopotentiel "gz"
    ! oui = symetrique
    CALL qlxinx (metsym, 'METSYM', idum, 0202, 2)

    CALL qlxinx (outlalo, 'OUTLALO', nlalo, 0108, 2)
    ! outlalo(ip1, ip2, ip3, nomlat, nomlon, grtyp, etiklat, etiklon)
    !         ip1 = valeur 0-32767
    !         ip2 = valeur 0-32767
    !         ip3 = valeur 0-4095
    !         nomlat = nom du champ de latitude 2 car
    !         nomlon = nom du champ de longitude 2 car
    !         grtyp = type de grille
    !         etiklat = nom de l'etiquette latitude
    !         etiklon = nom de l'etiquette longitude

    CALL qlxinx (pairvct, "PAIRES", npairuv, 0305, 2)
    ! ex: paires("uv", "uu", "vv", 0) vecteur "uu", "vv" geographique
    !                                 niveau donne par champ
    ! ex: paires("ventuvs", "us", "vs", "uv") vitesse du vent a la surface
    ! ex: paires("uvs", "us", "vs", 0) vecteurs du vent a la surface

    CALL qlxinx (pgcoupe, 'MOYENT', nmoy, 0232, 2)
    CALL qlxinx (moysrt, 'MOYSRT', nmoy, 0232, 2)
    CALL qlxinx (liren, 'LIREE', nlire, 0708, 2)
    CALL qlxinx (lirsr, 'LIRES', nlire, 0708, 2)
    CALL qlxinx(plmnmod, 'PLUSE', najou, 0707, 2)
    CALL qlxinx (pluss, 'PLUSS', najou, 0707, 2)
    CALL qlxinx (foise, 'FOISE', multp, 0707, 2)
    CALL qlxinx (foiss, 'FOISS', multp, 0707, 2)
    CALL qlxinx (divisee, 'DIVE', multp, 0707, 2)
    CALL qlxinx (divises, 'DIVS', multp, 0707, 2)
    CALL qlxinx (moinse, 'MOINSE', nenle, 0707, 2)
    CALL qlxinx (moinss, 'MOINSS', nenle, 0707, 2)
    CALL qlxinx (moyene, 'MOYENE', nmoys, 0101, 2)
    CALL qlxinx (ecrits, 'ECRITS', necrt, 0814, 2)
    CALL qlxinx (modul2e, 'MODUL2E', nmod, 0707, 2)
    CALL qlxinx (modul2s, 'MODUL2S', nmod, 0707, 2)
    CALL qlxinx (racine, 'RACINE', nraci, 0101, 2)
    CALL qlxinx (operat, 'PFOIS', npfo, 0303, 2)
    CALL qlxinx (expon, 'EXPON', npex, 0101, 2)
    CALL qlxinx (alogn, 'ALOGN', npex, 0101, 2)
    CALL qlxinx (absolu, 'ABSOLU', npex, 0101, 2)
    CALL qlxinx (carre, 'CARRE', npex, 0101, 2)

    CALL qlxinx (qqqecho, 'ECHO', dum, 0101, 2)
    CALL qlxinx (qqqident, 'IDENT', npar, 0103, 2)
    CALL qlxinx (qqqform, 'FORMAT', dum, 0101, 2)
    CALL qlxinx (coord, 'COORD', dum, 0202, 2)
    CALL qlxinx (qqqfilt, 'FILTRE', dum, 0204, 2)

    CALL qlxins (npack, 'COMPAC', dum, 1, 1)
    CALL qlxins (message, 'MESSAGE', dum, 1, 1)
    CALL qlxins (idum, 'DELTA', dum, 1, 1)
    CALL qlxins (typeent, 'TYPEENT', dum, 1, 1)
    CALL qlxins (typesrt, 'TYPESRT', dum, 1, 1)
    CALL qlxins ( voire, 'VOIRENT', dum, 1, 1)
    CALL qlxins ( voirs, 'VOIRSRT', dum, 1, 1)
    CALL qlxins ( pose, 'PAUSE', dum, 1, 1)
    !> \bug this directive as been broken for while! If the user provided the value oui/non, chk_userdate would still return -1.
    !> If the user provided an actual date, the result of chk_userdate would be undefined
    CALL qlxins ( userdate, 'DATE', dum, 3, 1)
    !> \bug The code depending on seldat was broken and impossible to reach
    CALL qlxins (idum, 'OPDAT', dum, 0, 1)
    CALL qlxins (printen, 'PRINTEN', dum, 7, 1)
    CALL qlxins (printsr, 'PRINTSR', dum, 7, 1)
    CALL qlxins (etikent, 'ETIKENT', nwetike, 3, 1)
    CALL qlxins (masque, 'MASQUE', dum, 1, 1)
    CALL qlxins (etiksrt, 'ETIKSRT', nwetiks, 3, 1)
    CALL qlxins (idum, 'ENREG', dum, 1, 1)
    CALL qlxins (ip2srt, 'IP2SRT', dum, 1, 1)
    CALL qlxins (ip3ent, 'IP3ENT', dum, 1, 1)
    CALL qlxins (ip3srt, 'IP3SRT', dum, 1, 1)
    CALL qlxins (unefois, 'UNEFOIS', dum, 1, 1)
    CALL qlxins (once, 'ONCE', dum, 1, 1)
    CALL qlxins (idum, 'DIESE', dum, 1, 1)
    CALL qlxins (ip1style, 'IP1STYLE', dum, 1, 1)
    CALL qlxins (dateform, 'DATEFORM', dum, 1, 1)
    CALL qlxins (compression_level, 'COMPRESS', dum, 1, 1)

    do i = 1, nbKeys - 1
       CALL qlxins(qlxval(i), qlxcon(i), dum, 1, 0)
    ENDDO

    do i = 1, 2
       CALL qlxins(qlxlval(i), qlxlcon(i), dum, 1, 0)
    ENDDO

    ! Initialisation parametres de sortie pour fichier formate
    CALL initid

    iopc = app_loglevel(lfn(idx_msglvl))
    ier = fstopl('REDUCTION32', .true., .false.)

    ipose = 0
    CALL readlx(5, kend, ipose)

    IF (associated(tmplat)) deallocate(tmplat)
    IF (associated(tmplon)) deallocate(tmplon)

    IF (outputFileMode == 1) THEN
        CALL copy2outputFile('HY')
        CALL copy2outputFile('!!')
    END IF
    iopc = app_loglevel('INFO')
    DO i = 1, nInput
        ! ier = fstfrm(lnkdiun(i))
        ! CALL fclos(lnkdiun(i))
        ldum = inputFiles(i)%close()
    END DO

    IF (outputFileMode == 1) THEN
        IF (voirs)  THEN
            IF (message) THEN
                call outputFile%print_summary()
                ! ier = fstvoi(lnkdiun(idx_ozsrt), 'RND')
                !> \todo Replace with fst24 equivalent
            END IF
        END IF

        ! ier = fstfrm(lnkdiun(idx_ozsrt))
        ! CALL fclos(lnkdiun(idx_ozsrt))
        ldum = outputFile%close()
    ELSE IF (outputFileMode == 2) THEN
        CALL app_log(APP_WARNING, '"MS" Nfile type are not supported in this version of PGSM')
    ELSE IF (outputFileMode == 3)  THEN
        ! Fermer fichier sequentiel
        ! CALL fclos(lnkdiun(idx_ozsrt))
    ELSE IF (outputFileMode == 4)  THEN
            ! CALL pgsmcf(lnkdiun(idx_ozsrt))
    END IF

    ! Imprime boite avec le temps d execution du pgm  pgsm
    IF (ipose > 0) THEN
        app_status = app_end(13)
        CALL qqexit(13)
    ELSE
        app_status = app_end(0)
    END IF
END PROGRAM
