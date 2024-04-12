!> CHAQUE PT D'UN CHAMP LU EST MI AU CARRE DANS ACCUMULATEUR
subroutine lrsmde(nom, type, idat, niv, ihr, ip3, etiqet)
    use app
    implicit none

!OBJET(LRSMDE)
!         LIRE UN CHAMP SUR FICHIER D'ENTRE OU DE SORTI ET SAUVE DANS
!         L'ACCUMULATEUR CHAQUE PT AU CARRE ET LES DIRECTIVES SUIVANTES
!         LIRMODE OU LIRMODS AJOUTERONT CHAQUE CHAMP(PT AU CARRE) A
!         L'ACCUMULATEUR LA SOMME DES CHAMPS EST GARDER DANS L'ACCUMULATEUR
!         ET PEUT ETRE SAUVE PAR LA DIRECTIVE ECRITS.

!ARGUMENTS
!   IN   NOM     NOM DU CHAMP "GZ", "TT"...LCAR(GZ)
!   IN   TYPE    TYPE DE CHAMP  "P"=PREVISION   "A"=ANALYSE
!   IN   NIV     NIVEAU DU CHAMP 500MB....
!   IN   IHR     HEURE DU CHAMP  (IP2)
!   IN   IP3     LIBRE A L'USAGER ET COMPTEUR POUR MOYENNE UTILISER PAR ECRITS
!   IN   ETIQET  ETIQETTE 10 CARACTERES

!APPEL   VIA DIRECTIVE
!        LIRMDE(NOM, TYPE, IDAT, NIV, IHR, IP3, ETIQET)
!        LIRMDS(NOM, TYPE, IDAT, NIV, IHR, IP3, ETIQET)

#include "llccmm.cdk90"
#include "voir.cdk90"
#include "accum.cdk90"
#include "chck.cdk90"
#include "lires.cdk90"
#include "ecrires.cdk90"
#include "indptr.cdk90"
#include "blancs.cdk90"
#include "styles.cdk90"

    external fstinf, pgsmlir, memoir, fstprm, pgsmabt, imprime, fstopc, messags, fstcvt
    integer fstinf, pgsmlir, fstprm, fstopc, fstcvt

    character *12 cetiket
    character *4 cnomvar
    character *2 ctypvar
    character *1 cigtyp
    integer etiqet(3), idat, ihr, iip3, ip3, irec1, iunit, niv(2), nom, num1, type
    integer inomb, i, j, iopc, lniv
    integer cnbits, cdatyp, cswa, clng, cdltf, cubc, extra1, extra2, extra3
    integer argdims, letiket(3)
    external argdims
    real p
    character*8 string

    iunit=1
    iip3=ip3
    if (ip3 == 4095) iip3=-1

    ! MODIFICATION DE HOLLERITH A CARACTERE
    cnomvar = '    '
    ctypvar = '  '
    cetiket = '            '
    cigtyp  = ' '

    letiket(1) = etiqet(1)
    letiket(2) = blancs
    letiket(3) = blancs
    if (argdims(7).gt.1) then
        letiket(2) = etiqet(2)
    endif
    if (argdims(7).gt.2) then
        letiket(3) = etiqet(3)
    endif

    if (argdims(4) > 1) then
        lniv = niv(1)
        p = transfer(niv(1), p)
        call convip_plus(lniv, p, -1*niv(2)-1000, ip1style, string, .false.)
    endif

 100  ier = fstcvt(    nom, type, letiket , -1, cnomvar, ctypvar, cetiket, cigtyp, .true.)
    irec1 = fstinf(iunit, nni, nnj, nnk, idat, cetiket, lniv, ihr, iip3, ctypvar, cnomvar)
    if (irec1 .lt. 0)   then
        call app_log(APP_ERROR, 'lrsmde: Record does not exist')
        call pgsmabt
    endif

    ichck = 1

    ier = fstprm( irec1, idatt, ideet, npas, nni, nnj, nnk, cnbits, cdatyp, jpp1, jpp2, jpp3, ctypvar, cnomvar, cetiket, cigtyp, igg1, igg2, igg3, igg4, cswa, clng, cdltf, cubc, extra1, extra2, extra3)
    if (ier .lt. 0) call app_log(APP_ERROR, 'lrsmde: FSTPRM failed')

    ! MODIFICATION DE CARACTERE A HOLLERITH
    cnumv = cnomvar
    ctypv = ctypvar
    cetik = cetiket
    cigty = cigtyp

    ! VERIFIER SI GRILLE GAUSSIENNE NI DOIT ETRE PAIR
    if (cigtyp == 'G'.and.mod(nni, 2).ne.0)  call messags(nni)

    inomb = nni * nnj * nnk

    if (.not.message) iopc= fstopc('TOLRNC', 'DEBUGS', .true.)

    allocate(tmpif0(nni, nnj))

    ! SI COMPTEUR .NE. 4095 ICNT=1
    icnt = 1
    if (iunit == 1.and.ip3 == 4095)  icnt=jpp3
    if (iunit == 2.and.ip3 == 4095)  icnt=jpp3

    num1 = pgsmlir(tmpif0, iunit, nni, nnj, nnk, idat, cetiket, jpp1, jpp2, jpp3, ctypvar, cnomvar, cigtyp)
    if (num1 < 0)  then
        call app_log(APP_ERROR, 'lrsmde: Record does not exist')
        call pgsmabt
    endif
    if (printen)  call imprime(cnomvar, tmpif0, nni, nnj)

    do j = 1, nnj
        do i = 1, nni
            tmpif0(i, j) = tmpif0(i, j) * tmpif0(i, j)
        enddo
    enddo

    return

entry lrsmds(nom, type, idat, niv, ihr, ip3, etiqet)
    iunit = 2
    iip3 = ip3
    if (ip3 == 4095) iip3 = -1

    go to 100
end
