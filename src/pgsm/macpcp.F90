!> Interpole ajustement convectif ou precipitation
subroutine macpcp(cnom, npar, itime)
    use app
    use packing, only : npack
    use pgsm_mod, only : nwetike, etikent, message, typeent, ip3ent
    use accum, only : npas
    use grilles, only : cgrtyp, gdin, gdout, li, lj, lg1, lg2, lg3, lg4
    use param, only : dat, deet
    use symetry, only : symetri
    implicit none

    !> Nom du champ
    character(len = 4), intent(in) :: cnom
    !> Nombre de locations utilisees dans itime
    integer, intent(in) :: npar
    !> Table contenant 2 heures ou niveaux
    integer, dimension(2), intent(in) :: itime

    !          extraire la difference entre deux champs dont les
    !          heures  sont differents
    !          avec routine fstinf on extrait le record necessaire pour
    !          routine fstprm qui identifit les parametres utilises
    !          par routine lire
    !          on reserve la memoire pour les deux champs de travail
    !          routine ecritur identifit la sorte de fichier utilisee
    !          pour ecrire

    external ecritur
    external loupneg, loupsou, pgsmabt, imprime, messags
    integer, external :: fstopc

#include "gdz.cdk90"

    character *12 cetike, cetiket
    character *1 cigtyp
    character *2 ctypvar

    real, dimension(:), allocatable :: lclif1, lclif2

    integer i, ig1, ig2, ig3, ig4, ip1, irec1, irec2
    integer jp1, jp2, jp3, ni, nj, nk, iopc
    integer cdatyp, cnbits
    integer cswa, clng, cdltf, cubc, extra1, extra2, extra3
    integer ezqkdef, ezdefset, ezsint, chkenrpos, datev
    logical symetri, sym

    type(fst_query) :: query
    type(fst_record) :: rec1
    type(fst_record) :: rec2

    if (npar .ne. 2) then
        if (message) call app_log(APP_ERROR, 'macpcp: Wrong call to MACPCP, must have 3 arguments')
        return
    endif

    ! # doit etre egal a zero dans fichier d'ENTRE
    call chk_userdate(datev)

    ! modification de hollerith a caractere
    if (etikent(1) .ne. -1) then
        write(cetiket, '(3A4)') (etikent(i), i=1, nwetike)
    else
        cetiket = '            '
    endif

    if (typeent .ne. -1) then
        write(ctypvar, '(A2)') typeent
    else
        ctypvar = '  '
    endif

    ! ip1 = 0
    ! irec1=fstinf(1, ni, nj, nk, datev, cetiket, ip1, itime(1), ip3ent, ctypvar, cnomvar)
    ! irec2=fstinf(1, ni, nj, nk, datev, cetiket, ip1, itime(2), ip3ent, ctypvar, cnomvar)
    query = file%new_query(datev = datev, etiket = cetiket, ip1 = 0, ip2 = itime(1), ip3 = ip3ent, typvar = ctypvar, nomvar = cnom)
    if (.not. query%findNext(rec1)) then
        call app_log(APP_ERROR, 'macpcp: Record does not exist in input file')
        return
    endif
    call query%free()

    query = file%new_query(datev = datev, etiket = cetiket, ip1 = 0, ip2 = itime(2), ip3 = ip3ent, typvar = ctypvar, nomvar = cnom)
    if (.not. query%findNext(rec2)) then
        call app_log(APP_ERROR, 'macpcp: Record does not exist in input file')
        return
    endif
    call query%free()

    if (rec1%nk > 1 .or. rec2%nk) then
        call app_log(APP_ERROR, 'macpcp: PGSM does not accept 3 dimension fields (NK>1)')
        call pgsmabt
    endif

    ! identifier parametres pour champ 1
    ier = fstprm( irec1, dat, deet, npas, ni, nj, nk, cnbits, cdatyp, jp1, jp2, jp3, ctypvar, cnomvar, cetike, cigtyp, ig1, ig2, ig3, ig4, cswa, clng, cdltf, cubc, extra1, extra2, extra3)
    if (ier .lt. 0) call app_log(APP_ERROR, 'macpcp: FSTPRM failed')

    ! verifier si grille gaussienne ni doit etre pair
    if (cigtyp .eq. 'G' .and. mod(ni, 2) .ne. 0)  call messags(ni)

    ! lire champ no 1
    allocate(lclif1(ni*nj))
    if ( .not. message) iopc= fstopc('TOLRNC', 'DEBUGS', .true. )

    num1 = pgsmlir(lclif1, 1, ni, nj, nk, datev, cetiket, jp1, jp2, jp3, ctypvar, cnomvar, cigtyp)

    if (printen)  call imprime(cnomvar, lclif1, ni, nj)

    ! identifier parametres pour champ 2
    ier = fstprm(irec2, dat, deet, npas, ni, nj, nk, cnbits, cdatyp, jp1, jp2, jp3, ctypvar, cnomvar, cetike, cigtyp, ig1, ig2, ig3, ig4, cswa, clng, cdltf, cubc, extra1, extra2, extra3)
    if (ier .lt. 0) call app_log(APP_ERROR, 'macpcp: FSTPRM failed')

    ! verifier si grille gaussienne ni doit etre pair
    if (cigtyp .eq. 'G' .and. mod(ni, 2) .ne. 0)  call messags(ni)

    ! lire champ 2
    allocate(lclif2(max0(li*lj, ni*nj)))
    if ( .not. message) iopc= fstopc('TOLRNC', 'DEBUGS', .true. )

    num2 = pgsmlir(lclif2, 1, ni, nj, nk, datev, cetiket, jp1, jp2, jp3, ctypvar, cnomvar, cigtyp)
    if (printen)  call imprime(cnomvar, lclif2, ni, nj)

    ! difference entre les deux champs
    call loupsou(lclif1, lclif2, ni * nj)

    ! interpolation horizontale
    if (cgrtyp .eq. '*') then
        ier = chkenrpos(ig1, ig2, ig3)
    else
        ! variable symetrique oui= .true. 
        if (cigtyp == 'A' .or. cigtyp == 'B' .or. cigtyp == 'G') then
            if (ig1 /= 0) sym = symetri(cnomvar)
        endif
        gdin = ezqkdef(ni, nj, cigtyp, ig1, ig2, ig3, ig4, inputFiles(1)%get_unit())
        ier = ezdefset(gdout, gdin)
        ier = ezsint(lclif2, lclif1)
    endif

    !     #   jp1 - contient heure du premier champ
    !     #   jp2 - contient heure du deuxieme champ
    jp1 = itime(1)
    jp2 = itime(2)
    jp3 = 0

    ! deet et npas contiennent les dernieres valeurs lues dans le dernier record

    ! eliminer toutes les valeurs du champ negative precip et acumulateur d'ajustement ne peuvent etre negatif

    call loupneg(lclif2, 0.0, 1, li * lj, 1)

    ! ecrire sur fichier standard, ms, sequentiel
    if (cgrtyp .eq. '*') then
        call ecritur(lclif1, npack, dat, deet, npas, ni, nj, nk, jp1, jp2, jp3, ctypvar, cnomvar, cetike, cigtyp, ig1, ig2, ig3, ig4)
    else
        call ecritur(lclif2, npack, dat, deet, npas, li, lj, nk, jp1, jp2, jp3, ctypvar, cnomvar, cetike, cgrtyp, lg1, lg2, lg3, lg4)
    endif

    deallocate(lclif2)
    deallocate(lclif1)
end
