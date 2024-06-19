subroutine plmnmod(nom, type, datev, niv, ip2, ip3, etiqet)
    call plmnmod_orig(nom, type, datev, niv, ip2, ip3, etiqet, 1, 1)
end subroutine


!> Additionne soustrait multiplit module 2 champs
subroutine plmnmod_orig(nom, type, datev, niv, ip2, ip3, etiqet, fact, iunit)
    use app
    use rmn_fst24
    use accum, only : nni, nnj, nnk, cigty, igg1, igg2, igg3, igg4, icnt, unefois, once
    use pgsm_mod, only : ier, ip1style, printen, tmpif0, tmpif1
    use chck, only : ichck
    use files, only : fentree, fsortie, inputFiles, outputFile
    implicit none

    !> Variable name
    integer, intent(in) :: nom
    !> Field type (a = analysis, p = forecast)
    integer, intent(in) :: type
    !> Validity date (CMC stamp)
    integer, intent(in) :: datev
    !> Level (IP1)
    integer, intent(in) :: niv(2)
    !> Forecast hour (IP2)
    integer, intent(in) :: ip2
    !> IP3
    integer, intent(in) :: ip3
    !> Label
    integer, intent(in) :: etiqet(3)
    !> Multiplication factor
    integer, intent(in) :: fact
    !> Fortran file unit
    integer, intent(in) :: iunit

    external :: pgsmabt, imprime
    external :: lopascm, messags
    integer, external :: fstcvt
    integer, external :: argdims

    ! Lire un champ sur fichier 1 ou 2 de meme nature et dimensions
    ! celui dans l accumulateur et que l on ajoute , soustrait , multiplit
    ! ou fait la somme de chaque point des deux champs au carre.

    ! APPEL VIA DIRECTIVE
    ! PLUSE(NOM, TYPE, IDAT, NIV, ip2, IP3, ETIQET) FICHIER D'ENTRE
    ! PLUSS(NOM, TYPE, IDAT, NIV, ip2, IP3, ETIQET)  FICHIER DE SORTIE
    ! MOINSE(NOM, TYPE, IDAT, NIV, ip2, IP3, ETIQET) FICHIER D'ENTRE
    ! MOINSS(NOM, TYPE, IDAT, NIV, ip2, IP3, ETIQET)  FICHIER DE SORTIE
    ! MODUL2E(NOM, TYPE, IDAT, NIV, ip2, IP3, ETIQET) FICHIER D'ENTRE
    ! MODUL2S(NOM, TYPE, IDAT, NIV, ip2, IP3, ETIQET)  FICHIER DE SORTIE
    ! FOISE(NOM, TYPE, IDAT, NIV, ip2, IP3, ETIQET) FICHIER D'ENTRE
    ! FOISS(NOM, TYPE, IDAT, NIV, ip2, IP3, ETIQET)  FICHIER DE SORTIE

#include "blancs.cdk90"

    character(len = 12) :: cetiket
    character(len = 4) :: cnomvar
    character(len = 2) :: ctypvar
    character(len = 1) :: cigtyp

    integer :: letiket(3)

    integer :: lniv
    real :: p
    character(len = 8) :: string

    type(fst_file), pointer :: file
    type(fst_query) :: query
    type(fst_record) :: record

    if (fentree == iunit) then
        file = inputFiles(1)
    else if (fsortie == iunit) then
        file = outputFile
    else
        call app_log(APP_ERROR, 'lrsmdes: Unrecognized file! Must be FENTREE or FSORTIE')
        call pgsmabt
    end if

    ! Verifier si directive liren ou lirsr a ete appele
    if (ichck .eq. 0)  then
        ! erreur faut appeler liren ou lirsr
        call app_log(APP_ERROR, 'plmnmod: Directives LIREE or LIRES must be called before')
        call pgsmabt
    endif

    ! Modification de hollerith a caractere
    cnomvar = '    '
    ctypvar = '  '
    cetiket = '            '
    cigtyp  = ' '

    letiket(1) = etiqet(1)
    letiket(2) = blancs
    letiket(3) = blancs
    if (argdims(7) .gt. 1) then
        letiket(2) = etiqet(2)
    endif
    if (argdims(7) .gt. 2) then
        letiket(3) = etiqet(3)
    endif

    lniv = niv(1)
    if (argdims(4) > 1) then
        p = transfer(niv(1), p)
        call convip_plus(lniv, p, -1*niv(2)-1000, ip1style, string, .false.)
    endif

    ier = fstcvt(nom, type, letiket, -1, cnomvar, ctypvar, cetiket, cigtyp, .true.)

    query = file%new_query(datev = datev, etiket = cetiket, ip1 = lniv, ip2 = ip2, ip3 = ip3, typvar = ctypvar, nomvar = cnomvar)
    do while (query%find_next(record))
        ! VERIFIER SI GRILLE GAUSSIENNE NI DOIT ETRE PAIR
        if (record%grtyp .eq. 'G' .and. mod(record%ni, 2) .ne. 0)  call messags(record%ni)

        if (record%ni .ne. nni) then
            write(app_msg, "(2x, 'plmnmod: Wrong field dimension: NI  ENTRE =', i10, 'NI ACCUMULATEUR=', i10)") record%ni, nni
            call app_log(APP_ERROR, app_msg)
            call pgsmabt
        endif

        if (record%nj .ne. nnj) then
            write(app_msg, "(2x, 'plmnmod: Wrong field dimension: NJ  ENTRE =', i10, 'NJ ACCUMULATEUR=', i10)") record%nj, nnj
            call app_log(APP_ERROR, app_msg)
            call pgsmabt
        endif

        if (record%nk .ne. nnk) then
            write(app_msg, "(2x, 'plmnmod: Wrong field dimension: NK  ENTRE =', i10, 'NK ACCUMULATEUR=', i10)") record%nk, nnk
            call app_log(APP_ERROR, app_msg)
            call pgsmabt
        endif

        if (cigty .ne. record%grtyp) then
            write(app_msg, "(2x, 'plmnmod: Wrong grid: GRILLE ENTRE=', a1, 'ACCUMULATEUR=', a1)") record%grtyp, cigty
            call app_log(APP_ERROR, app_msg)
            call pgsmabt
        endif

        if (cigty .eq. 'G' .or. cigty .eq. 'A' .or. cigty .eq. 'B') then
            if (record%ig1 .ne. igg1) then
                write(app_msg, *)'plmnmod: Wrong hemisphere ig1e =', record%ig1, ' has to be ', igg1, '. heck PLUSE/S - MOINS(E\S) - MODUL2E/S - FOIS(E\S) directives'
                call app_log(APP_ERROR, app_msg)
                call pgsmabt
            endif
        else
            if (cigty .eq. 'N' .or. cigty .eq. 'S' .or. cigty .eq. 'L') then
                if (record%ig1 .ne. igg1 .or. record%ig2 .ne. igg2 .or. record%ig3 .ne. igg3 .or. record%ig4 .ne. igg4) then
                    call app_log(APP_ERROR, 'plmnmod: 2 different fields, check PLUSE/S - MOINS(E\S) - MODUL2E/S - FOIS(E\S) directives')
                    call pgsmabt
                endif
            endif
        endif

        allocate(tmpif1(nni, nnj))

        if (.not. file%read(record, data = c_loc(tmpif1(1, 1))))  then
            call app_log(APP_ERROR, 'plmnmod: Failed to read field')
            return
        endif
        call prefiltre(tmpif1, record%ni, record%nj, record%grtyp)

        if (printen) call imprime(record%nomvar, tmpif1, nni, nnj)

        ! Ajoute 1 au compteur icnt dans common accum initialiser a 1 dans main program
        icnt = icnt + 1

        ! Additionne-soustrait-module-multiplit chaque pts des deux champs
        call lopascm(tmpif0, tmpif1, fact, nni * nnj * nnk)

        deallocate(tmpif1)

        if (fact .ne. 1 .or. unefois .or. once) exit
    end do
    call query%free()
end subroutine plmnmod_orig


subroutine pluss(nom, type, datev, niv, ip2, ip3, etiqet)
    ! fact = MULTIPLICATEUR POUR AJOUTER
    call plmnmod_orig(nom, type, datev, niv, ip2, ip3, etiqet, 1, 2)
end subroutine


subroutine moinse(nom, type, datev, niv, ip2, ip3, etiqet)
    ! fact = MULTIPLICATEUR POUR  SOUSTRAIRE
    call plmnmod_orig(nom, type, datev, niv, ip2, ip3, etiqet, -1, 1)
end subroutine


subroutine moinss(nom, type, datev, niv, ip2, ip3, etiqet)
    ! fact=MULTIPLICATEUR POUR  SOUSTRAIRE
    call plmnmod_orig(nom, type, datev, niv, ip2, ip3, etiqet, -1, 2)
end subroutine


subroutine modul2e(nom, type, datev, niv, ip2, ip3, etiqet)
    ! 2   fact=2 ADDITIONNER LES DEUX CHAMPS AU CARRE
    call plmnmod_orig(nom, type, datev, niv, ip2, ip3, etiqet, 2, 1)
end subroutine


subroutine modul2s(nom, type, datev, niv, ip2, ip3, etiqet)
    ! fact=2 ADDITIONNER LES DEUX CHAMPS AU CARRE
    call plmnmod_orig(nom, type, datev, niv, ip2, ip3, etiqet, 2, 2)
end subroutine


subroutine foise(nom, type, datev, niv, ip2, ip3, etiqet)
    ! fact=3 MULTIPLIER CHAQUE PT DES DEUX CHAMPS
    call plmnmod_orig(nom, type, datev, niv, ip2, ip3, etiqet, 3, 1)
end subroutine


subroutine foiss(nom, type, datev, niv, ip2, ip3, etiqet)
    ! fact=3 MULTIPLIER CHAQUE PT DES DEUX CHAMPS
    call plmnmod_orig(nom, type, datev, niv, ip2, ip3, etiqet, 3, 2)
end subroutine


subroutine divisee(nom, type, datev, niv, ip2, ip3, etiqet)
    ! fact=4 MULTIPLIER CHAQUE PT DES DEUX CHAMPS
    call plmnmod_orig(nom, type, datev, niv, ip2, ip3, etiqet, 4, 1)
end subroutine


subroutine divises(nom, type, datev, niv, ip2, ip3, etiqet)
    ! fact=4 MULTIPLIER CHAQUE PT DES DEUX CHAMPS
    call plmnmod_orig(nom, type, datev, niv, ip2, ip3, etiqet, 4, 2)
end