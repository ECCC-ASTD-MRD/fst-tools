      program melange
      implicit none

!     Compilation
!       r.compile -src fusion.ftn90 -o fusion -librmn

!     Usage
!       ./fusion -pilote fichier_pilotage -agregat fichier_agregat -fusion fichier_fusion

      integer iun_pilote,iun_agregat,iun_fusion, key1, key2
      integer dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1,ip2,ip3
      integer ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,nmax
      integer extra1,extra2,extra3,datev
      integer fstinf,fstprm,fstinl,fstecr,fstouv,fnom,fstluk,fstfrm
      integer ipos, ier, ni1,nj1,nk1,infon,i,j,n
      parameter (NMAX=65536)
      integer liste(NMAX)
      logical rewrite_flag
      external fstinf,fstprm,fstecr,ccard,fstinl,fstouv,fnom,fstluk

      integer low_i, low_j, high_i, high_j

      real, allocatable, dimension(:,:) :: pilote, agregat, fusion
      character *12 etiket
      character *4 nomvar
      character *1 grtyp, typvar

      character*128 cle(3)
      character*128 def(3),val(3)

      data cle /'pilote.', 'agregat.', 'fusion.'/
      data def /'void',    'void',  'void' /
      data val /'void',    'void',  'void' /

! Definition de la sous-fenetre a fusionner.
! Ces parametres doivent etre connus d'avance
      
      low_i = 3
      low_j = 9
      high_i = 32
      high_j = 21
      
! Recuperation des arguments d'appel (noms des fichiers)      
      ipos = 0
      call ccard(cle,def,val, 3, ipos)
      
! Assignation des unites Fortran aux fichiers      
      iun_pilote = 10
      iun_agregat = 11
      iun_fusion = 12
      
      
! Ouverture des fichiers

      rewrite_flag = .false.
      ier = fnom(iun_pilote,val(1),'STD+RND+R/O+OLD',0)
      print *,'Debug fnom=',ier
      ier = fnom(iun_agregat,val(2),'STD+RND+R/O+OLD',0)
      print *,'Debug fnom=',ier
      ier = fnom(iun_fusion, val(3), 'STD+RND+R/W', 0)
      
      ier = fstouv(iun_pilote,'RND')
      ier = fstouv(iun_agregat,'RND')
      ier = fstouv(iun_fusion,'RND')
      
! Initialisation de la liste des enregistrements a traiter.
! En mettant tout a -1, on traite tout le contenu du fichier d'entree      

      key1 = fstinl (iun_pilote,ni1,nj1,nk1,-1,' ',-1,-1,-1,' ',' ',liste,infon,NMAX)
      print *,'Debug infon=',infon
      do n=1,infon
        key1=liste(n)
! Recuperation des parametres venant des enregistrements pilote        
        ier = fstprm(key1,dateo,deet, npas, ni, nj, nk, nbits, datyp, ip1,&
                     ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2, ig3,&
                     ig4, swa, lng, dltf, ubc, datev, extra2, extra3)
        if (ier < 0) then
          print *,'Debug fstprm errno',ier
          exit
        endif

        allocate(pilote(ni,nj))
        allocate(agregat(ni,nj))
        allocate(fusion(ni,nj))
        ier = fstluk(pilote,key1,ni,nj,nk)
        if (ier < 0) then
          print *,'Debug fstluk errno',ier
          exit
        endif

        
!       On recherche dans le fichier agregat l'enregistrement equivalent.
!       Tout doit etre identique sauf l'etiquette.
!        
        if (ni.gt.1.and.nj.gt.1) then
          key2 = fstinf(iun_agregat,ni,nj,nk,datev, ' ', ip1, ip2, ip3, typvar, nomvar)
          ier = fstluk(agregat,key2,ni,nj,nk)
          if (ier < 0) then
            print *,'Debug fstluk errno',ier
            exit
          endif

!         On remplace le contenu de l'enregistrement "pilote" par celui de la fenetre "agregat"

          fusion = pilote
          do j=low_j,high_j
            do i=low_i,high_i
              fusion(i,j) = agregat(i,j)
            enddo
          enddo
        endif
        
! On ecrit le tout

        if (ni.eq.1.or.nj.eq.1) then
           nbits = 32
           datyp=5
           ier = FSTECR(pilote, pilote, -nbits, iun_fusion, dateo, deet, npas, ni, nj,&
                        nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2,&
                        ig3, ig4, datyp, rewrite_flag)
        else
           ier = FSTECR(fusion, fusion, -nbits, iun_fusion, dateo, deet, npas, ni, nj,&
                        nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2,&
                        ig3, ig4, 1, rewrite_flag)
        endif
        if (ier < 0) then
          print *,'Debug fstecr errno',ier
          exit
        endif
        deallocate(pilote)
        deallocate(agregat)
        deallocate(fusion)
      enddo

!      ier = fstvoi(iun_out,'RND')
      ier = fstfrm(iun_pilote)
      ier = fstfrm(iun_agregat)
      ier = fstfrm(iun_fusion)

      stop
      end
