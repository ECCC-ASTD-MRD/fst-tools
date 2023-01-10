!   S/P WRITLZN
!
      Subroutine WRITLZN(IDEU,S,SH,NSIG,tabctl,NombreC, &
                datex,delt,npax,etiket, DLAT,NLAT,Tourne)           
      use app
      IMPLICIT NONE
!
!Auteur: G.Pellerin (AUG93)
!         001 B.Dugas    (mar06) Declarations pour FSTD2000
!
!revision 001 G.PELLERIN -Dec93- Cle Tourne pour rotation de grille
!                         
!Language ftn77
!
!objet(WRITLZN)
!      ecrit les niveaux sigma et niveaux intermediaires 
!      ecrit les angles selon longitude pour graphisme
!      dans le fichier standard ZONFILE
!
!IMPLICITES
      External      qqexit
      External      fnom,exfin,fstnbr,fstouv, &
                    fstluk,fstinf,fstprm,fstecr,fstfrm
      Integer       fnom,exfin,fstnbr,fstouv, &
                    fstluk,fstinf,fstprm,fstecr,fstfrm
      Integer       ierr,ifrm,iecr,nil
      Integer       NIC,NJC,NKC
!
      Character     typvar*2,                nomvar*2, &
                    etiket*12,               grtyp*1

      Integer       dateo,deet,npas,         datex,delt,npax, &
                    ip1,ip2,ip20,ip3,        datyp,nbits,npak,&
                    ig1,ig2,ig3,ig4         

      Logical       rewrit,Vrai,Faux
      Logical       Tourne

      Save          dateo,deet,npas,         typvar,grtyp, &
                    ip1,ip2,ip20,ip3,        datyp,nbits,  &
                    ig1,ig2,ig3,ig4

      Integer       IDEU
      Data          nbits   / 24 /
      Data          Vrai    /     .true.     /, &
                    Faux    /     .false.    /
      Data          ig1,ig2,ig3,ig4          /  4 *  0 /
      Data          ip1,ip20,ip2,ip3         /  4 *  0 /
      Data          dateo,deet,npas          /  3 *  0 /
      Data          typvar,grtyp,datyp       / 'Z','L', 1 /

!     Declarations des variables statiques.

      Integer       NSIG
      Real          S(NSIG),SH(NSIG)
      Integer       NLAT
      Real          Dlat(NLAT)

      Integer       maxlat
      Parameter    ( Maxlat = 256 )
      Real          temp(maxlat)

      Integer       NombreC 
      Integer       tabctl(NombreC)

      Integer       NDELTAT,DELTAT,MODE,NI,NJ,NK,  &
                    NBIN,SOMNK,COMPLET,LATMIN

!      Equivalence ( NDELTAT, tabctl(1) ),( DELTAT, tabctl(2) ),
!                  ( MODE,    tabctl(3) ),( NI,     tabctl(4) ),
!                  ( NJ,      tabctl(5) ),( NK,     tabctl(6) ),
!                  ( NBIN,    tabctl(7) ),( SOMNK,  tabctl(8) ),
!                  ( COMPLET, tabctl(9) ),( LATMIN, tabctl(10))

!---------------------------------------------------------------------
!=====================================================================
!---------------------------------------------------------------------

!     Equivalence avec le tableau de controle.

                NDELTAT = tabctl(1)
                DELTAT  = tabctl(2)
                MODE    = tabctl(3)
                NI      = tabctl(4)
                NJ      = tabctl(5)
                NK      = tabctl(6)
                NBIN    = tabctl(7)
                SOMNK   = tabctl(8)
                COMPLET = tabctl(9)
                LATMIN  = tabctl(10)
                
!   declare le fichier cible

      ierr = fstouv(IDEU, 'RND')

!   definir les variables pour fstecr.
!
                 if(.not. Tourne ) grtyp='G'
                 dateo   = datex   
                 npas    = 0 
                 rewrit  = Faux
                 npak    = -nbits
                 deet    = DELTAT 
                 NIC     = NSIG 
                 NJC     = 1    
                 NKC     = 1    
                 ip1     = 0 
                 ip2     = 0 
                 ip3     = 0

!  Ecrit les niveaux intermediaires pour le graphisme O
!
                 nomvar  = 'S^'
                 S(1) = 1.

           iecr   = fstecr( S, temp,                    &
                           npak,IDEU,dateo,deet,npas,   &
                           NIC, NJC, NKC, ip1,ip2,ip3,  &
                           typvar,nomvar,etiket,grtyp,  &
                           ig1,ig2,ig3,ig4,datyp,       &
                           rewrit )

           If (iecr.lt.0)                           Then

                  WRITE(app_msg,6001)                   
                  call app_log(APP_ERROR,app_msg)
                  app_status=app_end(-1)
                  Call qqexit(1)

           End If

!  Ecrit les niveaux intermediaires pour le graphisme O
!
                 nomvar  = 'SH'

           iecr   = fstecr( SH, temp,                  &
                           npak,IDEU,dateo,deet,npas,  &
                           NIC, NJC, NKC, ip1,ip2,ip3, &
                           typvar,nomvar,etiket,grtyp, &
                           ig1,ig2,ig3,ig4,datyp,      &
                           rewrit )

           If (iecr.lt.0)                           Then

                  WRITE(app_msg,6002)                   
                  call app_log(APP_ERROR,app_msg)
                  app_status=app_end(-1)
                  Call qqexit( 2 )

           End If

!  Ecrit les latitudes si non regulieres pour le graphisme O
!
                 nomvar  = 'L>'
                 NIC     = NLAT 

      If (.not. Tourne )                            Then

           iecr   = fstecr( DLAT, temp,                 &
                           npak,IDEU,dateo,deet,npas,   &
                           NIC, NJC, NKC, ip1,ip2,ip3,  &
                           typvar,nomvar,etiket,grtyp,  &
                           ig1,ig2,ig3,ig4,datyp,       &
                           rewrit )

           If (iecr.lt.0)                           Then

                  WRITE(app_msg,6003)                   
                  call app_log(APP_ERROR,app_msg)
                  app_status=app_end(-1)
                  Call qqexit( 3 )

           End If

       End If


!   Fermer le fichier IDEU

      ifrm = fstfrm(IDEU)

      Return
 
!---------------------------------------------------------------------
!=====================================================================
!---------------------------------------------------------------------

 6001 Format(' Unable to write "S^" sigma before closing down.')
 6002 Format(' Unable to write "SH" sigma before closing down.')
 6003 Format(' Unable to write "L^" theta before closing down.')

      End
