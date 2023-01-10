!S/R LITZON 
!
      SUBROUTINE LITZON(IUN,IDEU,ierr,Debug)
      use app
      IMPLICIT NONE
!
!Auteur: G.Pellerin (FEV93)
!                         
!revision 001 G.PELLERIN -aou93- Ecrire les niveaux sigma dans
!                        le fichier ZONFILE pour le graphisme
!         002 G.PELLERIN -dec93- Lit les latitudes pour le modele SEF
!                        l'ecrire dans ZONFILE pour le graphisme
!         003 G.PELLERIN -dec93- Passer les latitudes pour ZONFIN
!                         pour les calculs de conversion de variables
!         004 G.Pellerin -fev94- conversion d'entier a characteres via
!                         r4astrg et strgar4 pour 64bits
!         005 G.Pellerin -fev94- augmenter Nombrec a 14
!         006 G.Pellerin (fev94) traitement differents des variables
!                                de surface et de profile
!         007 G.Pellerin (Avr2000) Introduction du common zontab 
!                                  pour compilation en FORTRAN90
!         008 B.Dugas    (mar06) Declarations pour FSTD2000, notamment
!                                dans le traitement de la variable etikex
!                                De plus, Maxbin 256 --> 999
!                                et       Maxniv  51 --> 999
!         009 K.Winger   (Nov06) - Read variable names as characters (VC/)
!                                  if they are not written as r4a (V/)
!                                - Read first record found (not necessarily ip2=1)
!                                - Do not write first record twice anymore
!                                - Pass ip2 to debalzn instead of heusav
!                                - Allow 4 character variables
!
!
!objet(LITZON)
!      reformatter les diagnostiques zonaux
!      lit dans le fichier standard NOUTZON
!
!MODULES
!
      External      DEBALZN,WRITLZN
      External      r4astrg
      External      fnom,exfin,qqexit,fstnbr,fstouv, &
                    fstluk,fstinf,fstprm,fstfrm
      Integer       ierr,fnom,exfin,fstnbr,fstouv, &
                    fstluk,fstinf,fstprm,fstfrm
      Integer       inf,iluk,inbr,iprm,nil
      Integer       ii,jj,kk,NIC,NJC,NKC

      Character     typvar*2,etiket*12,grtyp*1,nomvar*2, &
                    etikex*12,etik1*4,etik2*4,etik3*4,   &
                    etiket1*8,etiket2*4

      Integer       dateo,deet,npas,                          &
                    ip1,ip2,ip20,ip3,        datyp,nbits,     &
                    ig1,ig2,ig3,ig4,         swa,lng,dltf,ubc,&
                    extra1,extra2,extra3

      Save          dateo,deet,npas,                          &
                    ip1,ip2,ip20,ip3,        datyp,nbits,     &
                    ig1,ig2,ig3,ig4,         swa,lng,dltf,ubc,&
                    typvar,etiket,grtyp,     etikex

      Integer       IUN,IDEU
      Logical       Debug,Tourne

!    Variables needed to read variable names as characters

      Logical       varchar

!    Declarations des variables statiques.

      Integer       Maxbin
      Parameter   ( Maxbin = 999 )

      Integer       Maxniv,       MaxVar
      Parameter   ( Maxniv = 999, MaxVar = 256 )

      Integer       MaxVarP2 
      Parameter   ( MaxVarP2 = MaxVar +2 )

      Integer       MaxVarP3
      Parameter   ( MaxVarP3 = MaxVar +3 )

      Real          S(Maxniv), SH(Maxniv), Dlat(maxbin)
      Integer       var(MaxVarP3),NVAR,NSIG

      Character*8   listvar(MaxVar)
      Integer       propvar(MaxVar), posvar(0:MaxVar)

      save          listvar,propvar,posvar,NVAR,NSIG

      Integer       NombreC
      Parameter   ( NombreC = 14)

      Integer       tabctl(NombreC),KA
      Save          tabctl,KA

      Integer       NDELTAT,DELTAT,MODE,NI,NJ,NK,  &
                    NBIN,SOMNK,COMPLET,LATMIN,ROT,VIDE

!   Equivalence ( NDELTAT, tabctl(1) ),( DELTAT, tabctl(2) ),
!               ( MODE,    tabctl(3) ),( NI,     tabctl(4) ),
!               ( NJ,      tabctl(5) ),( NK,     tabctl(6) ),
!               ( NBIN,    tabctl(7) ),( SOMNK,  tabctl(8) ),
!               ( COMPLET, tabctl(9) ),( LATMIN, tabctl(10)),
!               ( ROT,     tabctl(11)),( VIDE,   tabctl(12))

      Common /zontab/ NDELTAT,DELTAT,MODE,NI,NJ,NK, &
                     NBIN,SOMNK,COMPLET,LATMIN,ROT,VIDE

!    Parametres servant a l'allocation de memoire dynamique.

      Real          temp,somx,somx2
      Pointer     ( ptemp,  temp (NBIN,SOMNK) ), &
                  ( psomx,  somx (NBIN,SOMNK) ), &
                  ( psomx2, somx2(NBIN,SOMNK) )
    
      Integer       errcod,heusav
      External      HPALLOC,HPDEALLC
      External      SETUVD0,BSORT

!---------------------------------------------------------------------
!=====================================================================
!---------------------------------------------------------------------
!
!   Verifier que IUN fait reference a un fichier de type valide. 

      inbr = fstnbr( IUN )
      If (inbr.le. 0)                                  Then
        WRITE(app_msg,6000)                   
        call app_log(APP_ERROR,app_msg)
        app_status=app_end(-1)
        Call qqexit(app_status)
      End If

!   Ouvrir le fichier.

      inbr = fstouv( IUN, 'RND' )

!   Lire l'information de controle se trouvant dans IUN.

      inf  = fstinf( IUN, NIC,NJC,NKC, -1,'CONTROLE',-1,-1,-1,'+','T/' )

      if (inf .ge. 0) then
        iluk = fstluk( tabctl, inf, NIC,NJC,NKC )
      else
        iluk = inf
      endif
                 
      if (NIC*NJC*NKC .ne. NombreC .or. iluk .lt. 0) Then
        WRITE(app_msg,6001)                   
        call app_log(APP_ERROR,app_msg)
        app_status=app_end(-1)
        Call qqexit(1)
      End If
  
!   Lire tous les parametres du fichier standard de controle.

      iprm  = fstprm(inf,dateo,deet,npas, NIC,NJC,NKC, &
                     nbits,datyp,    ip1,ip20,ip3,     &
                     typvar,nomvar,etiket,             &
                     grtyp,ig1,ig2,ig3,ig4,            &
                     swa,lng,dltf,ubc,                 &
                     extra1,extra2,extra3)
 
!   Declarer les variables de controle du fichier source.
      NDELTAT = tabctl(1) 
       DELTAT = tabctl(2)
         MODE = tabctl(3)
           NI = tabctl(4)
           NJ = tabctl(5)
           NK = tabctl(6)
         NBIN = tabctl(7)
        SOMNK = tabctl(8)
       LATMIN = tabctl(10)
          ROT = tabctl(11)
!        VIDE = tabctl(12)

!   Imprimer la table de controle du fichier source.
      print *,'LITZON - table de controle'
      print *,'=========================='
      print *,'NDELTAT=', tabctl(1) 
      print *,' DELTAT=', tabctl(2)
      print *,'   MODE=', tabctl(3)
      print *,'     NI=', tabctl(4)
      print *,'     NJ=', tabctl(5)
      print *,'     NK=', tabctl(6)
      print *,'   NBIN=', tabctl(7)
      print *,'  SOMNK=', tabctl(8)
      print *,' LATMIN=', tabctl(10)
      print *,'    ROT=', tabctl(11)
!     print *,'   VIDE=', tabctl(12)
      print *,'=========================='

!   Lire les angles selon longitudes du modele SEF

      Tourne = .TRUE.
      Do jj=1,Maxbin
        Dlat(jj)=0.
      End DO

      inf  = fstinf( IUN, NIC,NJC,NKC, -1,' ',-1,-1,-1,'+','L^'  )
      if (inf .ge. 0) then
        iluk = fstluk( Dlat, inf, NIC,NJC,NKC )
      else
        iluk = inf
      endif

!     ne sont ecrits que pour le modele sef  
      If (Debug)                                         Then
        print *,'theta =',(Dlat(ii),ii=1,NIC)
      End If

      If(iluk  .ge. 0       )                            Then
        Tourne = .FALSE.
  
        If (NIC*NJC*NKC .gt. Maxbin  )                   Then
          WRITE(app_msg,6010)                   
          call app_log(APP_ERROR,app_msg)
          app_status=app_end(-1)
          Call qqexit(10)
        End If
      End If
     
!   Lire les niveaux sigma du modele                       

      inf  = fstinf( IUN, NIC,NJC,NKC, -1,' ',-1,-1,-1, '+','S^'  )
      if (inf .ge. 0) then
        iluk = fstluk( S, inf, NIC,NJC,NKC )
      else
        iluk = inf
      endif

      if (NIC*NJC*NKC .gt. Maxniv .or. iluk .lt. 0 )                   Then
        WRITE(app_msg,6011)                   
        call app_log(APP_ERROR,app_msg)
        app_status=app_end(-1)
        Call qqexit( 11 )
      End If

!   Check si conforme au nombre dans table d'information 
 
      NSIG  = NIC
      KA    = NK-1

      if (NSIG .ne. NK)                                  Then 
        WRITE(app_msg,6012)                   
        call app_log(APP_ERROR,app_msg)
        app_status=app_end(-1)
        Call qqexit( 12 )
      End If

!   Lire  les niveaux intermediaires sinon les calcules

      inf  = fstinf( IUN, NIC,NJC,NKC, -1,' ',-1,-1,-1, '+','SH' )
      If ( inf.lt. 0)                                    Then
        CALL SETUVD0 (SH, .TRUE., S, NSIG, KA)
        Write(6,6013)
      Else
        iluk = fstluk( SH, inf, NIC,NJC,NKC )
      End If

      If (Debug)                                         Then
        print *,'sigma =',(SH(ii),ii=1,NSIG)
      End If

!   Triller par ordre decroissant ...modele coord sigma

      CALL BSORT(S,NSIG)
            
      If (Debug)                                         Then
        print *,'sigma =',(S(ii),ii=1,NSIG)
      End If


!   Lire LISTVAR. NV+3 pour l'etiquette.

      varchar = .false.

!   Read variable names as characters (datyp = 7)

      inf  = fstinf( IUN, NIC,NJC,NKC, -1,'CONTROLE ',ip1,ip20,ip3, '+','VC/' )
        
      if (inf .ge. 0) then

         iluk = fstluk( listvar, inf, NIC,NJC,NKC )
         NVAR = (NIC/8)-2

         varchar = .true.

      else

!     Read variable names as r4a format (datyp = 3)

        inf  = fstinf( IUN, NIC,NJC,NKC, -1,'CONTROLE ',ip1,ip20,ip3, '+','V/' )

        if (inf .ge. 0) then
           iluk = fstluk( var, inf, NIC,NJC,NKC )
           NVAR  = NIC/4 - 3
        else
           iluk = inf
        endif

      endif

      If (Debug)                                         Then
        nic=nic/4
        Do kk=1,NIC 
          print *,'var = ',var(kk)
        End Do
      Endif

      If (NIC.gt.MaxVarP3 .or.iluk.lt.0)               Then
        WRITE(app_msg,6002)                   
        call app_log(APP_ERROR,app_msg)
        app_status=app_end(-1)
        Call qqexit( 2 )
      End If

!   Lire POSVAR. NV+1 est contenu dans NIC.

      inf  = fstinf( IUN, NIC,NJC,NKC, -1,'CONTROLE ',ip1,ip20,ip3, '+','P/' )

      if (inf .ge. 0) then
        iluk = fstluk( posvar, inf, NIC,NJC,NKC )
      else
        iluk = inf
      endif


      If (NIC .gt. MaxVar .or. iluk.lt.0)              Then
        WRITE(app_msg,6003)                   
        call app_log(APP_ERROR,app_msg)
        app_status=app_end(-1)
        Call qqexit( 3 )
      End If

!   Imprime la liste des variables extraites dans modele.

      print *,'NVAR= ',NVAR

      posvar(0) = 1

      print *,'LITZON - Liste de variables '
      print *,'============================'
      Do II=1,NVAR
        if (.not.varchar) call r4astrg (listvar(II),var(II),0,3)
        propvar(ii) = 0
        if(listvar(ii)(1:1) .eq. '.') propvar(ii) = 1
        print *,listvar(II),propvar(II),posvar(II)
      End Do
      print *,'============================='

!  conversion d'entier a characteres pour la tikette
!  remplacer write(etikex,'(a4,a4)')tabctl(11),tabctl(12)

      if (varchar) then
        etiket1 = listvar(nvar+1)
        etiket2 = listvar(nvar+2)
        etikex = etiket1//etiket2
      else  
        call r4astrg (etik1, var(nvar+1),0,3)
        call r4astrg (etik2, var(nvar+2),0,3)
        call r4astrg (etik3, var(nvar+3),0,3)
        etikex = etik1//etik2//etik3
      endif
      print *,' ETIKEX=      ',etikex

!   ============================================================
!        de l'information a stoker  pour le graphisme
!   ============================================================

      CALL WRITLZN(IDEU,S,SH,NSIG,tabctl,NombreC, &
                  dateo,deet,npas,etikex,Dlat,NBIN,Tourne )

!   Allocation dynamique Somx et Somx2 (nbin,somnk)


      If (mod(MODE,2).eq.1) &
       Call hpalloc( psomx,    SOMNK*NBIN,    errcod,1 )

      If (MODE.ge.2) &
       Call hpalloc( psomx2,   SOMNK*NBIN,    errcod,1 )

        Call hpalloc( ptemp,    SOMNK*NBIN,    errcod,1 )
            
!   ===============================================================
!     Boucle sur les periodes sauvegardees
!   ===============================================================

      heusav = 0
      ip2    = -1
!                    ip2=1 comme si 0 heure (sauve au pas 1)
!                 si ip2=0 alors on doit sauver au pas 0 dans mzonxst      
100   CONTINUE

!   Lire Somx et Somx2 (nbin,somnk)

      If (mod(MODE,2).eq.1)                               Then

        inf  = fstinf( IUN, NIC,NJC,NKC, -1,'MZONXST ',ip1,ip2,ip3, '+','1/' )

        If ( inf .lt. 0  ) GoTo 200

!   Lire tous les parametres du fichier standard de controle.

        iprm  = fstprm( inf,dateo,deet,npas, NIC,NJC,NKC, &
                       nbits,datyp,    ip1,ip20,ip3,      &
                       typvar,nomvar,etiket,              &
                       grtyp,ig1,ig2,ig3,ig4,             &
                       swa,lng,dltf,ubc,                  &
                       extra1,extra2,extra3)

        print *,'litzon - apres lecture SOMX - ip2 = ',ip20
        ip2 = ip20

        If (Debug)                                         Then
          print *,'nic,njc,nkc            = ',nic,njc,nkc
          print *,'nbits,datyp            = ',nbits,datyp
          print *,'ip1,ip20,ip3           = ',ip1,ip20,ip3
          print *,'typvar,nomvar,etiket   = ',typvar,nomvar,etiket
        Endif  

        if (inf .ge. 0) then
          iluk = fstluk( somx, inf, NIC,NJC,NKC )
        else
          iluk = inf
        endif

        If ( iluk .lt. 0  .and. heusav .eq. 0 )            Then
          WRITE(app_msg,6008)                   
          call app_log(APP_ERROR,app_msg)
          app_status=app_end(-1)
          Call qqexit( 8 )
        End If

      End If

      If (MODE.ge.2)                                       Then

        inf  = fstinf( IUN, NIC,NJC,NKC, -1,'MZONXST ',ip1,ip2,ip3, '+','2/' )

        If ( inf .lt. 0  ) GoTo 200

        iluk = fstluk( somx2, inf, NIC,NJC,NKC )

        If ( iluk .lt. 0  .and. heusav .eq. 0 )            Then
          WRITE(app_msg,6009)                   
          call app_log(APP_ERROR,app_msg)
          app_status=app_end(-1)
          Call qqexit( 9 )
        End If


      End If

!   ============================================================
!       deballage de l'information stoker 
!   ============================================================


      heusav= heusav + 1

      CALL DEBALZN (SH,S,listvar,propvar,posvar,NombreC,         &
                   KA,NSIG,NVAR,somx,somx2,temp,NBIN,SOMNK,Dlat, &
                   dateo,deet,npas,etikex,ip2,Tourne,Debug )


      if (ip2 .eq. 1 .and. NDELTAT .ne. 1 ) then
        ip2 = ip2 + NDELTAT - 1
      else
        ip2 = ip2 + NDELTAT
      endif

      goto 100
        
200   CONTINUE

      Write(6,6014) heusav
      ierr = heusav

!   Fermer le fichier IUN

      inbr = fstfrm(IUN)

      If (inbr.lt. 0)                                      Then
        WRITE(app_msg,6015)                   
        call app_log(APP_ERROR,app_msg)
        app_status=app_end(-1)
        Call qqexit( 15 )
      End If

      Return

!---------------------------------------------------------------------
!=====================================================================
!---------------------------------------------------------------------

    4 Format(A4)

 6000 Format(' File xxxxxx is empty according to fstnbr.')
 6001 Format(' Unable to read "T/" control table or', &
            ' Incorrect control table sizes :',3I5)
 6002 Format(' Unable to read "V/" resp. "VC/" variable list or', &
            ' mismatch between expected and real dimensions.')
 6003 Format(' Unable to read "P/" variable list or', &
            ' mismatch between expected and real dimensions.')
 6004 Format(' Unable to read "S/" sin array or', &
            ' mismatch between expected and real dimensions.')
 6005 Format(' Unable to read "C/" cos array or', &
            ' mismatch between expected and real dimensions.')
 6006 Format(' Unable to read "B/" bin array or', &
            ' mismatch between expected and real dimensions.')
 6007 Format(' Unable to read "W/" weights array or', &
            ' mismatch between expected and real dimensions.')
 6008 Format(' Unable to read "1/" somx  for KOUNT =',I5)
 6009 Format(' Unable to read "2/" somx2 for KOUNT =',I5)
 6010 Format(' Unable to read "L^" latitudes in control file ')
 6011 Format(' Unable to read "S^" sigma levels  or', &
            ' Maximum number exceeded sizes :',3I5)
 6012 Format(' mismatch between expected and real dimensions:',2I3)
 6013 Format(' Unable to read "SH" sigma levels in control file ')
 6014 Format(' Nomber of time samples saved on diagnostics file ',I3)
 6015 Format(' User requests closing of I/O unit ',I3,'error')

      End
