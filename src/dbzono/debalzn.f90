! S/P DEBALZN
!
      Subroutine debalzn (SH,S,listvar,propvar,posvar,NombreC,      &
                        KA,NSIG,NVAR,somx,somx2,temp,NBANDE,NSOMNK, &
                        theta,datex,delt,npax,etiket,heusav,        &
                        Tourne,Debug)
      use app
      IMPLICIT NONE
!
!Auteur: G.Pellerin (FEV93)
!                         
!revision 001 G.pellerin -Jun93- Passer les niveaux sigma pour calcul
!                                des coefficients KT,KM
!         002 G.PELLERIN -Jui93- Routine ZONFIN pour conversion des
!                                variables
!         003 G.PELLERIN -Aou93- Offset pour graphique NINT (IG3)    
!         004 G.PELLERIN -Dec93- Parametres de grille pour modele SEF
!         005 G.PELLERIN -Dec93- Les angles de la grille gaussienne
!         006 G.PELLERIN -Jan95- Enlever routine zonfin (phys3.11)
!         007 G.PELLERIN -Jan95- Traitement du carre des variables 
!         008 G.PELLERIN -Avril(2000) Common zontab pour tabctl     
!         009 K.WINGER   -Nov06- NDELTAT*heusav -> heusav
!                              - Allow 4 character variables
! 
!Language ftn77
!
!objet(DEBALZN)
!      reformatter les diagnostiques zonaux
!      ecrit dans le fichier standard ZONFILE
!
!IMPLICITES
      External      outfld,qqexit
      External      fnom,exfin,fstnbr,fstouv,           &
                    fstluk,fstinf,fstprm,fstecr,fstfrm
      Integer       fnom,exfin,fstnbr,fstouv,           &
                    fstluk,fstinf,fstprm,fstecr,fstfrm  
      Integer       ierr,ifrm,iecr,nil
      Integer       ii,jj,kk,NIC,NJC,NKC

      Character     typvar*2, sqrvar*1,        nomvar*4, &
                    etiket*(*),                grtyp*1

      Integer       dateo,deet,npas,         datex,delt,npax,  &
                    ip1,ip2,ip20,ip3,        datyp,nbits,npak, &
                    ig1,ig2,ig3,ig4         

      Logical       rewrit,Vrai,Faux
      Logical       Tourne,Debug

      Save          dateo,deet,npas,         typvar,grtyp,  &
                    ip1,ip2,ip20,ip3,        datyp,nbits,   &
                    ig1,ig2,ig3,ig4

      Integer       IDEU
      Data          IDEU,nbits   / 37 ,24 /
      Data          Vrai      /.true. /, &
                    Faux      /.false./
      Data          datyp      / 1 /
      Data          ig1,ig2,ig3,ig4          /  4 *  0 /
      Data          ip1,ip20,ip2,ip3         /  4 *  0 /
      Data          dateo,deet,npas          /  3 *  0 /
      Data          typvar,grtyp,sqrvar      / 'Z','L','V' /

!     Declarations des variables statiques.

      Integer       Deb,Fin,Lon,HEUSAV
      Integer       NBANDE,NSOMNK 
      Integer       NVAR, NSIG, KA
      Real          S(NSIG),SH(NSIG)
      Real          theta(NBANDE)   

      Character*8   listvar(NVAR)
      Integer       propvar(NVAR), posvar(0:NVAR)

      Integer       NombreC 
      Integer       tabctl(NombreC)

      Integer       NDELTAT,DELTAT,MODE,NI,NJ,NK, & 
                    NBIN,SOMNK,COMPLET,LATMIN,ROT,VIDE    

!    Equivalence ( NDELTAT, tabctl(1) ),( DELTAT, tabctl(2) ),
!                ( MODE,    tabctl(3) ),( NI,     tabctl(4) ),
!                ( NJ,      tabctl(5) ),( NK,     tabctl(6) ),
!                ( NBIN,    tabctl(7) ),( SOMNK,  tabctl(8) ),
!                ( COMPLET, tabctl(9) ),( LATMIN, tabctl(10)),
!                ( ROT,     tabctl(11)),( VIDE,   tabctl(12))

      Common /zontab/ NDELTAT,DELTAT,MODE,NI,NJ,NK, &
                     NBIN,SOMNK,COMPLET,LATMIN,ROT,VIDE

      Real somx(NBANDE,NSOMNK),somx2(NBANDE,NSOMNK),temp(NBANDE,NSOMNK)
      Real DLAT, DLATM
!---------------------------------------------------------------------
!=====================================================================
!---------------------------------------------------------------------


!                etiket = 'ECHANTIL'
!                print *,'etiket= ',etiket     

!     declare le fichier cible

      ierr = fstouv(IDEU, 'RND')


!   definir les variables pour fstecr.
                 dateo   = datex   
                 npas    = npax 
                 rewrit  = Faux
                 npak    = -nbits
                 deet    = DELTAT 
                 DLAT    = (90.-latmin)/NBIN
                 DLATM   = latmin
!   ========================================
!   Boucle sur les variables echantillonnees
!   ========================================
      posvar(0) = 1
      Do II=1,NVAR
          nomvar = listvar(II)(2:3) 
          if ((listvar(II)(4:4) .ge. '0' .and. &
              listvar(II)(4:4) .le. '9' ) .or. &
             (listvar(II)(4:4) .ge. 'A' .and.  &
              listvar(II)(4:4) .le. 'Z' ) .or. &
             (listvar(II)(4:4) .ge. 'a' .and.  &
              listvar(II)(4:4) .le. 'z' )) then
            nomvar(3:3) = listvar(II)(4:4)
            if ((listvar(II)(5:5) .ge. '0' .and. &
                listvar(II)(5:5) .le. '9' ) .or. &
               (listvar(II)(5:5) .ge. 'A' .and.  &
                listvar(II)(5:5) .le. 'Z' ) .or. &
               (listvar(II)(5:5) .ge. 'a' .and.  &
                listvar(II)(5:5) .le. 'z' )) then
              nomvar(4:4) = listvar(II)(5:5)
            endif
          endif

                   if(Debug) then
                   print *,'nom= ',nomvar
                   Endif
                 Deb  = posvar(ii-1)
                 Fin  = posvar(ii) - 1
                 Lon  = posvar(ii) - posvar(ii-1)

               NIC  = NBIN 
               NJC  = Lon 
               NKC  = 1

                 ip1  = 0 
                   if(Lon.gt.1)  ip1 = 1
                 ip2  = heusav/(3600/DELTAT)
                 ip3  = heusav

               IG1  = NINT(DLAT*100)
               ig2  = max(heusav-NDELTAT,0) 
                    if(propvar(ii).eq.1) ig2 = 0 
               ig3  = (90.+ LATMIN)*100
               IG3  = NINT((90.+LATMIN+0.5*DLAT)*100)
               ig4  = 0
                 If(.NOT. TOURNE)                          Then 
                   grtyp='G'
                   ig1  = 1 
                   ig2  = 0 
                   ig3  = 0 
                 Endif
!  
!     Traitement des somme, du carre des sommes ou des deux

      If(mod(MODE,2).eq.1)                                Then
      
!   Copier et inverser le champ 
!
      If(Lon.gt.1)                           Then
        Do kk=Deb,Fin
           Do jj=1,nbin
             temp(jj,kk)=somx(jj,Fin-kk+Deb)
           End DO
        End DO
      Else             
      
        Do kk=Deb,Fin
           Do jj=1,nbin
             temp(jj,kk)=somx(jj,kk)
           End DO
        End DO
      Endif

!   Copier dans le fichier zonfile
!
           if(Debug)                                Then
                call outfld(nomvar,somx(1,Deb),nbin,Lon)
           Endif

           iecr   = fstecr( temp(1,Deb), somx,    &
                      npak,IDEU,dateo,deet,npas,  &
                      NIC, NJC, NKC, ip1,ip2,ip3, &
                      typvar,nomvar,etiket,grtyp, &
                      ig1,ig2,ig3,ig4,datyp,      &
                      rewrit )

           If (iecr.lt.0)                           Then
 
                WRITE(app_msg,6001)                   
                call app_log(APP_ERROR,app_msg)
                app_status=app_end(-1)
                Call qqexit(1)

           End If

!   Idem pour le carre des variables
 
      Elseif(MODE.ge.2)                            Then   
     
!   Conversion en variance des variables
!         Attn. il faut traiter le typvar autrement???
!
      if(Lon.gt.1)                           Then
        Do kk=Deb,Fin
           Do jj=1,nbin
             temp(jj,kk)=somx2(jj,Fin-kk+Deb)-(somx(jj,Fin-kk+Deb))**2
           End DO
        End DO
                   
      Else
        Do kk=Deb,Fin
           Do jj=1,nbin
             temp(jj,kk)=somx2(jj,kk)-(somx(jj,kk))**2
           End DO
        End DO
      Endif

!   Conversion en ecart-type des variables
!
      if(Lon.gt.1)                           Then
        Do kk=Deb,Fin
           Do jj=1,nbin
             somx2(jj,kk)=temp(jj,kk)
           End DO
        End DO
                   
      Else
        Do kk=Deb,Fin
           Do jj=1,nbin
             somx2(jj,kk)=temp(jj,kk)
           End DO
        End DO
      Endif

           if(Debug)                                Then
                call outfld(nomvar,somx2(1,Deb),nbin,Lon)
           Endif
   
           iecr   = fstecr( somx2(1,Deb), temp,   &
                      npak,IDEU,dateo,deet,npas,  &
                      NIC, NJC, NKC, ip1,ip2,ip3, &
                      sqrvar,nomvar,etiket,grtyp, &
                      ig1,ig2,ig3,ig4,datyp,      &
                      rewrit )

           If (iecr.lt.0)                           Then

              WRITE(app_msg,6002)                   
              call app_log(APP_ERROR,app_msg)
              app_status=app_end(-1)
              Call qqexit(2)

           End If
 
      End IF
     
      End Do

!         Fermer le fichier IDEU

                ifrm = fstfrm(IDEU)


      Return
 
!---------------------------------------------------------------------
!=====================================================================
!---------------------------------------------------------------------

    4 Format(A4)

 6001 Format(' Unable to write "1/" somx before closing down.')
 6002 Format(' Unable to write "2/" somx2 before closing down.')
 6003 Format(' Fnom error on file xxxxxx.')
 6004 Format(' File xxxxxx is empty according to fstnbr.')
 6005 Format(' No more available slices at this point.')

      End
