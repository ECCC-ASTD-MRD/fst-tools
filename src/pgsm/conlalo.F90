!
!**S/P CONLALO   CALCUL LAT LONG DE CHAQUE PT D'UNE GRILLE TYPE "Y" OU "Z"
!
      subroutine conlalo(lat,lon,ni,nj,grtyp,grtypxy,ig1,ig2,ig3,ig4)
         use app
         implicit none
      
      external conlal2
      
      integer lat,lon,ni,nj,grtyp,grtypxy,ig1,ig2,ig3,ig4
      character*1 cgrtyp, cgtypxy
      
      write(cgrtyp    , '(A1)') grtyp
      write(cgtypxy, '(A1)') grtypxy
      write(6,101) cgrtyp, cgtypxy
 101  format(' CONLALO:','CGRTYP: ',a1, 'CGTYPXY: ', a1)
      
      
      call conlal2(lat,lon,ni,nj,cgrtyp,cgtypxy,ig1,ig2,ig3,ig4)
      
      
      return
      end
      subroutine conlal2(lat,lon,ni,nj,cgrtyp,cgtypxy,ig1,ig2,ig3,ig4)
!     
         use app
         implicit none
!     
!AUTEUR   - P. SARRAZIN JANVIER 87 DRPN DORVAL P.Q. CANADA
!
!LANGAGE - RATFOR
!
!OBJET(CONLALO)
!          CALCULER LA LATITUDE ET LA LONGITUDE DE TOUS LES POINTS
!          DE LA GRILLE DE SORTIE DE TYPE "Y" OU "Z"
!
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!------------------------------------------------------
      external cigaxg,llfxy,pgsmabt,messags
!     
      integer ni,nj,ig1,ig2,ig3,ig4,i,j,hem
      real lat(ni,nj),lon(ni,nj),dlat,lat0,dlon,lon0
      real pii,pjj,d60,dgrw,buflat,buflon,dla,dlo 
      real xlat1,xlon1,xlat2,xlon2
!     
      character*1 cgrtyp, cgtypxy
      
!     
      if (cgrtyp.eq.'Z') then
!
!   ATTENTION BOUCLE SUIVANTE NJ PERMET D'AVOIR DES NI>NJ SANS PROBLEME
!
         do j=nj,1,-1
            lat(1,j)=lat(j,1)
         enddo
!     
         do i=1,ni
            do j=1,nj 
               lat(i,j)=lat(1,j)
               lon(i,j)=lon(i,1)
            enddo
         enddo
      endif
!     
      hem=1
      if (cgtypxy.eq.'S') hem=2 
!     
      if (cgtypxy.eq.'N'.or.cgtypxy.eq.'S')  then
         call cigaxg(cgtypxy,pii,pjj,d60,dgrw,ig1,ig2,ig3,ig4)
!     
         do i=1,ni
            do j=1,nj
               buflat=lat(i,j) - pjj
               buflon=lon(i,j) - pii
               call llfxy(dla,dlo,buflon,buflat,d60,dgrw,hem)
               if (dlo.le.0.0) dlo=dlo + 360.0
               lat(i,j)=dla
               lon(i,j)=dlo
            enddo
         enddo
      else if (cgtypxy.eq.'L') then
         call cigaxg(cgtypxy,lat0,lon0,dlat,dlon,ig1,ig2,ig3,ig4)
!     
         do i=1,ni
            do j=1,nj
               lat(i,j) = lat(i,j)*dlat + lat0 
               lon(i,j) = lon(i,j)*dlon + lon0 
            enddo
         enddo
      else
         call app_log(APP_ERROR,'conlalo: GRILLE not "N","S","L"')
         call pgsmabt
      endif
!     
      return
      end
      subroutine conlale(lat,lon,latg,long,ni,nj,                    cgrtyp,cgtypxy,ig1,ig2,ig3,ig4)
         use app
         implicit none
!
!AUTEUR   - Y. Chartier DRPN Dorval Avril 94
!
!LANGAGE - RATFOR
!
!OBJET(CONLALO)
!          CALCULER LA LATITUDE ET LA LONGITUDE DE TOUS LES POINTS
!          DE LA GRILLE DE SORTIE DE TYPE "Y" OU "Z"
!
!
!LIBRAIRIES
!         -SOURCE  ARMNSRC,DRPN
!         -OBJET   PGSMLIB,ID=ARMNPJS.
!
!------------------------------------------------------
      external cigaxg,llfxy,pgsmabt,messags
!
      integer ni,nj,ig1,ig2,ig3,ig4,i,j,hem
      real lat(ni,nj),lon(ni,nj),latg(ni,nj),long(ni,nj),dlat,      lat0,dlon,lon0
      real pii,pjj,d60,dgrw,buflat,buflon,dla,dlo 
      real xlat1,xlon1,xlat2,xlon2
!     
      character*1 cgrtyp, cgtypxy
  
!
      if (cgrtyp.eq.'Z') then
         do j=nj,1,-1
            lat(1,j)=lat(j,1)
         enddo

         do i=1,ni 
            do j=1,nj  
               latg(i,j)=lat(1,j)
               long(i,j)=lon(i,1)
            enddo
         enddo
!
         call cigaxg(cgtypxy,xlat1,xlon1,xlat2,xlon2,ig1,ig2,ig3,ig4)
         call ez_gfllfxy(lon,lat,long,latg,ni*nj,xlat1,xlon1,xlat2,xlon2)
      else 
         call app_log(APP_ERROR,'conlalo: GRILLE not "E"')
         call pgsmabt
      endif
!     
      return
      end
      

