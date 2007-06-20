subroutine bm_coarse_wrt(udst, z, ax, ay, nig, njg, & 
                      nomvar, typvar, etiket, ip1, ip2, ip3, dateo, deet, npas, datyp, nbits, &
                      grtyp, ig1, ig2, ig3, ig4, grref, ig1ref, ig2ref, ig3ref, ig4ref, avg)
  implicit none
  
  integer udst
  integer nig,njg
  real ax(nig)
  real ay(njg)
  real z(nig, njg)
  character*4 nomvar
  character*2 typvar
  character*12 etiket
  character*1 grtyp, grref, lgrtyp
  integer dateo, datyp, deet, npas, ig1, ig2, ig3, ig4, ig1ref, ig2ref, ig3ref, ig4ref, avg, ier

  real, dimension(:,:), allocatable :: zcoarse
  real, dimension(:), allocatable:: axcoarse, aycoarse,axcoarse_low, axcoarse_high, &
    aycoarse_low, aycoarse_high,x,y, x_low, x_high, y_low, y_high

  integer keyax
  logical rewrite_flag
  integer fstinf, fstecr, jstart, jend
  integer nbits, ip1, ip2, ip3
  integer i, j, nix, njx, nkx, imid, jmid
  integer lcl_avg

  real dxcore, dycore, dxcoarse, dycoarse
  real total_area, xtile_min, ytile_min, xtile_max, ytile_max, xfrac, yfrac, area
  integer nicoarse, njcoarse
  integer ez_cherche,ii,jj
  integer istart, iend, compression_code, usr_datyp

  imid = nig / 2
  jmid = njg / 2
  lcl_avg = avg

  dxcore = ax(imid) - ax(imid-1)
  dycore = ay(jmid) - ay(jmid-1)
  if (lcl_avg == -1) then
     dxcoarse = ax(2) - ax(1)
     dycoarse = ay(2) - ay(1)
     lcl_avg = nint(dxcoarse/dxcore)
  else
     dxcoarse = lcl_avg * dxcore
     dycoarse = lcl_avg * dycore
  endif

  if (lcl_avg == 1) then
     nicoarse = nig
     njcoarse = njg
  else
     nicoarse = nint((ax(nig)-ax(1))/dxcoarse)
     njcoarse = nint((ay(njg)-ay(1))/dycoarse)
  endif

  if (ig3 == -1) then
    ig3 = lcl_avg
  endif
    
  dxcoarse = (ax(nig)-ax(1))/(1.0*(nicoarse-1))
  dycoarse = (ay(njg)-ay(1))/(1.0*(njcoarse-1))


  allocate(zcoarse(nicoarse, njcoarse))
  allocate(axcoarse(nicoarse))
  allocate(aycoarse(njcoarse))
  allocate(axcoarse_low(nicoarse))
  allocate(axcoarse_high(nicoarse))
  allocate(aycoarse_low(njcoarse))
  allocate(aycoarse_high(njcoarse))
  allocate(x(nicoarse))
  allocate(y(njcoarse))
  allocate(x_low(nicoarse))
  allocate(y_low(njcoarse))
  allocate(x_high(nicoarse))
  allocate(y_high(njcoarse))
  

  do i=1,nicoarse
     axcoarse(i) = ax(1) + dxcoarse * (i-1)
     axcoarse_low(i) = ax(1) + dxcoarse * (1.0*i-1.5)
     ii = ez_cherche(axcoarse(i), ax, nig)
     x(i) = float(ii) + (axcoarse(i)-ax(ii))/(ax(ii+1)-ax(ii))
     ii = ez_cherche(axcoarse_low(i), ax, nig)
     x_low(i) = float(ii) + (axcoarse_low(i)-ax(ii))/(ax(ii+1)-ax(ii))
  enddo

  do i=1,nicoarse-1
     axcoarse_high(i) = axcoarse_low(i+1)
     ii = ez_cherche(axcoarse_high(i), ax, nig)
     x_high(i) = float(ii) + (axcoarse_high(i)-ax(ii))/(ax(ii+1)-ax(ii))
  enddo
  axcoarse_high(nicoarse) = ax(nig)
  x_high(nicoarse) = float(nig)
  
  do j=1,njcoarse
     aycoarse(j) = ay(1) + dycoarse * (j-1)
     aycoarse_low(j) = ay(1) + dycoarse * (1.0*j-1.5)
     jj = ez_cherche(aycoarse(j), ay, njg)
     y(j) = float(jj) + (aycoarse(j)-ay(jj))/(ay(jj+1)-ay(jj))
     jj = ez_cherche(aycoarse_low(j), ay, njg)
     y_low(j) = float(jj) + (aycoarse_low(j)-ay(jj))/(ay(jj+1)-ay(jj))
!     print *, j, aycoarse(j), jj, ay(jj), y(j)
  enddo

  do j=1,njcoarse-1
     aycoarse_high(j) = aycoarse_low(j+1)
     jj = ez_cherche(aycoarse_high(j), ay, njg)
     y_high(j) = float(jj) + (aycoarse_high(j)-ay(jj))/(ay(jj+1)-ay(jj))
  enddo
  aycoarse_high(njcoarse) = ax(njg)
  y_high(njcoarse) = float(njg)
   
  do j=2,njcoarse-1
    jstart = nint(y_low(j))
    jend   = nint(y_high(j))
    do i=2,nicoarse-1
       zcoarse(i,j) = 0.0
       total_area = 0.0
       istart = nint(x_low(i))
       iend   = nint(x_high(i))
       do jj=jstart, jend
        ytile_min = ay(jj)-0.5*(ay(jj)-ay(jj-1))
        ytile_max = ay(jj)+0.5*(ay(jj+1)-ay(jj))
        yfrac = ytile_max - ytile_min
        if (ytile_min < aycoarse_low(j)) then
            yfrac = ytile_max - aycoarse_low(j)
        endif         
        if (ytile_max > aycoarse_high(j)) then
            yfrac = aycoarse_high(j) - ytile_min
        endif         
         do ii=istart, iend
            xtile_min = ax(ii)-0.5*(ax(ii)-ax(ii-1))
            xtile_max = ax(ii)+0.5*(ax(ii+1)-ax(ii))
            xfrac = xtile_max - xtile_min
            if (xtile_min < axcoarse_low(i)) then
               xfrac = xtile_max - axcoarse_low(i)
            endif         
            if (xtile_max > axcoarse_high(i)) then
               xfrac = axcoarse_high(i) - xtile_min
            endif         
          area =  xfrac*yfrac
          total_area = total_area + area
          zcoarse(i,j) = zcoarse(i,j) + z(ii,jj) * area
         enddo
       enddo 
       zcoarse(i,j) = zcoarse(i,j)/total_area
    enddo
  enddo

! Moyenne 1e rangee

  j = 1
  jstart = 1
  jend = nint(y_low(2))
  do i=2,nicoarse-1
      zcoarse(i,j) = 0.0
      total_area = 0.0
      istart = nint(x_low(i))
      iend   = nint(x_high(i))
      do jj=jstart, jend
      
      if (jj == 1) then
        ytile_min = ay(jj)
      else
        ytile_min = ay(jj)-0.5*(ay(jj)-ay(jj-1))      
      endif
      
      ytile_max = ay(jj)+0.5*(ay(jj+1)-ay(jj))
      yfrac = ytile_max - ytile_min
      if (ytile_min < aycoarse_low(j)) then
          yfrac = ytile_max - aycoarse_low(j)
      endif         
      
      if (ytile_max > aycoarse_high(j)) then
          yfrac = aycoarse_high(j) - ytile_min      
      endif         
      
      do ii=istart, iend
        xtile_min = ax(ii)-0.5*(ax(ii)-ax(ii-1))
        xtile_max = ax(ii)+0.5*(ax(ii+1)-ax(ii))
        xfrac = xtile_max - xtile_min
        if (xtile_min < axcoarse_low(i)) then
            xfrac = xtile_max - axcoarse_low(i)
        endif         
        if (xtile_max > axcoarse_high(i)) then
            xfrac = axcoarse_high(i) - xtile_min
        endif         
      area =  xfrac*yfrac
      total_area = total_area + area
      zcoarse(i,j) = zcoarse(i,j) + z(ii,jj) * area
      enddo
    enddo 
    zcoarse(i,j) = zcoarse(i,j)/total_area
  enddo


! Moyenne rangee du haut 

  j = njcoarse
  jstart = nint(y_high(njcoarse-1))
  jend = njg
  do i=2,nicoarse-1
      zcoarse(i,j) = 0.0
      total_area = 0.0
      istart = nint(x_low(i))
      iend   = nint(x_high(i))
      do jj=jstart, jend
        if (jj == njg) then
          ytile_max = ay(njg)
        else
          ytile_max = ay(jj)+0.5*(ay(jj+1)-ay(jj))
        endif
        ytile_min = ay(jj)-0.5*(ay(jj)-ay(jj-1))
        
        yfrac = ytile_max - ytile_min
        if (ytile_min < aycoarse_low(j)) then
            yfrac = ytile_max - aycoarse_low(j)
        endif         
                
        do ii=istart, iend
          xtile_min = ax(ii)-0.5*(ax(ii)-ax(ii-1))
          xtile_max = ax(ii)+0.5*(ax(ii+1)-ax(ii))
          xfrac = xtile_max - xtile_min
          if (xtile_min < axcoarse_low(i)) then
              xfrac = xtile_max - axcoarse_low(i)
          endif         
          if (xtile_max > axcoarse_high(i)) then
              xfrac = axcoarse_high(i) - xtile_min
          endif         
          area =  xfrac*yfrac
          total_area = total_area + area
          zcoarse(i,j) = zcoarse(i,j) + z(ii,jj) * area
        enddo
    enddo 
    zcoarse(i,j) = zcoarse(i,j)/total_area
  enddo
  
! Moyenne 1e colonne

  do j=1,njcoarse
    jstart = nint(y_low(j))
    jend   = nint(y_high(j))
    if (j == 1) then
      jstart = 1
    endif
    if (j == njcoarse) then
      jend = njg
    endif
    i=1
    zcoarse(i,j) = 0.0
    total_area = 0.0
    istart = 1
    iend   = nint(x_low(2))
    do jj=jstart, jend
      if (jstart > 1) then
        ytile_min = ay(jj)-0.5*(ay(jj)-ay(jj-1))
      else
        ytile_min = ay(1)
      endif
      if (jend < njg) then
        ytile_max = ay(jj)+0.5*(ay(jj+1)-ay(jj))
      else
        ytile_max = ay(njg)
      endif
      yfrac = ytile_max - ytile_min
      if (ytile_min < aycoarse_low(j)) then
          yfrac = ytile_max - aycoarse_low(j)
      endif         
      if (ytile_max > aycoarse_high(j)) then
          yfrac = aycoarse_high(j) - ytile_min
      endif         
      do ii=istart, iend
        if (ii == 1) then
          xtile_min = ax(1)
        endif
        xtile_max = ax(ii)+0.5*(ax(ii+1)-ax(ii))
        xfrac = xtile_max - xtile_min
        if (xtile_max > axcoarse_high(i)) then
            xfrac = axcoarse_high(i) - xtile_min
        endif         
      area =  xfrac*yfrac
      total_area = total_area + area
      zcoarse(i,j) = zcoarse(i,j) + z(ii,jj) * area
      enddo
    enddo 
    zcoarse(i,j) = zcoarse(i,j)/total_area
  enddo
  
! Moyenne derniere colonne
    
  do j=1,njcoarse
    jstart = nint(y_low(j))
    jend   = nint(y_high(j))
    if (j == 1) then
      jstart = 1
    endif
    if (j == njcoarse) then
      jend = njg
    endif
    i=nicoarse
    zcoarse(i,j) = 0.0
    total_area = 0.0
    istart = nint(x_high(nicoarse-1))
    iend   = nig
    do jj=jstart, jend
      if (jstart > 1) then
        ytile_min = ay(jj)-0.5*(ay(jj)-ay(jj-1))
      else
        ytile_min = ay(1)
      endif
      if (jend < njg) then
        ytile_max = ay(jj)+0.5*(ay(jj+1)-ay(jj))
      else
        ytile_max = ay(njg)
      endif
      yfrac = ytile_max - ytile_min
      if (ytile_min < aycoarse_low(j)) then
          yfrac = ytile_max - aycoarse_low(j)
      endif         
      if (ytile_max > aycoarse_high(j)) then
          yfrac = aycoarse_high(j) - ytile_min
      endif         
      do ii=istart, iend
        xtile_min = ax(ii)-0.5*(ax(ii)-ax(ii-1))
        if (ii == nig) then
          xtile_max = ax(nig)
        else
          xtile_max = ax(ii)+0.5*(ax(ii+1)-ax(ii))
        endif
        xfrac = xtile_max - xtile_min
        if (xtile_max > axcoarse_high(i)) then
            xfrac = axcoarse_high(i) - xtile_min
        endif         
      area =  xfrac*yfrac
      total_area = total_area + area
      zcoarse(i,j) = zcoarse(i,j) + z(ii,jj) * area
      enddo
    enddo 
    zcoarse(i,j) = zcoarse(i,j)/total_area
  enddo
  

  keyax = fstinf(udst, nix, njx, nkx, -1, etiket, ig1, ig2, ig3, '  ', '>>  ')
  if (keyax.lt.0) then
     ier = FSTECR(axcoarse, axcoarse, -32, udst, dateo, deet, npas, nicoarse, 1, 1, &
          ig1, ig2, ig3, 'X ', '>>  ', etiket, grref, &
          ig1ref, ig2ref, ig3ref, ig4ref, 5, .true.)
     
     ier = FSTECR(aycoarse, aycoarse, -32, udst, dateo, deet, npas, 1, njcoarse, 1, &
          ig1, ig2, ig3, 'X ', '^^  ', etiket, grref, &
          ig1ref, ig2ref, ig3ref, ig4ref, 5, .true.)
     
  endif
  
  call bemol_get_compression_code(compression_code)
  if (compression_code == -1) then
     usr_datyp = datyp
  else
    if (compression_code == 1) then
      if (datyp < 10) then
        usr_datyp = datyp+128
      else
        usr_datyp = datyp
      endif
     if (usr_datyp == 129) then
      usr_datyp = 134
     endif
    else
      if (datyp > 128) then
        usr_datyp = datyp-128
        if (usr_datyp == 6) then
          usr_datyp = 1
        endif
      else
        usr_datyp = datyp
      endif
    endif
  endif


  lgrtyp = grtyp
  if (lgrtyp.eq.'#') then
     lgrtyp = 'Z'
  endif
     

  ier = FSTECR(zcoarse, zcoarse, -nbits, udst, dateo, deet, npas, nicoarse, njcoarse, &
       1, ip1, ip2, ip3, typvar, nomvar, etiket, lgrtyp, &
       ig1, ig2, ig3, ig4, usr_datyp, rewrite_flag)

  deallocate(zcoarse)
  deallocate(axcoarse)
  deallocate(aycoarse)
  deallocate(axcoarse_low)
  deallocate(axcoarse_high)
  deallocate(aycoarse_low)
  deallocate(aycoarse_high)
  deallocate(x)
  deallocate(y)
  deallocate(x_low)
  deallocate(y_low)
  deallocate(x_high)
  deallocate(y_high)
  return
end subroutine bm_coarse_wrt
