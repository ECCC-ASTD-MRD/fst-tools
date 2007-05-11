  integer nmax, nparams, cle_dst, cle_core, cle_coarse, cle_missing, cle_nbits, &
    cle_ig3core, cle_ig3coarse, cle_compression, cle_avg, cle_quiet, lu_src, lu_dst, lu_core, lu_coarse, &
    cle_nistart, cle_niend, cle_ni, cle_njstart, cle_njend, cle_nj, cle_grtyp
    
  parameter (nmax = 524288)
  parameter (cle_dst         = 121)
  parameter (cle_coarse      = 122)
  parameter (cle_core        = 123)
  parameter (cle_missing     = 124)
  parameter (cle_compression = 125)
  parameter (cle_nbits     = 126)
  parameter (cle_ig3core   = 127)
  parameter (cle_ig3coarse = 128)
  parameter (cle_avg       = 129)
  parameter (cle_quiet     = 130)
  parameter (cle_nistart   = 131)
  parameter (cle_niend     = 132)
  parameter (cle_ni        = 133)
  parameter (cle_njstart   = 134)
  parameter (cle_njend     = 135)
  parameter (cle_nj        = 136)
  parameter (cle_grtyp     = 137)
    
  parameter (nparams       = 137)

  parameter (lu_src    = 10)
  parameter (lu_dst    = 151)
  parameter (lu_core   = 152)
  parameter (lu_coarse = 153)

! Fill codes

  integer abort, minimum, maximum, missing
  
  parameter (abort       = 13)
  parameter (minimum     =  1)
  parameter (maximum     =  2)
  parameter (missing     =  3) 

    
