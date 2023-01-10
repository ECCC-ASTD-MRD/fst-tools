!**s/p qqqintx  choisir le degre de l'interpolation
   subroutine qqqintx(ordre)
   use app
   implicit none
   integer ordre
!
!auteur  Y.Chartier Dec 91
!    Utilise le nouveau dispatcher de fscint.f pour ajuster
!    le degre d'interpolation
!
!
!  e      ordre     ordre de l'interpolation 0,1, ou 3
!
!implicites
!*
character*8 op
integer ezsetopt
integer ier

   select case (ordre)
   case (100)
      ier = ezsetopt('interp_degree', 'nearest')
   case (1)
      ier = ezsetopt('interp_degree', 'linear')
   case (3)
      ier = ezsetopt('interp_degree', 'cubic')
   case (4)
      ier = ezsetopt('interp_degree', 'average')
   case (5)
      ier = ezsetopt('interp_degree', 'sph_average')
   case default
      call app_log(APP_WARNING,'qqqintx:Wrong interpolation value, should be 0, 1 or 3, will default to 3')
      ier = ezsetopt('interp_degree','cubic')
   end select

   return
   end
