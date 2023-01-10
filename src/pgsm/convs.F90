!
!**   s/p convs, batir une table avec noms,ecart,facteur,bas,haut
!
      subroutine convs(nom,  ecart,  facteur, bas, haut)
         use app
         implicit none
!
!auteur p. sarrazin mai 82 drpn dorval quebec canada
!
!     langage ratfor
!
!     objet(convs)
!     la directive convs assigne a chaque table la valeur appropriee
!     les tables augmentent a chaque appel convs
!
!arguments
!     in    nom    nom du champ que l on veut modifier
!     in    ecart  valeur plmnmodr au champ
!     in    facteur valeur utiliser pour multiplication
!     champ(i,j)=(champ(i,j) + ecart)*facteur
!     in    bas    valeur < bas auront la valeur de bas
!     in    haut   valeur du champ > haut auront la valeur de haut
!
!implicites
!     appel   via directive
!     conv(nom,ecart,facteur,bas,haut)
!
!     messages
!     plus de 40 changements d'echelle routine convs
!
!   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
#include "voir.cdk90"
#include "convers.cdk90"
#include "dummys.cdk90"
!
!     trouver si le nom existe
!     oui- remplacer
!     non- plmnmodr
!
      external fstcvt
      integer  nom,i,ier, fstcvt
      character*4 cnom, cnoma
      real bas,ecart,facteur,haut
!
      write(cnom,'(A4)') nom

      cnoma = cnom
      i = 1
 10   if (i.le.nomb) then
         if (cnoma.ne.nomss(i)) then
            i = i + 1
            goto 10
         endif
      endif
!
!     definir nomb danger si plus grand que 40
!
      nomb = max0(nomb,i)
      if (nomb.lt.256) then
         hauts(i)=1.e+30
         bass(i)= -hauts(i)
         ecarts(i) = ecart
         facts(i) = facteur
         nomss(i) = cnoma
         if (ncon.ge.4) bass(i)=bas
         if (ncon.eq.5) hauts(i)=haut
      else
         if (message) then
            call app_log(APP_WARNING,'convs: More than 256 directives, ignoring others')
         endif
      endif
      return
      end

