
# Étapes pour tester `fst-tools`

1. Fichier INPUT contrôle (created with librmn 020.0.4, or a least a previous, stable version) : `input.fst`
qui contient toute sorte d'enregistrements
2. Créer les binaires `fststat`, `fstcompress`, `fstcomp`, `voir` avec librmn alpha et les mettres dans un répertoire séparé 'bin_rmn_alpha'
pour avoir les utilitaires (déjà linké avec vieux librmn)
3. `. r.load.dot rpn/utils/20231219`

## Test 1: pour tester `voir`

- `              voir -iment input.fst > voir_input_orig 2>&1`
- `bin_rmn_alpha/voir -iment input.fst > voir_input_new 2>&1`
- Comparer entre `voir_input_orig` et `voir_input_new` (avec `xxdiff` par exemple)

## Test 2: pour tester "lecture des champs et faire des stats"

- `              fststat -fst input.fst > list_stat_orig 2>&1`
- `bin_rmn_alpha/fststat -fst input.fst > list_stat_new 2>&1`
- Comparer entre `list_stat_orig` et `list_stat_new`

## Test 3: pour créer un fichier STD compressé
(pas sûre qu'il y aurait un problème à le faire s'il existe des champs déjà compressés dans le fichier de départ. - il peut se plaindre)

- `rm cmp_*.fst`
- `              fstcompress -fstin input.fst -fstout cmp_orig.fst > list 2>&1`
- `bin_rmn_alpha/fstcompress -fstin input.fst -fstout cmp_new.fst > list 2>&1`

## Test 4: pour comparer les fichiers compressés entre 'rmn 20' et 'rmn alpha'
* utiliser `fststat` de rmn 20.0.4 

- `              fststat -fst cmp_orig.fst > list_cmp_orig 2>&1`
- `              fststat -fst cmp_new.fst  > list_cmp_new 2>&1`
- `bin_rmn_alpha/fststat -fst cmp_orig.fst > lista_cmp_orig 2>&1`
- `bin_rmn_alpha/fststat -fst cmp_new.fst  > lista_cmp_new 2>&1`

- Comparer tous ces listings - devraient être identiques:
    - `xxdiff list_cmp_orig list_cmp_new`
    - `xxdiff list_cmp_new lista_cmp_new`
    - `xxdiff list_cmp_orig lista_cmp_new`
- Comparer avec `fstcomp` (pour aussi tester `fstcomp`)
    - `              fstcomp -a cmp_orig.fst -b cmp_new.fst > list_comp_orig 2>&1`
    - `bin_rmn_alpha/fstcomp -a cmp_orig.fst -b cmp_new.fst > list_comp_new 2>&1`
    - Est-ce qu'on voit tous 'zéros' pour les E-MAX, E-MOY, E-REL-MAX   E-REL-MOY et 1.0000E+00 pour C-COR dans chaque listing?

## Test 5: pour comparer les fichiers après fstuncompress

- `              fstuncompress -fstin  cmp_orig.fst -fstout uncmp_orig.fst > list 2>&1`
- `bin_rmn_alpha/fstuncompress -fstin  cmp_new.fst  -fstout uncmp_new.fst > list 2>&1`
- `              fststat -fst uncmp_orig.fst > list_uncmp_orig 2>&1`
- `              fststat -fst uncmp_new.fst  > list_uncmp_new 2>&1`
- `bin_rmn_alpha/fststat -fst uncmp_new.fst  > lista_uncmp_new 2>&1`
- `xxdiff list_uncmp_orig list_uncmp_new`
- `xxdiff list_uncmp_orig lista_uncmp_new`
- `              fstcomp -a uncmp_orig.fst -b uncmp_new.fst > list_comp_orig 2>&1`
- `bin_rmn_alpha/fstcomp -a uncmp_orig.fst -b uncmp_new.fst > list_comp_new 2>&1`

##

Tu peux toujours en savoir plus sur les options en faisant:

```
voir -h
fststat -h
fstcomp -h 
fstcompress -h
fstuncompress -h
fstcomp -h
```

##

Pour `editfst`, c'est la prochaine étape, avant `pgsm`. `editfst` ne touche pas aux valeurs dans les enregistrements mais il peut changer certains 'headers'
