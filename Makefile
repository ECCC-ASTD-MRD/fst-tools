.SUFFIXES :

.SUFFIXES : .ftn90 .c .o .a

SHELL = /bin/sh

CPP = /lib/cpp

RMNLIB = $(ARMNLIB)/lib/$(EC_ARCH)$(ABI)/librmnbeta.a

FFLAGS =

CFLAGS =

#OPTIMIZ =  -debug -O 0
#OPTIMIZ = -O 3 -fast
OPTIMIZ = -O 2 -fast
#OPTIMIZ_AIX = -optf='-qsimd=auto' -optc='-qsimd=auto' -O 2
OPTIMIZ_AIX = -O 2
#OPTIMIZ_AIX = -optf='-qarch=pwr7 -qsimd=auto' -optc='-qarch=pwr7 -qsimd=auto' -O 2

CPPFLAGS = -I$(ARMNLIB)/include

MYLIB =  $(ARMNLIB)/lib/$(EC_ARCH)$(ABI)/librmn.a

.PRECIOUS: $(RMNLIB) $(MALIB)

include $(ARMNLIB)/include/makefile_suffix_rules.inc
default: obj

OBJET = f_pgsm.o c_pgsm.o

FICHIERS_CDK90 = \
accum.cdk90     chck.cdk90     ecrires.cdk90  idents.cdk90   lnkflds.cdk90  qqqfilt.cdk90\
cfldinf.cdk90   convers.cdk90  enrege.cdk90   impnone.cdk90  nivos.cdk90    symnom.cdk90\
champs.cdk90    dates.cdk90    gdz.cdk90      indptr.cdk90   packin.cdk90   tp12ig.cdk90\
champseq.cdk90  defin.cdk90    grilles.cdk90  lires.cdk90    pairs.cdk90   voir.cdk90\
charac.cdk90    dummys.cdk90   heures.cdk90   llccmm.cdk90   param.cdk90

FICHIERS_FTN90 = \
calcul.ftn90 champ.ftn90 champ_seq.ftn90 chk_hy.ftn90 chkenrpos.ftn90 chk_userdate.ftn90 chmpdif.ftn90 comme.ftn90 \
conlalo.ftn90 conver.ftn90 convs.ftn90 coord.ftn90 coupe.ftn90 coupzm.ftn90 ecrits.ftn90 \
ecritur.ftn90 epais.ftn90 fst_get_mask_key.ftn90 fillcoord.ftn90 grigaus.ftn90 grigef.ftn90 grigrib.ftn90 grille2.ftn90 \
grillps.ftn90 grilstd.ftn90 griltp4.ftn90 gristdb.ftn90 gristereo.ftn90 gritp12.ftn90 grlalon.ftn90 \
heure.ftn90 imprime.ftn90 initid.ftn90 initseq.ftn90 itrouve.ftn90 lastcol.ftn90 legvar.ftn90 \
liraxez.ftn90 liren.ftn90 lopascm.ftn90 loupmir.ftn90 lrsmdes.ftn90 macpcp.ftn90 messags.ftn90 \
metsym.ftn90 operat.ftn90 outlalo.ftn90 pairvct.ftn90 pgsm2.ftn90 pgsmabt.ftn90 pgsmlic.ftn90 \
pgsmlir.ftn90 pgsmluk.ftn90 plmnmod.ftn90 prefiltre.ftn90 putfld.ftn90 qaaqr.ftn90 qqqecho.ftn90 \
qqqfilt.ftn90 qqqform.ftn90 qqqident.ftn90 routines.ftn90 scalair.ftn90 scalair_msk.ftn90 \
setintx.ftn90 setxtrap.ftn90 sorti.ftn90 stenfilt.ftn90 symetri.ftn90 testseq.ftn90 \
uvect.ftn90 uvecteur_masque.ftn90 vdauv.ftn90 verlalo.ftn90


FICHIERS_C = \
c_pgsm.c

FICHIERS = $(FICHIERS_FTN90) $(FICHIERS_C)

f_pgsm.o: f_pgsm.ftn90
bidon.o: bidon.ftn90
c_pgsm.o : c_pgsm.c

obj: $(OBJET)
f_pgsm.ftn90: $(FICHIERS_FTN90) $(FICHIERS_CDK90)
	cat $(FICHIERS_FTN90) > f_pgsm.ftn90

#Produire les fichiers objets (.o) pour tous les fichiers

genlib: $(OBJET)
#Creer ou mettre a jour la programmatheque
	$(AR) rcv $(MYLIB) $(OBJET)

pgsm-interp-aix: f_pgsm.ftn90 c_pgsm.c
	r.compile -o $@ $(OPTIMIZ_AIX) -src bidon.ftn90 -obj ./f_pgsm.o ./c_pgsm.o $(HOME)/userlibs/$(EC_ARCH)/*.o -librmn rmnbeta_013 -libsys mass

pgsm-interp: f_pgsm.ftn90 c_pgsm.c
	r.compile -o $@ $(OPTIMIZ) -src bidon.ftn90 f_pgsm.ftn90 c_pgsm.c -librmn rmn_012 -obj $(HOME)/src/interp/*.o 

pgsm:
	r.compile -o $@ $(OPTIMIZ) -src bidon.ftn90 f_pgsm.ftn90 c_pgsm.c -librmn rmn_012

pgsm-AIX:
	r.compile -o $@ $(OPTIMIZ_AIX) -src bidon.ftn90 f_pgsm.ftn90 c_pgsm.c -librmn rmn_012 -libsys mass

clean:
#Faire le grand menage. On enleve tous les fichiers sources\ninutiles et les .o
	rm -f *.o *~ *.f *.f90 pgsm pgsm2000 pgsm89

fastclean:
	rm *.o pgsm.f

