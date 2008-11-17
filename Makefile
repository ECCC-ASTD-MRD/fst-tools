include /usr/local/env/armnlib/include/$(ARCH)$(ABI)/Makefile_addons

.SUFFIXES : .ftn .f .c .o

SHELL = /bin/sh

CPP = /lib/cpp

RMNLIB = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmnbeta.a

FFLAGS =

CFLAGS =

OPTIMIZ = -O 2
OPTIMIZ =  -debug -O 0

CPPFLAGS = -I$(ARMNLIB)/include

MYLIB =  $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmn.a

.PRECIOUS: $(RMNLIB) $(MALIB)

default: obj

.ftn.o:
	r.compile $(OPTIMIZ) -opt "=$(FFLAGS)" -src $*.ftn

.f.o:
	r.compile $(OPTIMIZ) -opt "=$(FFLAGS)" -src $*.f

.c.o:
	r.compile $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

OBJET = f_pgsm.o c_pgsm.o

FICHIERS_CDK90 = \
accum.cdk90     chck.cdk90     ecrires.cdk90  idents.cdk90   lnkflds.cdk90  qqqfilt.cdk90\
cfldinf.cdk90   convers.cdk90  enrege.cdk90   impnone.cdk90  nivos.cdk90    symnom.cdk90\
champs.cdk90    dates.cdk90    gdz.cdk90      indptr.cdk90   packin.cdk90   tp12ig.cdk90\
champseq.cdk90  defin.cdk90    grilles.cdk90  lires.cdk90    pairs.cdk90   voir.cdk90\
charac.cdk90    dummys.cdk90   heures.cdk90   llccmm.cdk90   param.cdk90

FICHIERS_FTN90 = \
calcul.ftn90 champ.ftn90 champ_seq.ftn90 chk_hy.ftn90 chkenrpos.ftn90 chmpdif.ftn90 comme.ftn90 \
conlalo.ftn90 conver.ftn90 convs.ftn90 coord.ftn90 coupe.ftn90 coupzm.ftn90 ecrits.ftn90 \
ecritur.ftn90 epais.ftn90 fst_get_mask_key.ftn90 fillcoord.ftn90 grigaus.ftn90 grigef.ftn90 grigrib.ftn90 grille2.ftn90 \
grillps.ftn90 grilstd.ftn90 griltp4.ftn90 gristdb.ftn90 gristereo.ftn90 gritp12.ftn90 grlalon.ftn90 \
heure.ftn90 imprime.ftn90 initid.ftn90 initseq.ftn90 itrouve.ftn90 lastcol.ftn90 legvar.ftn90 \
liraxez.ftn90 liren.ftn90 lopascm.ftn90 loupmir.ftn90 lrsmdes.ftn90 macpcp.ftn90 messags.ftn90 \
metsym.ftn90 operat.ftn90 outlalo.ftn90 pairvct.ftn90 pgsm2.ftn90 pgsmabt.ftn90 pgsmlic.ftn90 \
pgsmlir.ftn90 pgsmluk.ftn90 plmnmod.ftn90 prefiltre.ftn90 putfld.ftn90 qaaqr.ftn90 qqqecho.ftn90 \
qqqfilt.ftn90 qqqform.ftn90 qqqident.ftn90 routines.ftn90 scalair.ftn90 \
setintx.ftn90 setxtrap.ftn90 sorti.ftn90 stenfilt.ftn90 symetri.ftn90 testseq.ftn90 \
uvect.ftn90 vdauv.ftn90 verlalo.ftn90


FICHIERS_C = \
c_pgsm.c

FICHIERS = $(FICHIERS_FTN90) $(FICHIERS_C)

f_pgsm.ftn90: $(FICHIERS_FTN90) $(FICHIERS_CDK90)
	cat $(FICHIERS_FTN90) > f_pgsm.ftn90

obj: $(OBJET)
#Produire les fichiers objets (.o) pour tous les fichiers

genlib: $(OBJET)
#Creer ou mettre a jour la programmatheque
	$(AR) rcv $(MYLIB) $(OBJET)

pgsmflib:
	r.build -o pgsm -libpath $(PGSM) -libappl pgsm -librmn rmnbeta -bidon -main pgsm

pgsm:
	r.build -o $@ -obj *.o -bidon -main pgsm -librmn rmn_009

pgsm_007:
	r.build -o $@ -obj *.o -bidon -main pgsm -libappl dies -librmn rmn_007

pgsm2009: f_pgsm.ftn90 c_pgsm.c
	r.compile -o $@ -src f_pgsm.ftn90 c_pgsm.c -bidon -main pgsm -librmn rmnalpha

pgsm2008: f_pgsm.ftn90 c_pgsm.c
	r.compile -o $@ -src f_pgsm.ftn90 c_pgsm.c -obj $(HOME)/src/interp/*.o -bidon -main pgsm -librmn rmn_rc010

pgsm2007:
	r.build -o $@ -obj *.o  $(HOME)/src/isi4/*.o -bidon -main pgsm -librmn rmnbeta

pgsm2006:
	r.build -o $@ -obj *.o  $(HOME)/src/interp/*.o -bidon -main pgsm -librmn rmnbeta

pgsm2002:
	r.build -o $@ -obj *.o $(HOME)/userlibs/$(ARCH)/*.o -bidon -libappl dies -main pgsm -librmn rmn_rc008

pgsm2000:
	r.build -o $@ -obj *.o $(HOME)/userlibs/$(ARCH)/*.o -libappl dies -librmn rmn_007

pgsm89:
	r.build -o $@ -obj *.o -libappl dies -librmn rmn_007 -fstd89

pgsmnew: c_pgsm.o
	r.build -o pgsm -obj *.o /users/dor/armn/lib/public/xdf98.o  -libpath $(PGSM)/lib/$(ARCH)$(ABI) -libappl dies efence -librmn rmnbeta

# 	r.build -o pgsm -obj *.o -libpath $(PGSM)/lib/$(ARCH)$(ABI) -libappl dies -librmn rmnbeta


pgsm-stereo:
	r.build -o pgsm -obj *.o -libpath $(PGSM)/lib/$(ARCH)$(ABI) -libappl dies -librmn rmnbeta

pgsm-exp:
	r.build -o pgsm -obj *.o -libpath $(PGSM)/lib/$(ARCH)$(ABI) -libappl dies -librmn rmnbeta

pgsm-debug:
	r.build -o pgsm -obj *.o $(HOME)/src/interp/*.o -libpath $(PGSM)/lib/$(ARCH)$(ABI) -libappl dies -librmn rmnbeta

pgsm-debug89:
	r.build -o pgsm89 -obj *.o $(HOME)/src/interp/*.o -libpath $(PGSM)/lib/$(ARCH)$(ABI) -libappl dies -librmn rmn_005 -fstd89

pgsm-exp89:
	r.build -o pgsm -obj *.o $(HOME)/src/interp/*.o -libpath $(PGSM)/lib/$(ARCH)$(ABI) -libappl dies -librmn rmnbeta -fstd89

pgsm6.9.8:
	r.build -o pgsm -obj *.o $(ARMNLIB)/lib/$(ARCH)$(ABI)/c_ezscint_5.1.o -libpath $(PGSM)/lib/$(ARCH)$(ABI) -libappl dies -librmn rmn_005

pgsm6.9.8_89:
	r.build -o pgsm -obj *.o $(ARMNLIB)/lib/$(ARCH)$(ABI)/c_ezscint_5.1.o -libpath $(PGSM)/lib/$(ARCH)$(ABI) -libappl dies -librmn rmn_005 -fstd89

pgsm-src:
	r.compile -src f_pgsm.ftn c_pgsm.c $(HOME)/src/interp/c_ezscint.c $(HOME)/src/interp/f_ezscint.ftn $(HOME)/src/utils/diese/dies.c $(HOME)/src/utils/diese/fillgrid.ftn90 -debug -O 0 -o pgsm-src -librmn rmn_007

pgsm-src90:	f_pgsm.ftn90
	r.compile -src f_pgsm.ftn90 c_pgsm.c -O 2 -obj $(HOME)/src/isi4/*.o -bidon -main pgsm -o pgsm-src -librmn rmnbeta

clean:
#Faire le grand menage. On enleve tous les fichiers sources\ninutiles et les .o
	rm -f *.o *~ *.f *.f90 pgsm pgsm2000 pgsm89

fastclean:
	rm *.o pgsm.f

