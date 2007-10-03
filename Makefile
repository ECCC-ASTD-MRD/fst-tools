include /usr/local/env/armnlib/include/$(ARCH)$(ABI)/Makefile_addons

.SUFFIXES : .ftn .f .c .o

SHELL = /bin/sh

CPP = /lib/cpp

RMNLIB = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmn.a

FFLAGS = 

CFLAGS = 

OPTIMIZ = -O 2
OPTIMIZ =  -debug -O 0

CPPFLAGS = -I$(ARMNLIB)/include

MYLIB =  $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmn.a

.PRECIOUS: $(RMNLIB) $(MALIB)

default: obj

.ftn.o:
	r.compile_021 -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $*.ftn

.c.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

OBJET = f_pgsm.o c_pgsm.o

FICHIERS_CDK = \
accum.cdk     chck.cdk     ecrires.cdk  idents.cdk   lnkflds.cdk  qqqfilt.cdk\
cfldinf.cdk   convers.cdk  enrege.cdk   impnone.cdk  nivos.cdk    symnom.cdk\
champs.cdk    dates.cdk    gdz.cdk      indptr.cdk   packin.cdk   tp12ig.cdk\
champseq.cdk  defin.cdk    grilles.cdk  lires.cdk    pairs.cdk   voir.cdk\
charac.cdk    dummys.cdk   heures.cdk   llccmm.cdk   param.cdk

FICHIERS_FTN = \
calcul.ftn     ecrits.ftn     gristdb.ftn      liren.ftn    routines.ftn\
champ.ftn      ecritur.ftn    gritp12.ftn      lopascm.ftn  pgsmlic.ftn    scalair.ftn\
champ_seq.ftn  epais.ftn      loupmir.ftn  pgsmlir.ftn    setintx.ftn\
chkenrpos.ftn  chk_hy.ftn     fillcoord.ftn  grlalon.ftn      lrsmdes.ftn  pgsmluk.ftn    setxtrap.ftn\
chmpdif.ftn    heure.ftn      macpcp.ftn   plmnmod.ftn    sorti.ftn\
comme.ftn      grigaus.ftn    imprime.ftn      messags.ftn  prefiltre.ftn  symetri.ftn\
conlalo.ftn    grigef.ftn     initid.ftn       metsym.ftn   putfld.ftn     stenfilt.ftn testseq.ftn\
conver.ftn     grigrib.ftn    initseq.ftn      operat.ftn   qaaqr.ftn      uvect.ftn\
convs.ftn      grille2.ftn    itrouve.ftn      outlalo.ftn  qqqecho.ftn    vdauv.ftn\
coord.ftn      grillps.ftn    gristereo.ftn    lastcol.ftn      pairvct.ftn  qqqfilt.ftn    verlalo.ftn\
coupe.ftn      grilstd.ftn    legvar.ftn       pgsm2.ftn      qqqform.ftn\
coupzm.ftn     griltp4.ftn    liraxez.ftn      pgsmabt.ftn  qqqident.ftn

FICHIERS_C = \
c_pgsm.c

FICHIERS = $(FICHIERS_FTN) $(FICHIERS_C) 

f_pgsm.ftn: $(FICHIERS_FTN) $(FICHIERS_CDK)
	cat $(FICHIERS_FTN) > f_pgsm.ftn

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

clean:
#Faire le grand menage. On enleve tous les fichiers sources\ninutiles et les .o 
	rm -f *.o f_pgsm.f pgsm pgsm2000 pgsm89

fastclean:
	rm *.o pgsm.f	

