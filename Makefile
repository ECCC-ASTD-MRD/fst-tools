include Makefile_$(ARCH)$(ABI)

.SUFFIXES : .ftn .f .c .o

SHELL = /bin/sh

CPP = /lib/cpp

RMNLIB = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmn.a

FFLAGS = 

CFLAGS = 

OPTIMIZ = -O 2

CPPFLAGS = -I$(ARMNLIB)/include

MYLIB =  $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmn.a

.PRECIOUS: $(RMNLIB) $(MALIB)

default: obj

.ftn.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $*.ftn

.ftn.f:
	rm -f $*.f

	r.ftntof -P $(DEFINE) $< > $*.f
	chmod 444 $*.f

.c.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

.f.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

OBJET = pgsm.o c_pgsm.o

FICHIERS_CDK = \
accum.cdk     chck.cdk     ecrires.cdk  idents.cdk   lnkflds.cdk  qqqfilt.cdk\
cfldinf.cdk   convers.cdk  enrege.cdk   impnone.cdk  nivos.cdk    symnom.cdk\
champs.cdk    dates.cdk    gdz.cdk      indptr.cdk   packin.cdk   tp12ig.cdk\
champseq.cdk  defin.cdk    grilles.cdk  lires.cdk    pairs.cdk   voir.cdk\
charac.cdk    dummys.cdk   heures.cdk   llccmm.cdk   param.cdk

FICHIERS_FTN = \
calcul.ftn     ecrits.ftn     gristdb.ftn      liren.ftn    pgsmglue.ftn   routines.ftn\
champ.ftn      ecritur.ftn    gritp12.ftn      lopascm.ftn  pgsmlic.ftn    scalair.ftn\
champ_seq.ftn  epais.ftn      loupmir.ftn  pgsmlir.ftn    setintx.ftn\
chkenrpos.ftn  fillcoord.ftn  grlalon.ftn      lrsmdes.ftn  pgsmluk.ftn    setxtrap.ftn\
chmpdif.ftn    heure.ftn        macpcp.ftn   plmnmod.ftn    sorti.ftn\
comme.ftn      grigaus.ftn    imprime.ftn      messags.ftn  prefiltre.ftn  symetri.ftn\
conlalo.ftn    grigef.ftn     initid.ftn       metsym.ftn   putfld.ftn     testseq.ftn\
conver.ftn     grigrib.ftn    initseq.ftn      operat.ftn   qaaqr.ftn      uvect.ftn\
convs.ftn      grille2.ftn    itrouve.ftn      outlalo.ftn  qqqecho.ftn    vdauv.ftn\
coord.ftn      grillps.ftn    gristereo.ftn    lastcol.ftn      pairvct.ftn  qqqfilt.ftn    verlalo.ftn\
coupe.ftn      grilstd.ftn    legvar.ftn       pgsm.ftn     pgsm2.ftn      qqqform.ftn\
coupzm.ftn     griltp4.ftn    liraxez.ftn      pgsmabt.ftn  qqqident.ftn

FICHIERS_C = \
c_pgsm.c

FICHIERS = $(FICHIERS_FTN) $(FICHIERS_C) 

pgsm.o : pgsm.f

f_pgsm.ftn: $(FICHIERS_FTN) $(FICHIERS_CDK)
	cat $(FICHIERS_FTN) > f_pgsm.ftn

obj: $(OBJET)
#Produire les fichiers objets (.o) pour tous les fichiers

genlib: $(OBJET)
#Creer ou mettre a jour la programmatheque 
	$(AR) rcv $(MYLIB) $(OBJET)

pgsm: pgsm.o
	r.build -o $@ -obj *.o -librmn

pgsm89: pgsm.o
	r.build -o $@ -obj *.o -librmn rmnbeta -fstd89

pgsmnew: c_pgsm.o
	r.build -o pgsm -obj *.o /users/dor/armn/lib/public/xdf98.o  -libpath $(PGSM)/lib/$(ARCH)$(ABI) -libappl dies efence -librmn rmnbeta

# 	r.build -o pgsm -obj *.o -libpath $(PGSM)/lib/$(ARCH)$(ABI) -libappl dies -librmn rmnbeta


pgsm-stereo: 
	r.build -o pgsm -obj *.o -libpath $(PGSM)/lib/$(ARCH)$(ABI) -libappl dies -librmn rmnbeta

pgsm-exp:
	r.build -o pgsm -obj *.o -libpath $(PGSM)/lib/$(ARCH)$(ABI) -libappl dies -librmn rmnbeta

pgsm-exp89:
	r.build -o pgsm -obj *.o $(HOME)/src/interp/*.o -libpath $(PGSM)/lib/$(ARCH)$(ABI) -libappl dies -librmn rmnbeta -fstd89

clean:
#Faire le grand menage. On enleve tous les fichiers sources\ninutiles et les .o 
	-if [ "*.ftn" != "`echo *.ftn`" ] ; \
	then \
	for i in *.ftn ; \
	do \
	fn=`r.basename $$i '.ftn'`; \
	rm -f $$fn.f; \
	done \
	fi
	rm *.o pgsm.f

fastclean:
	rm *.o pgsm.f	

