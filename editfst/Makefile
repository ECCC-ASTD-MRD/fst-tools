# check that a compiler architecture is defined
ifeq "$(BASE_ARCH)" "$(EC_ARCH)"
$(error FATAL: EC_ARCH is equal to BASE_ARCH, no compiler architecture is defined, ABORTING)
endif
# check that environment variable ARMNLIB is present
ifeq "$(ARMNLIB)" ""
$(error FATAL: environment variable ARMNLIB is not defined, ABORTING)
endif
# check that the suffix rules files is available
ifeq (,$(wildcard  $(ARMNLIB)/include/makefile_suffix_rules.inc))
$(error FATAL: file $(ARMNLIB)/include/makefile_suffix_rules.inc not found, ABORTING)
endif

.SUFFIXES:

.SUFFIXES : .ftn .cdk .c .o .f90 .f

SHELL = /bin/bash

COMPILE = compile

FFLAGS =

CFLAGS =

OPTIMIZ = -O 2
#OPTIMIZ = -debug

VER = 6.19

LIBRMN = rmn_014

EDITFST = editfst_$(VER)-$(BASE_ARCH)

default: $(EDITFST)

#include $(ARMNLIB)/include/makefile_suffix_rules.inc
.ftn.f:
	s.ftn -c $(OPTIMIZ) -P -src $<

.ftn.o:
	s.ftn -c $(OPTIMIZ) $(FFLAGS) -src $<

.f90.o:
	s.f90 -c $(OPTIMIZ) $(FFLAGS) $<

.f.o:
	s.f90 -c $(OPTIMIZ) $(FFLAGS) $<

.c.o:
	s.cc $(OPTIMIZ) $(CFLAGS) -c $<


OBJECTS= configuration.o \
	 copystx.o 	 critsup.o 	 desire.o 	 dmpdes.o \
	 editfst.o 	 entsort.o 	 exdes.o         fermes.o \
	 fstnol.o        holacar.o       julhr.o 	 ouvred.o \
	 ouvres.o        rewinds.o 	 sautsqi.o 	 sauvdez.o \
	 select.o        setper.o 	 sqicopi.o 	 stdcopi.o \
	 weofile.o       zap.o 	         ip1equiv.o	 

FICHIERS= \
	 copystx.f 	 critsup.f 	 desire.f 	 dmpdes.f \
	 editfst.f 	 entsort.f 	 exdes.f    	 fermes.f \
	 fstnol.f        holacar.f       julhr.f 	 ouvred.f \
	 ouvres.f        rewinds.f 	 sautsqi.f 	 sauvdez.f \
	 select.f 	 setper.f 	 sqicopi.f 	 stdcopi.f \
	 weofile.f       zap.f

FTNDECKS= \
	 copystx.ftn 	 critsup.ftn 	 desire.ftn 	 dmpdes.ftn \
	 editfst.ftn 	 entsort.ftn 	 exdes.ftn       fermes.ftn \
	 fstnol.ftn      holacar.ftn	 julhr.ftn 	 ouvred.ftn \
	 ouvres.ftn      rewinds.ftn	 sautsqi.ftn 	 sauvdez.ftn \
	 select.ftn      setper.ftn      sqicopi.ftn 	 stdcopi.ftn \
	 weofile.ftn     zap.ftn         ip1equiv.ftn


COMDECKS= \
	 char.cdk 	 desrs.cdk 	 fiches.cdk 	 key.cdk \
	 lin128.cdk 	 logiq.cdk 	 maxprms.cdk 	 tapes.cdk 


copystx.o:     copystx.ftn     maxprms.cdk     logiq.cdk       desrs.cdk       \
               tapes.cdk       key.cdk         char.cdk        fiches.cdk
critsup.o:     critsup.ftn     maxprms.cdk     desrs.cdk       logiq.cdk       \
               char.cdk        fiches.cdk
desire.o:      desire.ftn      maxprms.cdk     desrs.cdk       lin128.cdk      \
               logiq.cdk       fiches.cdk      char.cdk
dmpdes.o:      dmpdes.ftn      maxprms.cdk     logiq.cdk        desrs.cdk      \
               fiches.cdk      char.cdk        maxprms.cdk
editfst.o:     editfst.ftn     maxprms.cdk     tapes.cdk        fiches.cdk     \
               logiq.cdk       desrs.cdk       key.cdk          char.cdk
entsort.o:     entsort.ftn
exdes.o:       exdes.ftn       maxprms.cdk     desrs.cdk        fiches.cdk
fermes.o:      fermes.ftn      maxprms.cdk     desrs.cdk        key.cdk        \
               char.cdk        tapes.cdk       fiches.cdk
fstnol.o:      fstnol.ftn
holacar.o:     holacar.ftn
julhr.o:       julhr.ftn
ouvred.o:      ouvred.ftn      maxprms.cdk     logiq.cdk        key.cdk        \
               char.cdk        tapes.cdk       fiches.cdk
ouvres.o:      ouvres.ftn      maxprms.cdk     logiq.cdk        key.cdk        \
               char.cdk        tapes.cdk       fiches.cdk
rewinds.o:     rewinds.ftn     lin128.cdk      maxprms.cdk      logiq.cdk      \
               desrs.cdk       tapes.cdk       fiches.cdk       char.cdk
sautsqi.o:     sautsqi.ftn     lin128.cdk      maxprms.cdk      tapes.cdk      \
               logiq.cdk       char.cdk        fiches.cdk
sauvdez.o:     sauvdez.ftn     maxprms.cdk     desrs.cdk        char.cdk       \
               fiches.cdk
select.o:      select.ftn      logiq.cdk       maxprms.cdk      fiches.cdk     \
               desrs.cdk       tapes.cdk
setper.o:      setper.ftn      maxprms.cdk     logiq.cdk        lin128.cdk     \
               desrs.cdk       fiches.cdk
sqicopi.o:     sqicopi.ftn     maxprms.cdk     logiq.cdk        lin128.cdk     \
               fiches.cdk      desrs.cdk       char.cdk         tapes.cdk
stdcopi.o:     stdcopi.ftn     lin128.cdk      maxprms.cdk     logiq.cdk       \
               tapes.cdk       char.cdk        fiches.cdk
weofile.o:     weofile.ftn     lin128.cdk      maxprms.cdk     logiq.cdk       \
               fiches.cdk      char.cdk   
zap.o:         zap.ftn         maxprms.cdk     fiches.cdk      logiq.cdk       \
               desrs.cdk       char.cdk
ip1equiv.o:    ip1equiv.ftn

#
# a few extra items on top of rmnlib version 014, this will eventually get removed
#
EXTRAS = EXTRAS

convert_ip123.o: $(EXTRAS)/convert_ip123.f90

fst_missing.o: $(EXTRAS)/fst_missing.c

DlInterface.o: $(EXTRAS)/DlInterface.c

fstd98.o: $(EXTRAS)/fstd98.c $(EXTRAS)/if_fstd98.c $(EXTRAS)/qstdir.h  $(EXTRAS)/convert_ip.h

EXTRA_OBJECTS = \
	convert_ip123.o fstd98.o fst_missing.o DlInterface.o

$(EDITFST): $(EXTRA_OBJECTS) $(OBJECTS)
	s.f90 -o editfst_$(VER)-$(BASE_ARCH) $(EXTRA_OBJECTS) $(OBJECTS) -l$(LIBRMN)

all: $(EDITFST) test.fst

test.fst: create_test_file
	./create_test_file

create_test_file:	create_test_file.f90 $(EXTRA_OBJECTS)
	s.f90 $< $(EXTRA_OBJECTS) -o $@ -l$(LIBRMN)
	rm -f test.fst

tests: $(EDITFST) test.fst
	./run_tests.sh $(EDITFST) test.fst

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
	rm -f *.mod *.o $(EDITFST)  create_test_file test.fst
