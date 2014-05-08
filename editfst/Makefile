# check that a compiler architecture is defined
ifeq "$(BASE_ARCH)" "$(EC_ARCH)"
$(error FATAL: EC_ARCH is equal to BASE_ARCH, no compiler architecture is defined, ABORTING)
endif

.SUFFIXES:

.SUFFIXES : .o .c .f90 .f .F90

SHELL = /bin/bash

COMPILE = compile

#FFLAGS =
FFLAGS = -IEXTRAS -IEXTRAS/Localinclude -IEXTRAS/Localinclude/$(EC_ARCH) -LEXTRAS/Locallib/$(EC_ARCH)

#CFLAGS =
CFLAGS = -IEXTRAS -IEXTRAS/Localinclude -IEXTRAS/Localinclude/$(EC_ARCH) -LEXTRAS/Locallib/$(EC_ARCH)

OPTIMIZ = -O 2
#OPTIMIZ = -debug

VER = 6.20

#LIBRMN = rmn_014
LIBRMN = rmnbeta_015

EDITFST = editfst_$(VER)-$(BASE_ARCH)

default: $(EDITFST)


.F90.o:
	s.f90 -c -DRELEASE="'$(VER)'" $(OPTIMIZ) $(FFLAGS) -src $<

.f90.o:
	s.f90 -c $(OPTIMIZ) $(FFLAGS) $<

.c.o:
	s.cc $(OPTIMIZ) $(CFLAGS) -c $<


OBJECTS= configuration.o \
	 copystx.o 	 critsup.o 	 desire.o 	 dmpdes.o \
	 editfst.o 	 fermes.o \
	 fstnol.o        holacar.o       ouvred.o \
	 ouvres.o        rewinds.o 	 sautsqi.o 	 sauvdez.o \
	 select.o        setper.o 	 sqicopi.o 	 stdcopi.o \
	 weofile.o       zap.o


$(EDITFST): $(OBJECTS)
	s.f90 -o editfst_$(VER)-$(BASE_ARCH) $(OBJECTS) $(FFLAGS) -l$(LIBRMN)

all: $(EDITFST) test.fst

test.fst: create_test_file
	./create_test_file

create_test_file:	create_test_file.f90
#	s.f90 $< $(EXTRA_OBJECTS) -o $@ $(FFLAGS) -l$(LIBRMN)
	s.f90 $< -o $@ $(FFLAGS) -l$(LIBRMN)
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
