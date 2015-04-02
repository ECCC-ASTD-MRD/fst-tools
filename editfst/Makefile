# check that a compiler architecture is defined
ifeq "$(COMP_ARCH)" ""
$(error FATAL: environment variable COMP_ARCH absent, no compiler architecture is defined, ABORTING)
endif

.SUFFIXES:

.SUFFIXES : .o .c .f90 .f .F90

SHELL = /bin/bash

COMPILE = compile

#FFLAGS =
#FFLAGS = -IEXTRAS/Localinclude -IEXTRAS/Localinclude/$(EC_ARCH) -LEXTRAS/Locallib/$(EC_ARCH)

#CFLAGS =
#CFLAGS = -IEXTRAS/Localinclude -IEXTRAS/Localinclude/$(EC_ARCH) -LEXTRAS/Locallib/$(EC_ARCH)

OPTIMIZ = -O 2
#OPTIMIZ = -debug

include editfst.inc

#LIBRMN = rmn_014
LIBRMN = rmn_015.2

LIB = lib/$(EC_ARCH)/libeditfst_$(RELEASE).a

EDITFST = editfst_$(RELEASE)-$(BASE_ARCH)

default: $(EDITFST)

.f90.o:
	s.f90 -c $(OPTIMIZ) $(FFLAGS) $<

.c.o:
	s.cc -c $(OPTIMIZ) $(CFLAGS) $<

OBJECTS_SUPP= excdes_new.o fstd98.o xdf98.o

OBJECTS= configuration.o \
	 copystx.o 	 critsup.o 	 desire.o 	 dmpdes.o \
	 editfst.o 	 fermes.o \
	 fstnol.o        holacar.o       ouvred.o \
	 ouvres.o        rewinds.o 	 sautsqi.o 	 sauvdez.o \
	 select.o        setper.o 	 sqicopi.o 	 stdcopi.o \
	 weofile.o       zap.o

$(LIB): $(OBJECTS)
	mkdir -p lib/$(EC_ARCH)
	ar rcv $(LIB) $(OBJECTS)
	rm -f $(OBJECTS)

$(EDITFST): $(LIB) $(OBJECTS_SUPP)
	echo "program edit_fst ; call editfst ; stop ; end" >bidon.f90
	s.f90 bidon.f90 $(OBJECTS_SUPP) -o $@ $(FFLAGS) $(LIB) -l$(LIBRMN)
	rm -f bidon.f90 $(OBJECTS_SUPP)

all: $(EDITFST) test.fst

test.fst: create_test_file
	./create_test_file

create_test_file:	create_test_file.f90
#	s.f90 $< $(EXTRA_OBJECTS) -o $@ $(FFLAGS) -l$(LIBRMN)
	s.f90 $< -o $@ $(FFLAGS) -l$(LIBRMN)
	rm -f test.fst

tests: test.fst
	./run_tests.sh $(EDITFST) test.fst

clean:
	rm -f $(OBJECTS) *.mod

distclean: clean
	rm -f *.mod *.o $(EDITFST)  create_test_file test.fst $(LIB) bidon.f90
