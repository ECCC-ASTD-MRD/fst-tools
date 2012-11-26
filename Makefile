.SUFFIXES :

.SUFFIXES : .ftn90 .c .o

SHELL = /bin/sh

CPP = /lib/cpp

RMNLIB = $(ARMNLIB)/lib/$(EC_ARCH)$(ABI)/librmn.a

FFLAGS =  

CFLAGS = 

DEFINE = -defines =-DFICHSTD98
#
#OPTIMIZ = -O 0 -debug
OPTIMIZ = -O 2

VER_C = 304
VER_U = 303

default: fstcompress fstuncompress

.ftn90.o:
	s.compile -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.o:
	s.compile -abi $(ABI) $(DEFINE) $(OPTIMIZ) -includes "$(CPPFLAGS)" -optc "=$(CFLAGS)" -src $<

OBJET =

fstcompress: fstcompress.o $(OBJET)
	s.compile -o fstcompress_$(VER_C)-$(BASE_ARCH) -obj fstcompress.o $(OBJET) -librmn rmn_013

fstcompress+: fstcompress.o $(OBJET)
	s.compile -o fstcompress+ -obj fstcompress.o $(OBJET) -librmn rmnbeta

fstuncompress: fstuncompress.o $(OBJET) 
	s.compile -o fstuncompress_$(VER_U)-$(BASE_ARCH) -obj fstuncompress.o $(OBJET) -librmn rmn_013

fstuncompress+: fstuncompress.o $(OBJET) 
	s.compile -o fstuncompress+ -obj fstuncompress.o $(OBJET) -librmn rmnbeta

clean:
	  \rm -f *.o *.f90 *.stb *.f *_*-$(BASE_ARCH) 

