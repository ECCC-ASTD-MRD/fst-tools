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

.ftn90.o:
	r.compile -arch $(EC_ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.o:
	r.compile -arch $(EC_ARCH) -abi $(ABI) $(DEFINE) $(OPTIMIZ) -includes "$(CPPFLAGS)" -optc "=$(CFLAGS)" -src $<

OBJET =

fstcompress: fstcompress.o $(OBJET)
	r.build -o fstcompress -obj fstcompress.o $(OBJET) -librmn rmn_009

fstcompress+: fstcompress.o $(OBJET)
	r.build -o fstcompress+ -obj fstcompress.o $(OBJET) -librmn rmnbeta

fstuncompress: fstuncompress.o $(OBJET) 
	r.build -o fstuncompress -obj fstuncompress.o $(OBJET) -librmn rmn_009

fstuncompress+: fstuncompress.o $(OBJET) 
	r.build -o fstuncompress+ -obj fstuncompress.o $(OBJET) -librmn rmnbeta

clean:
	  \rm -f *.o *.f90 *.stb *.f

