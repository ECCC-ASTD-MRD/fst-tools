#include $(ARMNLIB)/include/$(EC_ARCH)/Makefile_addons

.SUFFIXES : 

.SUFFIXES : .ftn .c .o 

SHELL = /bin/sh

CPP = /lib/cpp

RECLIB = $(REC)/lib/$(EC_ARCH)$(ABI)/librec.a

FFLAGS = 

CFLAGS = -I/usr/include/Motif1.2R6 -I/usr/X11R6/include -I$(ARMNLIB)/include -I$(HOME)/src/include -DX_WGL

OPTIMIZ = -O 2

CPPFLAGS = -I$(ARMNLIB)/include 

.PRECIOUS:

default: obj

.ftn.o:
	r.compile -arch $(EC_ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.f90.o:
	r.compile -arch $(EC_ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.o:
	r.compile -arch $(EC_ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

FTNDECKS=  fststat.ftn fststatm.ftn statfld4.ftn

OBJECTS= fststat.o fststatm.o statfld4.o
OBJSUP= /users/dor/armn/lib/OBJ/*.o


obj: $(OBJECTS)
#Produire les fichiers objets (.o) pour tous les fichiers

fststat: $(OBJECTS)
	r.build  -obj $(OBJECTS) -o $@ -librmn rmn_009

fststat+: $(OBJECTS)
	r.build -obj $(OBJECTS) -o $@ -librmn rmnbeta

clean:
	/bin/rm -f *.f *.o fststat

