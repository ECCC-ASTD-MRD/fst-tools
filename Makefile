#include /usr/local/env/armnlib/include/$(ARCH)/Makefile_addons

.SUFFIXES : 

.SUFFIXES : .f90 .c .o 

SHELL = /bin/sh

CPP = /lib/cpp

RECLIB = $(REC)/lib/$(ARCH)$(ABI)/librec.a

FFLAGS = 

CFLAGS = -I/usr/include/Motif1.2R6 -I/usr/X11R6/include -I$(ARMNLIB)/include -I$(HOME)/src/include -DX_WGL

OPTIMIZ = -O 2

CPPFLAGS = -I$(ARMNLIB)/include 

.PRECIOUS:

default: obj

.f90.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

FTNDECKS=  fststat.ftn fststatm.ftn statfld4.ftn

OBJECTS= fststat.o
OBJSUP= /users/dor/armn/lib/OBJ/*.o


obj: $(OBJECTS)
#Produire les fichiers objets (.o) pour tous les fichiers

fststat: $(OBJECTS)
	r.compile -src  $@.ftn90 -o $@ -librmn rmn_rc008

fststat__: $(OBJECTS)
	r.build -obj $(OBJECTS) -o fststat__ -librmn rmnbeta

clean:
	/bin/rm -f *.f *.o fststat

