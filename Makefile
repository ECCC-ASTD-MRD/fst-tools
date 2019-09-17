#include $(RPN_TEMPLATE_LIBS)/include/$(EC_ARCH)/Makefile_addons

.SUFFIXES : 

.SUFFIXES : .ftn .c .o 

SHELL = /bin/sh

CPP = /lib/cpp

RECLIB = $(REC)/lib/$(EC_ARCH)$(ABI)/librec.a

FFLAGS = 

CFLAGS = -I/usr/include/Motif1.2R6 -I/usr/X11R6/include -I$(HOME)/src/include -DX_WGL

OPTIMIZ = -O 2
$(info OPTIMIZ is ${OPTIMIZ})

CPPFLAGS =

.PRECIOUS:

VER = 6.7

LIBRMN = 

default: fststat 

.ftn.o:
	s.compile $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.f90.o:
	s.compile $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.o:
	s.compile $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

FTNDECKS=  fststat.ftn fststatm.ftn statfld4.ftn

OBJECTS= fststat.o fststatm.o statfld4.o
OBJSUP= /users/dor/armn/lib/OBJ/*.o


obj: $(OBJECTS)
#Produire les fichiers objets (.o) pour tous les fichiers

fststat: $(OBJECTS)
	s.compile -obj $(OBJECTS) -o $@_$(VER)-$(BASE_ARCH) $(OPTIMIZ) -librmn $(LIBRMN)

fststat+: $(OBJECTS)
	r.build -obj $(OBJECTS) -o $@ -librmn rmnbeta

clean:
	/bin/rm -f *.f *.o fststat_$(VER)-$(BASE_ARCH)

