include Makefile_$(ARCH)$(ABI)

.SUFFIXES : 

.SUFFIXES : .ftn .f .c .o 

SHELL = /bin/sh

CPP = /lib/cpp

RECLIB = $(REC)/lib/$(ARCH)$(ABI)/librec.a

FFLAGS = 

CFLAGS = -I/usr/include/Motif1.2R6 -I/usr/X11R6/include -I$(ARMNLIB)/include -I$(HOME)/src/include -DX_WGL

OPTIMIZ = -O 2

CPPFLAGS = -I$(ARMNLIB)/include =-I$(HOME)/src/include -DX_WGL

.PRECIOUS:

default: obj

.ftn.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $*.f

.ftn.f:
	rm -f $*.f
	r.ftntof -P $(DEFINE) $< > $*.f
	chmod 444 $*.f

.c.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

.f.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.a:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<
	ar rv $@ $*.o
	rm -f $*.o

.f.a:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<
	ar rv $@ $*.o
	rm -f $*.o

FTNDECKS=  fststat.ftn fststatm.ftn statfld4.ftn

FDECKS= fststat.f fststatm.f statfld4.f

CDECKS= 

OBJECTS= fststat.o fststatm.o statfld4.o

COMDECKS= xfsl-voir.cdk   xfsl.cdk

fststat.o: 	fststat.ftn 
fststatm.o: 	fststatm.ftn 
statfld4.o: 	statfld4.ftn 


obj: $(CDECKS) $(FDECKS) $(OBJECTS)
#Produire les fichiers objets (.o) pour tous les fichiers

fststat:
	r.build -obj *.o -o fststat -librmn rmn_x

clean:
	/bin/rm -f *.f *.o fststat

