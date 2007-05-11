include /usr/local/env/armnlib/include/$(ARCH)$(ABI)/Makefile_addons

.SUFFIXES : 

.SUFFIXES : .ftn .f .c .o 

SHELL = /bin/sh

CPP = /lib/cpp


FFLAGS = -I$(ARMNLIB)/include -DX_WGL

CFLAGS = -I$(ARMNLIB)/include -I$(ARMNLIB)/xml/include/libxml2

OPTIMIZ = -O 2
#OPTIMIZ = -debug -O 0

CPPFLAGS = -I$(ARMNLIB)/include  -DX_WGL

.PRECIOUS: $(RECLIB)

default: fst2xml

.ftn.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

.c.a:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<
	ar rv $@ $*.o
	rm -f $*.o

.ftn.a:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<
	ar rv $@ $*.o
	rm -f $*.f $*.o

FTNDECKS=  

CDECKS= fst2xml.c xml2fst.c

fst2xml: fst2xml.o 
	r.build -o fst2xml -bidon c -main fst2xml_ -obj fst2xml.o -librmn rmnbeta

xml2fst: xml2fst.o 
	r.build -o xml2fst -bidon c -main xml2fst_ -obj xml2fst.o -librmn rmnbeta 

clean:
	rm -f *.o
