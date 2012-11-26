#include $(ARMNLIB)/include/$(EC_ARCH)$(ABI)/Makefile_addons

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

VER = 004

default: fst2xml xml2fst

.ftn.o:
	s.compile -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.o:
	s.compile -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

.c.a:
	s.compile -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<
	ar rv $@ $*.o
	rm -f $*.o

.ftn.a:
	s.compile -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<
	ar rv $@ $*.o
	rm -f $*.f $*.o

FTNDECKS=  

CDECKS= fst2xml.c xml2fst.c

fst2xml: fst2xml.o 
	s.compile -o fst2xml_$(VER)-$(BASE_ARCH) -bidon c -main fst2xml_ -obj fst2xml.o -librmn rmn_013

xml2fst: xml2fst.o 
	s.compile -o xml2fst_$(VER)-$(BASE_ARCH) -bidon c -main xml2fst_ -obj xml2fst.o -librmn rmn_013 

clean:
	rm -f *.o *_$(VER)-$(BASE_ARCH)

