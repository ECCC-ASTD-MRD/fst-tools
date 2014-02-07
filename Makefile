
.SUFFIXES : .ftn .f .cdk

SHELL = /bin/sh

FFLAGS =

CFLAGS =

OPTIMIZ = -O 2

REV = 3.0

LIBRMN = rmn_014

default: absolu

.ftn.o:
	s.compile -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.o:
	s.compile -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

.f.o:
	s.compile -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<


OBJECTS= reflex.o

FICHIERS= reflex.f

absolu: $(OBJECTS) 
	s.compile -o reflex_$(REV)-$(BASE_ARCH) -obj $(OBJECTS) -arch $(EC_ARCH) -abi $(ABI) -librmn $(LIBRMN)

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
	rm *.o reflex_$(REV)-$(BASE_ARCH)
