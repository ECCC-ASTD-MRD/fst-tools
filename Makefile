.SUFFIXES :

.SUFFIXES : .ftn .f .c .o

SHELL = /bin/sh

CPP = /lib/cpp

FFLAGS =

CFLAGS =

OPTIMIZ = -O 2

CPPFLAGS = -I$(ARMNLIB)/include

default: absolu

include $(ARMNLIB)/include/makefile_suffix_rules.inc

FDECKS= voir.f

OBJET= voir.o
OBJSUP=/users/dor/armn/lib/OBJ/*.o

FICHIERS = $(FDECKS)

absolu: $(OBJET)
	r.build -o voir -obj $(OBJET) -arch $(EC_ARCH) -abi $(ABI) -librmn rmn_rc010
        
voir+: $(OBJET)
	r.build -o voir+ -obj $(OBJET) -arch $(EC_ARCH) -abi $(ABI) -librmn rmnbeta
        
voirca: $(OBJET)
	r.build -o voirca -debug -obj $(OBJET) /home/dormrb02/RMNLIB_working_copy/trunk/primitives/*.o -arch $(EC_ARCH) -abi $(ABI) -librmn rmn_rc010

voir__: $(OBJET)
	r.build -o voir__ -obj $(OBJET) -arch $(EC_ARCH) -abi $(ABI) -librmn rmnbeta

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
	rm *.o voir
