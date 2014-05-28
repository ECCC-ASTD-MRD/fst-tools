.SUFFIXES :

.SUFFIXES : .ftn .f .c .o

SHELL = /bin/sh

CPP = /lib/cpp

FFLAGS =

CFLAGS =

OPTIMIZ = -O 2
$(info OPTIMIZ is ${OPTIMIZ})

CPPFLAGS = 

VER = 98.30

LIBRMN = 

default: absolu

include $(RPN_TEMPLATE_LIBS)/include/makefile_suffix_rules.inc

FDECKS= voir.f

OBJET= voir.o
OBJSUP=/users/dor/armn/lib/OBJ/*.o

FICHIERS = $(FDECKS)

absolu: $(OBJET)
	s.compile -o voir_$(VER)-$(BASE_ARCH) -obj $(OBJET) -abi $(ABI) -librmn $(LIBRMN)
        
voir+: $(OBJET)
	r.build -o voir+ -obj $(OBJET) -arch $(EC_ARCH) -abi $(ABI) -librmn rmnbeta
        
voirca: $(OBJET)
	s.compile -o voirca -debug -obj $(OBJET) /home/dormrb02/RMNLIB_working_copy/trunk/primitives/*.o -arch $(EC_ARCH) -abi $(ABI) -librmn rmn_rc010

voir__: $(OBJET)
	s.compile -o voir__ -obj $(OBJET) -arch $(EC_ARCH) -abi $(ABI) -librmn rmnbeta

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
	rm *.o voir_$(VER)-$(BASE_ARCH)
