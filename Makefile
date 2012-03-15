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

FDECKS= fstcomp.f

OBJET= fstcomp.o

FICHIERS = $(FDECKS)

absolu: $(OBJET)
	r.build -o fstcomp -obj $(OBJET) -arch $(ARCH) -abi $(ABI) -librmn rmn_012

fstcomp+: $(OBJET)
	r.build -o fstcomp+ -debug -obj $(OBJET) -arch $(ARCH) -abi $(ABI) -librmn rmn_012

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
	rm *.o fstcomp
