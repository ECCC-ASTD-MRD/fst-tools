.SUFFIXES :

.SUFFIXES : .ftn .f .c .o .ftn90 

SHELL = /bin/sh

CPP = /lib/cpp

FFLAGS =

CFLAGS =

OPTIMIZ = -O 2
$(info OPTIMIZ is ${OPTIMIZ})

CPPFLAGS = 

include version.inc

LIBRMN = 

default: absolu

include $(RPN_TEMPLATE_LIBS)/include/makefile_suffix_rules.inc

FDECKS= fstcomp.f

OBJET= fstcomp.o statfldx.o

FICHIERS = $(FDECKS)

absolu: $(OBJET)
	s.compile -o fstcomp_$(subst ",,$(RELEASE))-$(BASE_ARCH) -obj $(OBJET) $(OPTIMIZ) -librmn $(LIBRMN)

fstcomp+: $(OBJET)
	s.compile -o fstcomp+ -debug -obj $(OBJET) -librmn $(LIBRMN)

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
	rm -f *.o statfldx.f90 fstcomp_$(subst ",,$(RELEASE))-$(BASE_ARCH)
	rm -rf .fo
