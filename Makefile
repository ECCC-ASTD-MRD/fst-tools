.SUFFIXES : .ftn .f .cdk

SHELL = /bin/sh

COMPILE = compile

FFLAGS =

CFLAGS =

OPTIMIZ = -O 0 -debug

default: absolu

.ftn.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

.f.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<


FTNDECKS= \
        bsort.ftn       dbzono.ftn       debalzn.ftn       inctcon.ftn \
        litzon.ftn       outfld.ftn       setuvd0.ftn       writlzn.ftn

FDECKS= \
         bsort.f       dbzono.f       debalzn.f       inctcon.f \
         litzon.f       outfld.f       setuvd0.f       writlzn.f 

OBJECTS= \
         bsort.o       dbzono.o       debalzn.o       inctcon.o \
         litzon.o       outfld.o       setuvd0.o       writlzn.o 

COMDECKS= \
         ctescon.cdk

FICHIERS = $(FDECKS) $(CDECKS) $(ASDECKS) $(HDECKS)


bsort.o: bsort.ftn 
dbzono.o: dbzono.ftn 	ctescon.cdk 
debalzn.o: debalzn.ftn 
inctcon.o: inctcon.ftn 	ctescon.cdk 
litzon.o: litzon.ftn 
outfld.o: outfld.ftn 
setuvd0.o: setuvd0.ftn 
writlzn.o: writlzn.ftn 

absolu: $(OBJECTS) 
	r.build -o dbzono -obj $(OBJECTS) -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -librmn rmn_rc008

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
	rm *.o dbzono
