.SUFFIXES : 

.SUFFIXES : .ftn90 .f90 .ftn .c .o 

SHELL = /bin/sh

CPP = /lib/cpp

OPTIMIZ = -O 2
#OPTIMIZ = -debug -O 0 -optc =-Wall
#OPTIMIZ = -debug -O 0 
$(info OPTIMIZ is ${OPTIMIZ})

FFLAGS = 
.PRECIOUS:

VER = 2.05
VERZC = 2.1

LIBRMN = 

default: bemol zcrop

.ftn90.o:
	s.compile -abi $(ABI) $(OPTIMIZ) -src $<

.ftn.o:
	s.compile -abi $(ABI) $(OPTIMIZ) -src $<

.f90.o:
	s.compile -abi $(ABI) $(OPTIMIZ) -src $<

.c.o:
	s.compile -abi $(ABI) $(OPTIMIZ) -src $<

FTNDECKS= bemol.ftn90 bm_openfiles.ftn90 bm_std_wrt.ftn90  bm_vanilla_wrt.ftn90 bm_wrt_axay.ftn90 \
	  bm_closefiles.ftn90 bm_core_wrt.ftn90 bm_coarse_wrt.ftn90 fillgrid.ftn90 statfld.ftn90

CDECKS= diese.c bm_utils.c c_inventaire_tuiles.c diese_var_g.c bemol_opt.c dies_process_flds.c

OBJECTS= bemol.o dies.o bm_openfiles.o bm_std_wrt.o bm_vanilla_wrt.o bm_wrt_axay.o \
	  bm_closefiles.o bm_core_wrt.o bm_coarse_wrt.o fillgrid.o bm_utils.o statfld.o c_inventaire_tuiles.o diese_var_g.o \
	  c_dies_process_flds.o  bemol_opt.o 

obj: $(OBJECTS) 
#Produire les fichiers objets (.o) pour tous les fichiers

bemol: $(OBJECTS)
	s.compile -o bemol_$(VER)-$(BASE_ARCH) -obj $(OBJECTS) -librmn $(LIBRMN)

zcrop:
	(cd utils/zcrop; s.compile -o zcrop_$(VERZC)-$(BASE_ARCH) -src zcrop.ftn90 -librmn $(LIBRMN))
	(cd utils/zcrop; mv zcrop_$(VERZC)-$(BASE_ARCH) ../..)

bemol+: $(OBJECTS)
	r.build -o bemol+ -obj $(OBJECTS) -librmn rmnbeta

bemol_008: $(OBJECTS)
	s.compile -o bemol_1.39_008 -obj $(OBJECTS) -librmn rmn_008

clean:
	/bin/rm -f $(OBJECTS) *.stb *.f90 *~ bemol_$(VER)-$(BASE_ARCH) zcrop_$(VERZC)-$(BASE_ARCH)
	(cd utils/zcrop; /bin/rm -f *.o *.f90 *~)
