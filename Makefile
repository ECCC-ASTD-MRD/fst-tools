.SUFFIXES : 

.SUFFIXES : .ftn90 .f90 .ftn .c .o 

SHELL = /bin/sh

CPP = /lib/cpp

OPTIMIZ = -O 2
#OPTIMIZ = -debug -O 0 -optc =-Wall
#OPTIMIZ = -debug -O 0 

FFLAGS = 

.PRECIOUS:

default: obj

.ftn90.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -src $<

.ftn.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -src $<

.f90.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -src $<

.c.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -src $<

FTNDECKS= bemol.ftn90 bm_openfiles.ftn90 bm_std_wrt.ftn90  bm_vanilla_wrt.ftn90 bm_wrt_axay.ftn90 \
	  bm_closefiles.ftn90 bm_core_wrt.ftn90 bm_coarse_wrt.ftn90 fillgrid.ftn90 statfld.ftn90

CDECKS= diese.c bm_utils.c c_inventaire_tuiles.c diese_var_g.c bemol_opt.c dies_process_flds.c

OBJECTS= bemol.o dies.o bm_openfiles.o bm_std_wrt.o bm_vanilla_wrt.o bm_wrt_axay.o \
	  bm_closefiles.o bm_core_wrt.o bm_coarse_wrt.o fillgrid.o bm_utils.o statfld.o c_inventaire_tuiles.o diese_var_g.o \
	  c_dies_process_flds.o  bemol_opt.o 

obj: $(OBJECTS) 
#Produire les fichiers objets (.o) pour tous les fichiers

bemol: $(OBJECTS)
	r.build -o bemol -obj $(OBJECTS) -librmn rmn_009

bemol+: $(OBJECTS)
	r.build -o bemol+ -obj $(OBJECTS) -librmn rmnbeta

clean:
	/bin/rm -f $(OBJECTS) *.stb *.f90 *~ bemol bemol_2000

