# Table of Contents
1. [Description](#description)
2. [Usage](#usage)
3. [Command line argument definition](#command-line-argument-definition)
4. [Quick start](#quick-start)
5. [Directives](#directives)
6. [Examples](#examples)

# Description
''Programme général de Sortie des Modèles''\
''General purpose model output program''

pgsm is a utility designed to perform horizontal interpolations and basic arithmetic operations on RPN standard files.Input files must be RPN standard files and output files may be RPN standard files (random or sequential), binary FORTRAN sequential files, or formatted ASCII files.

PGSM can:
- Interpolate data on various geographical projections, compressed or not.
- Interpolate wind components UU and VV with respect to the scale and orientation of the output grid.
- Perform symmetric or antisymmetric extrapolations from an hemispheric grid.
- Compute thicknesses between 2 levels.
- Compute precipitation amounts between 2 forecast hours.
- Extract low, mid and high clouds.
- Perform mathematical operations on one field or more.
- Compute latitudes and longitudes from the X-Y coordinates of a grid.
- Read navigational records of latitudes-longitudes (grid type Y) or grid coordinates (grid type Z) in an input or output file and interpolate values from these coordinates. 

# Usage 
```bash
pgsm -iment input1 input2 ... input40 -ozsrt output -i directives -l listing -date
pgsm -iment input1... -ozsrt output >/dev/null <<ENDPGSM
```

# Command line argument definition

|key     | description |
|--------|-------------|
|-iment  | name[s] of RPN standard files (random) to be used as source for fields and grid; required. A maximum of 40 files is currently allowed|
|-ozsrt  | name of RPN standard file (random) to be used as output for fields and grid; required |
|-i      | name of directives file; defaults to "stdin". In a shell script, directives may also be included after the call to pgsm by using the "here document" syntax: <<ENDPGSM on the calling line and ENDPGSM by itself as the last directive. Of course ENDPGSM is only an example and may be anything provided it does not appear elsewhere between the calling and the last lines. See example|
|-l      | name of output file for messages and diagnostics; defaults to "stdout". Equivalent to ">". May be sent to /dev/null if directives are debugged already|
|-date   | oprun|oui|non :Obsolete. Kept for backward compatibility. Date is now used only in the information messages printed at the beginning and end of the program |

# Quick start 
PGSM needs basically 4 directives (i.e. commands) to get most of the job done.

- **SORTIE**:To specify the type of the output file (most of the time, sortie(std,100) will do)
- **GRILLE**:To specify the output grid
- **HEURE**:To specifiy the forecast hours desired (TOUT) will get all the available hours
- **CHAMP**:To interpolate the fields on the target grid defined by the GRILLE command.

This example interpolates from the operational model into a 400x200 global Gaussian grid the following records:
- Seal level pressure (PN). Since PN is defined at the surface, we do not need to add a level
- Geopotential height (GZ), at 1000 and 500 mb.
- Air Temperature (TT), at 1000 and 500 mb.

```bash
pgsm -iment $CMCGRIDF/prog/regpres/2002083000_0* -ozsrt pnm.fst -i pnm.dir
```

The file "pnm.dir" contains...

```text
sortie(std,100)
heure(tout)
grille(gauss, 400, 200, global)
champ('PN')
champ('GZ', 1000, 500)
champ('TT', 1000, 500)
end
```

**WARNING** By default PGSM performs a **CUBIC interpolation**. Use SETINTX to specify another type of interpolation.

# Directives 
* PGSM recognizes many [directives](src/pgsm/DIRECTIVES.md), providing much flexibility
* It is recommended to write your command scripts in lower case letters.
* If you do wish to write your commands in CAPITAL letters, be careful to start your commands on the 2nd command of the line, otherwise if your command starts with a C and is in the first column, it will be interpreted as a COMMENT. This is a legacy of FORTRAN 66 coding style that was currently used when pgsm was born, near 1980.
* The commands must be written in lines of no longer than 80 characters. The length of the command can be extended by writing on succeeding lines, where the split must not be within a list surrounded by square brackets nor within quotes. 

# Examples 

1. Typical example. Interpolates model output to a global latlon grid using the CHAMP directive. Interpolated fields include UU and VV wind components, geopotential height, precipitation and cloud fields
```bash
pgsm -iment tape1 -ozsrt sorti -l output <<FINPGSM 
   VOIRENT=NON
* 	            Will not list the content of the standard input file. 
   SORTIE(STD,400,R)
*               Opens standard output standard file "sorti", for up to * 400 records.
*               'R' allows overwriting of records with same * descriptors. 
   HEURE(12,24,36)
*               We want forecasts of 12, 24 and 36 hours.
   GRILLE(STD,30,40,NORD)
*             	Interpolation to standard 'A' grid. 
   CHAMP(UV,500)
*               Interpolation of 500mb UU and VV wind components. 
   SETINTX(LINEAIR)
*               Linear interpolation for following fields. 
   PRINTSR=OUI,5,10,51,55,10,15
*               Prints values of next field interpolated, at coordinates * i=5,15,25,35,45, and j=10,25,40,55. 
   CHAMP(Z,500)
*               Interpolates and prints values of 500mb geopotential field. 
   PRINTEN=NON 
*               Turns off printing option for following fields. 
   CHAMP(PCP,12,24) 
*               Precipitation amounts: PCP='PR' (Hr 24 - Hr 12).    
   CHAMP(Z,300,500)
*               Interpolates sigma levels 0.1 et 0.3 (sigma*10000 + 2000). 
   CHAMP(NUAGES)
*               Interpolates 'NH','NM','NB'. 
FINPGSM         End of directives.
```

2. Computes differences between geopotential height fields to compute thicknesses.
```bash
pgsm -iment tape1 -ozsrt sorti -l output <<DIRectives
   SORTIE(STD,4000,R)
   GRILLE(LATLON,96,32,0.0,360.0,2.5,2.5)
*               The first point will be at Equator and Greenwich, and
*               other points at 2.5 degrees lat-lon.
   HEURE(12,24,36)
*               We want 12, 24 and 36 hour forecasts.
   CHAMP(EPAIS,1000,500)
*               Thicknesses: (500mb GZ - 1000mb GZ)
   CHMPDIF(Z,'DZ',[1000,500,300,100],12,0)
*               Reads (500mb GZ) - (1000mb GZ) at IP2=12 and IP3=0, and writes
*               in the output file: name='DZ', IP1=1000, IP2=500 and IP2=12.
*               Then reads (100mb GZ - 300mb GZ) at IP2=12 and IP3=0, writes
*               in the output file: name='DZ', IP1=300, IP2=100 and IP3=12.
   CHMPDIF(Z,'HZ',500,[12,24],0)
*               Reads (500mb GZ at IP2=24) - (500mb GZ at IP2=12) with IP3=0, and
*               writes in the output file: name=HZ, IP1=500, IP2=12 and IP3=24.
*               N.B. The number of parameters in [..,..,..,..] must be even.
DIRectives      End of directives.
```

3. Computes mean and zonal means on 500 mb height fields
```bash
pgsm -iment tape1 -ozsrt sorti -l out <<FINPGSM
   SORTIE(STD,4000,R)  
   HEURE(12,24,36)  
   GRILLE(TAPE1,10,20,30)  
*               Reads records of latitudes and longitudes from the input file,
*               with IP1=10, IP2=20 and IP3=30.
   CHAMP(ALL,TOUT)     
*               Interpolation of all fields at all levels, except UU and VV.
   ETIKSRT='ZMEANMER'  
*               Changes the label in the output file for the following fields.
   MOYENT(Z,MER,500,300) 
*               Computes the meridional mean of 500mb and 300mb geopotential, 
*               writes the output record with same descriptors as input file
*               except nj=1 and either TYPESRT, IP3SRT or ETIKSRT, one of which
*               must have been changed beforehand (ETIKSRT in this case).
   ETIKSRT='ZMEANZON'  
   MOYENT(Z,ZON,500)   
*               Computes the zonal mean of the 500mb geopotential, writes 
*               the output record with the same descriptors as input file 
*               except ni=1 and either TYPESRT, IP3SRT or ETIKSRT, one of which
*               must have been changed earlier (ETIKSRT in this case).
FINPGSM         End of directives
```

4. Does some computation on height fields. Uses LIREE, MOINSE, ECRITS.
```bash
pgsm -iment tape1 -ozsrt tape2 -l output <<FINPGSM

   SORTIE(STD,4000)    
   LIREE(U,'P',011593000,1000,12,0,'F050B25N')
*               Reads UU from input file, issued January 15 1993 00Z with
*               IP1=1000, IP2=12, IP3=0 and ETIKET='F050B25N', and saves it
*               in a temporary file also called an accumulator.
*               N.B. It is usually easier to use the default date -1.
   MOINSE(U,'P',011593000,1000,24,0,'F050B25N')
*               Reads UU from input file, issued January 15 1993 00Z with
*               IP1=1000, IP2=24, IP3=0 and ETIKET='F050B25N', and subtracts
*               it from the accumulator (12Z UU - 24Z UU).  The result is 
*               saved in the accumulator. Other directives of same type:
*               PLUSE, PLUSS, MOINSS.
   ECRITS('UM',-16,-1,1000,12,24,'P','F050B25N',-1,IMPRIM)
*               Writes in the output file: name='UM', compression set 
*               to 16 bits per data point, date of input file, IP1=1000, 
*               IP2=12, IP3=24, typvar='P', label='F050B25N', grid type
*               the same as input grid, and prints the arguments of 
*               LIREE and ECRITS in the 
*               diagnostic file.
FINPGSM         End of directives.

5. Does some computation on height fields. Uses LIREE, PLUSE, ECRITS.
```bash
pgsm -iment tape1 -ozsrt tape2 -l output <<ENDPGSM
   SORTIE(STD,4000)   
   LIREE(Z,'P',-1,1000,12,COMTEUR,'F050B25N')
   PLUSE(Z,'P',-1,1000,24,COMTEUR,'F050B25N')
*               Reads 'GZ', with IP1=1000, IP2=24, ETIKET='F050B25N' 
*               and IP3 is used as a register (COMTEUR) for the
*               number of fields read, and adds the field to the 
*               content of the accumulator, point to point.  In this
*               case: (GZ 12H + GZ 24H).
   MOYENE(1.0)
*               Computes the mean of accumulated fields, multiplied by 1.
   PFOIS(50.0,10.0,1.0)
*               Adds 50.0 to each point in the accumulator, multiplies by 10 
*               and divides by 1.
   ECRITS('ZM',-16,-1,1000,12,24,'P','F050B25N','G',IMPRIM)
*               Writes the output record: name='ZM', compression set 
*               to 16 bits, date from input file, IP1=1000, IP2=12, IP3=0, 
*               typvar='P', ETIKET='F050B25N' and GRTYP='G', and prints the
*               arguments of ECRITS in the diagnostic file.
   LIREE(Z,'P',-1,1000,12,0,'F050B25N')
   RACINE(1.0) 
*               Computes the square root of each point in the accumulator
*               and multiplies by 1.
   EXPON(10.0) 
*               Computes the exponential of each point in the accumulator 
*               and multiplies by 10.
   ALOGN(10.0) 
*               Computes the logarithm of each point in the accumulator
*               and multiplies by 10.
   ECRITS('ZM',-16,-1,1000,12,24,'P','F050B25N','G',IMPRIM)
*               Writes an output record: name='ZM', compression=16 bits, 
*               date from input file, IP1=1000, IP2=12, IP3=0, typvar='P',
*               ETIKET='F050B25N' and grid type='G', and prints the
*               arguments of ECRITS in the diagnostic file. 
FINPGSM         End of directives.
```