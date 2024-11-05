# Table of Contents
1. [Description](#description)
2. [Usage](#usage)
3. [Command line argument definition](#command-line-argument-definition)
4. [Directives description](#directives-description)
   1. [DESIRE](#desire)
   2. [EXCLURE](#exclure)
   3. [CRITSUP](#critsup)
   4. [ZAP](#zap)
   5. [PERIODE](#periode)
   6. [DEBUG](#debug)
   7. [DIAG](#diag)
   8. [ECR](#ecr)
   9. [LIMITE](#limite)
   10. [VOIRD](#voird)
   11. [VOIRS](#voirs)
   12. [FIXDATE](#fixdate)
   13. [SAUVDES](#sauvdes)
   14. [REWINDS](#rewinds)
   15. [STDCOPI](#stdcopi)

# Description

Utility used for editing and copying records from RPN standard files into a new or an existing RPN standard file. It can do a straight (complete) copy of the input file or it can copy records selectively as indicated from the standard input or from a file of directives named in the "-i" key.

# Usage

```bash
editfst [-s -d -sseq -dseq -i -l -e -v -vs -vd -c -nrecmin -k -m -n]
```
Examples of usage:
```bash
editfst -s input1 input2 input3 -d output0 -i command.dir -l out_list
editfst -s infile0 -d outfile0 -sseq -dseq
```

# Command line argument definition

|key     | description |
|--------|-------------|
| -s srcfile</br>-s filea fileb ... | one or multiple (up to 35) names of the input files; For the rest of the documentation, "srcfiles" will be referred as the files defined in here. Source files can also be defined in the STDCOPI in the stdin directives |
| -d dstfile | name of the destination (resulting/output) file; For the rest of the documentation, "dstfile" will be referred as the file defined in here. Destination files can also be defined in the STDCOPI in the stdin directives    |
| -sseq	      | indicates that the srcfiles is of type SEQuential |
| -dseq	      | indicates that the dstfile is of type SEQuential |
| -i 0</br>-i edit.dir | - a "0" means to concatenate all the srcfiles into the dstfile</br>- If key is omitted, this means utility expects directives from stdin</br>- If the key is given a name of the file, the utility will act on the directives stored in this file</br>- **(The directives given to the utility controls the selective copying from the source files to the destination file. These directives are documented in a table below.)** |
| -l listing  | file given for the stdout listing |
| -e 	      | A 'RND' record will be over-written if its search descriptors NOM,TYPE,IP1,IP2, IP3,ETIKET (except DATE) are identical to the ones of the record to be copied |
| -v	      | verbose mode, indicates which records are added to the dstfile like the format utility voir |
| -vs	      | prints the table of contents of the srcfiles like the format utility voir |
| -vd	      | prints the table of contents of the dstfile like the format utility voir  |
| -c 100	  | maximum number of records to be copied to the dstfile |
| -nrecmin 100| minimum number of records expected to be copied to the dstfile. If the number of records copied are less than this value, a non-zero status code will be returned, otherwise, it will return "0" (normal status code) |
| -k fatale	  | minimum error level to abort: 'fatale' or 'errors' |
| -m inform	  | message level: either 'inform' or 'errors' or 'debugs' |
| -n	      | Outputs no boxes around the message EXDB, EXFIN, at the beginning and at the end of the execution respectively|

# Directives description

* Directives must '''not''' start on the first column. (a "C" on the first colum indicates that it is commented out)
* Directives are implemented in sequential order so some will affect subsequent commands!
* The directives '''cannot''' have more than '''80 characters''' on one line. If the directive is longer than ''80'' characters, it can be continued on the next line provided that the split does not occur in the middle of a [list of elements] (surrounded by square brackets, or in the middle of quotes (character strings).

## DESIRE
Copies (desires) the records that meet the 7 selection parameters:
### Usage
```bash
DESIRE(TYPVAR,NOMVAR,ETIKET,DATE,IP1, IP2,IP3)
```

- **TYPVAR :** type of variable 'A'(analysis),'P'(forecast), ...
- **NOMVAR :** variable name
- **ETIKET :** label recorded from the original model run
- **DATE   :** CMC date timestamp,if the keyword COMMUNE is used, it will be controlled by the directive "PERIODE"
- **IP1    :** level of the field (in pressure,eta,metres,other)
- **IP2    :** hour of the forecast (rounded off if not exact)
- **IP3    :** value of 0 unless otherwise modified

_NOTE: If one wants to use supplementary selection parameters such as NI,NJ,NK,IG1,IG2,IG3,IG4,GRTYP, see the command CRITSUP below_

The selection parameters can be defined in these 3 forms:
- The universal key (-1) means no selection criteria for this parameter
- One value, integer or a character string surrounded by quotes
- A list of up to 10 elements in this form [?,?,?]

### Examples 
```text
# Examples of usage:
desire(-1,-1,-1,-1,-1,-1,-1)
desire('A','TT','REGIONAL',100185000,12000,6,0)
desire('P',['UU','VV'], -1 ,-1 ,[1000.,750.],[0,12,18,24],-1)
desire(['A','P'],'PN',-1,[100185000,@,100685120,DELTA,6],-1,-1,-1)
desire(-1,'TT','GLOBAL',COMMUNE,-1,0,0)
desire(-1,-1,-1,-1,[1.0,SIGMA],-1,-1)
```

For **DATE,IP1,IP2,IP3**, it can also be given a range with a starting and ending integer in the following form: **[start,@,end]**, and intervals can be added with the keyword 'DELTA' in the form **[start,@,end,DELTA,interval]**
- Examples for DATE
''Always use 'r.date' to see the actual date from the encoded timestamp.'' 
   - **[299823200,@,299888000,DELTA,6]**: From May 22,2002,00Z to May 25,2002, 00Z (inclusive) every 6 hours
   - **[299969000,@]**: From May 28,2002,18Z(included) and on.
   - **[@,299855600]**: Anything before and including May 23,2002, 12Z.
- Examples for IP2
   - **[@,120]**: All values up to 120 (hours)
   - **[12,@,48,DELTA,12]**: From hour 12 to hour 48, every 12 hours
- Examples for IP1:
   - **[@,12000]**: Anything up to 12000 for IP1 values
   - **[50,@,1000,DELTA,50]**: All values starting at 50mb up to 1000mb, every 50mb

In addition, for IP1 only, it can be defined with a level type and a level value in a pair form: [real value, level type] The level types recognized are:

- SIGMA (values from 0.0 to 1.0)
- MBAR (millibars)
- METERS(metres)
- OTHER
- HYBRID(values from 0.0 to 1.0)(revision 5.84 or higher)

-Examples only for IP1
   - **[100.,MBAR,750.,MBAR,1000.,MBAR]**: 100 mb, 750 mb, 1000 mb
   - **[100.,MBAR,@,750.,MBAR,DELTA,50.,MBAR]**: From 100 mb to 750 mb,every 50mb
   - **[@,750.,MBAR]**: anything lower and up to 750 mb
   - **[.859,SIGMA,1.0,SIGMA]**: sigma levels at 10590 and 12000 only

_NOTE: If both "DESIRE" and "EXCLURE" are not used, it will copy all **srcfiles** into the **dstfile**. The search of records to copy is done throughout the entire "random access" file or, from the current position of a "sequential" file. When there are fewer than 7 parameters defined, the last missing parameters are set to "-1"_


## EXCLURE
Excludes the records that meet the 7 selection parameters.
### Usage
EXCLURE(TYPVAR,NOMVAR,ETIKET, DATE,IP1, IP2,IP3)
See above in "DESIRE" for the description of the parameters. 

### Examples 
```text
exclure(A,TT,'REGIONAL',299823200,12000,6,0)
```

_NOTE: This command should be used before "DESIRE" as the directives are implemented in sequential order_


## CRITSUP
1 to 8 supplementary selection parameters that can be used with the DESIRE and EXCLURE directives.(This command only affects future calls to DESIRE/EXCLURE.)
### Usage
CRITSUP(NI,NJ,NK,GRTYP, IG1,IG2,IG3,IG4)

- **NI**: number of points along I
- **NJ**: number of points along J
- **NK**:number of points along K
- **IG1**: grid descriptor 1
- **IG2**: grid descriptor 2
- **IG3**: grid descriptor 3
- **IG4**: grid descriptor 4
- **GRTYP**: grid type ('E','G','L',etc.)

The selection parameters can be defined in these 2 forms.
- The universal key (-1) means no selection criteria for this parameter
- One value, integer or for GRTYP, one character string surrounded by quotes

_NOTE: The "critsup (-1)" will negate the previous "critsup" command for the EXCLURE/DESIRE commands that follow afterwards_
 	
### Examples 
```text
critsup(400,200,-1,'G',-1,-1,-1,-1)
exclure('A',-1,-1,-1,-1,-1,-1)
desire(-1,['UU','VV'],-1,-1,-1,-1,-1)
critsup(-1)
desire(-1,'TT',-1,-1,-1,-1,-1)
```


## ZAP
Modifies the record parameters during the copy by replacing with the values given in the arguments of this directive.
### Usage
ZAP(TYPVAR,NOMVAR,ETIKET,DATE, IP1,IP2,IP3)

Only one value can be given per argument as this is **NOT** a selection criteria. If the value is -1, no modification will be done for that argument. If several **ZAP** commands appear, only the last one is applicable. Use STDCOPI if one wants to use several sets of a selection and modification **ZAP** commands

### Examples 
selects all 'TT' records and then replaces in each of these selected records, the **typvar** with 'A' and the etiket with 'GLBANAL'
```text
desire(-1,'TT',-1,-1,-1,-1,-1)
zap('A',-1,'GLBANAL',-1,-1,-1,-1)
```

selects all 'UU' records and then replaces in each of these selected records, the **etiket** with 'HELLO' and writes it out. Then it selects all the TT records and then writes them out and replaces the **typvar** with 'A' and the **etiket** with 'GOODBYE' at the same time.)

```text
zap (-1,-1,'HELLO' ,-1,-1,-1,-1)
desire (-1,'UU',-1,-1,-1,-1,-1)
stdcopi (-1)
zap ('A',-1,'GOODBYE',-1,-1,-1,-1)
desire (-1,'TT',-1,-1,-1,-1,-1)
stdcopi (-1)
``` 

_NOTE: When some parameters are missing, the value of -1 is the default_


## PERIODE
Sets a common date or time period for the following DESIRE and EXCLURE directives when 'COMMUNE' is used in the argument of DATE. This should be called before DESIRE/EXCLURE.
### Usage 
PERIODE(DN, ECART, DUREE, DELTA)

- **DN** : Argument used to call IOPDATM.
:It may take one of the following forms:
:
:'OPRUN'- operationnal run date
:YYJJJZZ - a positive number; year,julian day,zulu hr
:-CMCSTAMP - a negative integer; CMC date timestamp (-299823200).
- **ECART**: Number of hours added to the date received from IOPDATM to obtain the beginning of the period
- **DUREE**: Duration of the period in hours from the start of the period
- **DELTA**: The interval in hours desired from the start of the period NOTE: Omitted arguments get their default value.

### Examples 
```text
PERIODE('OPRUN',6,240,6)
desire(-1,'TT','GLOBAL',COMMUNE,-1,0,0)
PERIODE(9930000,6,24,1)
desire(-1,'UU','GLOBAL',COMMUNE,-1,0,0)
PERIODE(-299823200,0,12,3)
desire(-1,'VV','GLOBAL',COMMUNE,-1,0,0)
PERIODE(#, 0, 0, 1)
desire(-1,'WW','GLOBAL',COMMUNE,-1,0,0)
```

## DEBUG 
### Usage 
DEBUG=OUI/NON [ NON ]
- OUI: Same meaning as the "-m debugs" key
- NON: Cancels the effect of a "DEBUG=OUI" directive or of a "-m" key

## DIAG
### Usage 
DIAG=OUI/NON [ NON ]
- OUI: Same meaning as the "-m inform" key
- NON: Cancels the effect of a "DIAG=OUI" directive of of the "-m" key

## ECR
### Usage 
ECR=OUI/NON[ NON ]
- OUI: Same meaning as the "-e" key
- NON: Cancels the effect of a "ECR=OUI" directive or that of the "-e" key

## LIMITE
### Usage 
LIMITE = N [ -1 ]
- N > 0 Copy up to N number of records into the dstfile
- N < 0 Copy unlimited number of records into the dstfile

Same as the "-c" key

## VOIRD 
### Usage 
VOIRD = OUI/NON [ NON ]
- OUI: Same meaning as the "-vd" key
- NON: Cancels the effect of a "VOIRD=OUI" directive or of a "-vd" key

## VOIRS
### Usage 
VOIRS = OUI/NON [ NON ]
- OUI: Same meaning as the "-vs" key
- NON: Cancels the effect of a "VOIRS=OUI" directive or of a "-vs" key

## FIXDATE
### Usage 
FIXDATE = OUI/NON [ NON ]
- OUI: Indicates that the date of the first record is valid for all the records in the srcfiles
- NON: Indicates to check the date for each record in the srcfiles

## SAUVDES
### Usage 
SAUVDES = N [ O ] Controls the validity of the directives DESIRE or EXCLURE after copy
- N =-1 The DESIRE/EXCLURE directives stay valid
- N > 0 The first N DESIRE/EXCLURE directives stay valid
- N = 0 The DESIRE/EXCLURE directives become invalid

## REWINDS
### Usage 
REWINDS(SFILE) [#]
Opens the sequential file 'SFILE' and goes to the beginning.

;SFILE
: Name of the file to be rewound (string surrounded by quotes)
### Examples
```text
REWINDS('SEQFILE')
```

## STDCOPI
Copies, in the conditions set by the previous directives, a selection of input file records to the output file.
### Usage
STDCOPI(SFILE,TYPS,DFILE,TYPD) [#, -1, -1, -1]

- **SFILE**: Input filename (character string with quotes)
- **TYPS**: Type of the input file: 'RND' or 'SEQ'
- **DFILE**: Output filename
- **TYPD**: Type of the output file: 'RND', 'SEQ'
### Examples
Add the contents from files 'SX'(RND), 'SY'(SEQ) and 'SZ'(SEQ) to the file 'yyy'(RND)
```text
STDCOPI('SX', 'RND', 'yyy', 'RND')
STDCOPI('SY', 'SEQ')
STDCOPI('SZ')
```

Other examples: look at ZAP

### Notes

- STDCOPI can change the input and output files on the fly.
-  The names and type of files don't change if:
   1. they have an actual value of -1
   2. unspecified (argument list abbreviated)
- If the input file is sequential, the record search is done starting from its current position.
- If the output file is sequential, EDITFST writes to it from its current position.

