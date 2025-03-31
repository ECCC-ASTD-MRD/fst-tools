# Table of Contents
1. [Description](#description)
2. [Usage](#usage)
3. [Command line argument definition](#command-line-argument-definition)

# Description

Lists the table of contents of the given RPN standard file (random or sequential)

# Usage

```bash
voir -iment fichier [-seq -style] 
```

Examples of usage:
```bash
voir -iment stdrnd01 -style DATEV+LEVEL+IP1
voir -iment stdseq00 -seq
```

# Command line argument definition

|      key       |     description    | 
| -------------- | ------------------ |
| -iment ''file''| input RPN standard file |
| -seq           | indicate the file is a RPN standard sequential |
| -style         | defines how parameters are displayed as a combination(+) of NOMVAR TYPVAR ETIKET NIJK DATEO DATEV STAMP LEVEL DATYP DEET NPAS IP1 IP2 IP3 IPS DECODE GRID IGS META|


| Datatype | DATYP in fstecr | character in voir |
|----------|-----------------|-------------------|
| binary, transparent | 0  X |
| floating point | 1 | R |
| unsigned integer | 2 | I |
| character (R4A in an integer) | 3 | C |
| signed integer | 4 | S |
| IEEE floating point | 5 | E |
| floating point (16 bit, made for compressor) | 6 | F |
| character string | 7 | A |
| complex IEEE | 8 | Z |
| compressed short (unsigned) integer | 130 | i |
| compressed IEEE | 133 | e |
| compressed floating point | 134 | f |
