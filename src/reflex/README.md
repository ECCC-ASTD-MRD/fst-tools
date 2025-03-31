# Table of Contents
1. [Description](#description)
2. [Usage](#usage)
3. [Command line argument definition](#command-line-argument-definition)
4. [Notes](#notes)

# Description

Recuperation Et Fusion Logique d'Enregistrements XDF

* REFLEX is a utility used to retrieve all active records
* Records contained in one or more XDF files (up to 10) in order to on a single target file. Only those records will be recovered from the source files.

* REFLEX can also be used to reconstruct a file that has been damaged by the premature termination of execution of a program using in write mode (file not closed).

* REFLEX can also be used to obtain certain statistics about an XDF file.

# Usage

```bash
reflex -ixent ficent1 [ficent2 ... ficent10] [-oxsrt ficsrt] [-rstr] [-stats] [-errtolr niveau] [-msglvl niveau]
```

# Command line argument definition

|      key       |     description    | 
| -------------- | ------------------ |
| -ixent         | input XDF RPN standard file |
| -oxsrt         | output RPN XDF file |
| -rstr          | indicate to fix a corrupt input file, written ot -oxsrt |
| -stats         | displays input file statistics and state diagnostic. reflex will validate directory and file header|
| -errtolr       | indicate error tolerance above which a prematur exit will occur|
| -msglvl        | specify message level (TRIVIAL, INFORMATIF, WARNING, ERROR, FATAL and SYSTEM)|

# Notes
* When merging files, all files must be compatible with each other.
compatible with each other. This means they must have the same key descriptors
descriptors and an identical number of primary and auxiliary keys.

* If the FICSRT target file does not exist, REFLEX will create it using
using the descriptor parameters of the first source file encountered.

* In restore mode (presence of the -rstr key), a single input file
will be processed.