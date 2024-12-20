# Table of Contents
1. [Description](#description)
2. [Usage](#usage)
3. [Command line argument definition](#command-line-argument-definition)
4. [Description of Report Heading](#description-of-report-heading)

# Description

Utility used to compare the contents of two RPN standard files (record by record). The comparison is done by looking at the records in file A sequentially and for each record, look in file B for the equivalent record before making a comparison of the values stored within this field. The output of the comparison report is through standard output but maybe redirected given a filename in the "-l" key.<br />

# Usage

```bash
fstcomp [-a -b -l -as -bs - af -bf -li -nd -ne -d -n -va -vb -nt -n1 -n2 -n3 -nn -x]
```
<br />
Examples of usage:
```bash
fstcomp -a filea -b fileb
fstcomp -a filea -b fileb -ne -nd -nt
fstcomp -va filea -vb fileb -l listing
fstcomp_7.7 -a fichier1 -b fichier2 -packerr 1
```

# Command line argument definition

|      key       |     description    | 
| -------------- | ------------------ |
| -a ''filea''   | filename of one of the two input RPN standard files for comparison. Default filename is "a" |
| -b ''fileb''   | filename of the other input RPN standard file for comparison. Default filename is "b" |
| -l ''listing'' | file given for the stdout listing |
| -as            | obsolete do not use |
| -bs            | obsolete do not use |
| -af            | obsolete do not use |
| -bf            | obsolete do not use |
| -li            | the exponent of the maximum relative error tolerance. Example: ```-li =-3``` means that FSTCOMP will tolerate differences not greater than 0.001. The default value is E-07 (.0000001) |
| -ld            | the exponent of the maximum error between values before considering them equal. Example: ```-ld =-4``` means that FSTCOMP will consider values to be equal if differences not greater than 0.001. The default value is E-32  |
| -packerr       | the multiplier (usually 1) on the tolerance criterion (which is the compaction error) for the comparison. Example: Tolerance criterion for R16 (16bit compaction) = (Max - Min) / 2**16 If the difference between two values are smaller than (tolerance)*(packerr), it will be tagged as no difference |
| -nd            | To ignore the "date validity" parameter when searching for the equivalent record in file "B" to the record in file "A". (datev=dateo+npas*deet) |
| -ne            | To ignore the "etiket" parameter when searching for the equivalent record in file "B" to the record in file "A" |
| -ng            | To ignore the grid validation when searching for the equivalent record in file "B" to the record in file "A" |
| -d             | To print more information than just the standard comparison |
| -n             | Outputs no boxes around the message EXDB, EXFIN, at the beginning and at the end of the execution respectively |
| -va ''filea''  | filename of one of the two input RPN random standard files for comparison and will print a table of contents of this file in a "voir" style. If the key is mentioned, the default filename would be "va" |
| -vb ''fileb''  | filename of the other input RPN random standard files for comparison and will print a table of contents of this file in a "voir" style. If the key is mentioned, the default filename would be "vb" |
| -nt            | To ignore the "typvar" parameter when searching for the equivalent record in file "B" to the record in file "A" |
| -n1            | To ignore the "ip1" parameter when searching for the equivalent record in file "B" to the record in file "A" |
| -n2            | To ignore the "ip2" parameter when searching for the equivalent record in file "B" to the record in file "A" |
| -n3            | To ignore the "ip3" parameter when searching for the equivalent record in file "B" to the record in file "A" |
| -nn            | To ignore the "nomvar" parameter when searching for the equivalent record in file "B" to the record in file "A" |
| -x             | To specify that the "datyp" of the records in the files are of type "X" (ie: X32) - raw binary mode. Normally the "datyp" is in real "R" or IEEE "E" style representation |

# Description of Report Heading

|  Title    | Description |
| --------- | ----------- |
| CLEA      | key of record found in file A |
| CLEB      | key of record found in file B |
| NOM       | nomvar - variable name of field |
| ETIKET    | etiket - label of run/data |
| IP1       | ip1 - code of model level [../../LIBRARY_INDEX/c/convip.html (see convip)] |
| IP2       | ip2 - hour of forecast |
| IP3       | ip3 |
| E-REL-MAX | maximum relative error =&gt; |1.0-A/B| where values A and B are compared. B must be non-zero so A and B could be switched for the purpose of calculation |
| E-REL-MOY | average relative error. See E-REL-MAX |
| VAR-A     | variance of field in file A |
| C-COR     | correlation (1.000 if fields are equal) |
| MOY-A     | mean of field in file A |
| BIAIS     | bias (0.000 if fields are equal) |
| E-MAX     | maximum absolute error (0.000 if fields are equal) |
| E-MOY     | average absolute error (0.000 if fields are equal) |