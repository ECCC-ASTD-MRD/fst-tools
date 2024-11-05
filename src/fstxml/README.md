# Table of Contents
1. [Description](#description)
2. [Usage](#usage)
3. [Example](#example)

# Description
Convert an RPN Standard File to XML

# Usage
fst2xml(fstFile, xmlFile, encoding, format, tokens_per_line)
- fstFile : the input file name, in FSTD format
- xmlFile : the output file name, in XML format
- encoding : unused
- format : format to be used for printing (limited to 32 characters)
- tokens_per_line : the number of output values to be printed on a single line.  This must match 'format'.

# Example 

```bash
fst2xml('my_fst', 'new_xml', 'unused', '14.6g', 5)
fst2xml -fst fichier.fst
```

