# Table of Contents
1. [Description](#description)
2. [Usage](#usage)
3. [Notes](#notes)

# Description
fstcompress / fstuncompress are two utilities used to compress / decompress the contents of an RPN standard file using in-house compression algorithms. **The compression process is completely lossless**, and is done within every record of the standard file. Once compressed, and contrary to standard compression utilities like bzip2 and gzip, the resulting standard file can be used without further manipulation by the standard RPN utilities and your programs (i.e. there is no need to decompress the file "on the fly") .

The compression ratio depends upon the nature of the fields, and is typically about 2-2.75 for operational files but can go as high as 20 for vary sparse data fields. In general, the compression ratio increases with the grid resolution: high resolution grids (2-5 km) compress much better than coarser resolution grids (50-100 km).

There are two different compression algorithms :
- one based on the integer tokenisation typically used in the RPN standard files,
- the other based on the IEEE floating point format. The latter got introduced in the version 3.00 of fstcompress / fstuncompress.

fstcompress chooses the algorithm based on datyp. If "datyp" of the source field is 1, the integer tokenisation algorithm is chosen... if datyp is 5, then the IEEE format algorithm is chosen.

There are very good reasons to compress your RPN standard files
- It effectively doubles or triples your storage capacity, either on disk or on CFS.
- Conversely, it doubles or triples your network bandwidth, by having 2-3 times less data to transfer.
- Once compressed, the savings are permanent.
- The decompression overhead is negligible, i.e. the cost of decompressing the data on the fly is not significant. In fact recent timings showed the wall clock processing time to be shorter with source compressed RPN files than uncompressed ones.
- It does not alter the precision of your data.

# Usage

```bash
fstcompress -fstin uncompressed.fst -fstout compressed.fst -level [best / fast] -nbits [custom_nbits]
fstuncompress -fstin compressed.fst -fstout uncompressed.fst
```

In fstcompress, the default value for level is **best**.

The only reason why one would want to use **fstuncompress** is for using the standard files with binaries produced with versions of rmnlib older than librmn_008.

There are 3 PDF documents that explain the compression method :

- The [following PDF document](https://wiki.cmc.ec.gc.ca/w/images/e/e1/Compression.pdf) describes the technique used to compress the data
- The [following PDF document](https://wiki.cmc.ec.gc.ca/w/images/7/78/Floating-point-compression.pdf) describes the technique used to compress 32-bit IEEE floating point data
- [This presentation (PDF)](https://wiki.cmc.ec.gc.ca/w/images/4/4d/Presentation-compression.pdf) explains in deeper details the method used and some statistics about the performance of our in-house method compared to two other popular compression programs (bzip and gzip2)

# Notes

To compare the results between the compressed and uncompressed data files, it is recommended to re-compress the source data file, because the decompaction-recompaction process induces some minor, machine-precision numerical variations in the datafiles. Here is a sample test run.

```bash
fstuncompress -fstin file.fst -fstout file_u.fst
fstcompress -fstin file.fst -fstout file_z.fst
fstcomp -a file_z.fst -b file_u.fst
```

The output of fstcomp should indicates no significant differences between the records.

The compression algorithms can also be activated by an environment variable. Before invoking your application, simply use
```bash
export FST_OPTIONS="DATATYPE_REMAP=1,134 2,130 4,132 5,133"
```

and any field that would have been written with datyp=1 will be written with datyp=134, i.e. "real compressed" (for nbits 16 and less only)
