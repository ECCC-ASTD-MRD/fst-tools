# Table of Contents
1. [Description](#description)
2. [Usage](#usage)
3. [Command line argument definition](#command-line-argument-definition)
4. [Notes](#notes)

# Description

Zonal diagnostics debaler 

* The accumulation vectors written during integration are separated by variables for specific bands.

* This interface program reads a standard file **NOUTZON** and produces the **ZONFILE** file. This file is required for latitude band variable plotting (GRAPHZN).
  
* The **NOUTZON** standard file is no longer required after execution of dbzono. All information required for variable and latitude band are written in **ZONFILE**.

* Although it can handle other types of averaging (i.e. by region) dbzono has been designed specifically for GRAPHZN.

# Usage

```bash
reflex -ixent ficent1 [ficent2 ... ficent10] [-oxsrt ficsrt] [-rstr] [-stats] [-errtolr niveau] [-msglvl niveau]
```

# Command line argument definition

|      key       |     description    | 
| -------------- | ------------------ |
| -iszonal       | name of standard input file |
| -omzonal       | name of standard output file |
| -echoz         | diagnostics are printed or not during execution |
| -date          | oprun/yes/no (optional)|

* NOUTZON CONTENT
   * An integration with zonal diagnostics will have produced a file containing control records and extraction records. At each NPTEMPS, an information table is updated. This table contains the grid dimensions, the time step number of bands, number of steps for the experiment and a control key required for clone integration.

   * The file also contains the list of variables to be extracted, the relative weights of each grid point with respect to the area and its rank (its position in the accumulation vector). This information is written only once at the start of integration. In addition, records contain model levels and relative band positions. This information forms the axes for the graphics software.

   * The accumulators in which the model variables are averaged or sampled are written to a so-called extraction record.The squares of the averages can also be calculated. These records will be saved at the same time.

* ZONFILE CONTENT
   * Each variable is averaged over a specific period of time and there may be several, depending on the duration of the integration. For example, for a 48-hour integration, you may want to save averages every 12 hours or 24 hours. The output file therefore contains these averages for each periods. Recordings containing multiple levels can be viewed with REC. This file conforms to the requirements required for GRAPHZN, the strip chart of variables.

# Notes
* The unpacker can handle a file from an integration MIPS (32-bit) or NEC (64-bit/32-bit) integration.  
* The content of the NOUTZON file is written in such a way that it can be read from a server. Compaction is 32-bit and the information table contains integers only. The list of  variables is written in R4A format. Accumulation vectors are written in IEEE format.

* Required routines: BSORT,DEBALZN,LITZON,OUTFLD,ZON

# Examples
* When DBZONO is run, the first record read is the information table. This table appears as follows:

```
    LITZON - control table
    ==========================
    NDELTAT= 24
     DELTAT= 1800
       MODE= 3
         NI= 128
         NJ= 64
         NK= 21
       NBIN= 64
      SOMNK= 107
     LATMIN= -90
    ==========================
```
* These parameters have been defined before integration and written into a control record. Diagnostics are saved every 24 steps of 1800 seconds, i.e. every 12 hours. Averages and average squares are saved (MODE=3). There are 64 latitude bands and the final position of the last variable extracted is 107 in the accumulation vector. There are 128 by 64 grid points in the horizontal and 21 levels in the vertical.

* Playback of the next two recordings returns the levels and the relative position of the latitude bands. Intermediate levels will be calculated if they have not been in the file.

* After reading the variable control record, the list of extracted variables will be written, as well as the variable, i.e. whether it is a sampled or averaged variable and its relative position in the accumulation vector.

```
    LITZON - list of variables
    ============================
    .TS.           1 2
    +VE. 0 3
    .UU> 1 24
    .VV> 1 45
    .SS.           1 66
    .TT.           1 87
    .TT.           1 108
    ============================
     ETIKEX= SEFEXP27
```

* Even if the variable contains 4 characters, only the first three are used. The second column indicates whether the variable is a sampled(1) or averaged(0) variable. The . or + character in the variable name indicates the same thing. The third column indicates whether it's a vertical slice or not. the position indicator of the vertical levels corresponding to this variable in the extraction slice. There are two more variables in the list where the has been encoded. In the example, the label indicates that this is a global model integration.
 
* With this information, it is possible to read the accumulators and by variables for each period saved.






