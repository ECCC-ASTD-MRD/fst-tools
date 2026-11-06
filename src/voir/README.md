# Table of Contents
1. [Description](#description)
2. [Usage](#usage)
3. [Command line argument definition](#command-line-argument-definition)
4. [-style tokens](#-style-tokens)
5. [Changes since 20.0](#changes-since-200)

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
| -style         | selects which columns are shown (see [-style tokens](#-style-tokens)) |
| -morehelp      | print a detailed description of the -style tokens |

# -style tokens

`-style` selects which columns are printed. A non-empty `-style` starts from a
**base state**; each recognized token overrides it; unrecognized tokens are
ignored. Tokens are **case-insensitive** and may be separated by `+` or spaces.

- **`-style` absent** → the *default columns* (see below).
- **bare `-style`** (present, no value) → the 20.0 bare default
  (`NINJNK+DATEV+LEVEL+GRIDINFO`).
- **non-empty `-style`** → base state + the effect of each token.

**Base state** (non-empty `-style`, no recognized token):
`NOMV TV ETIQUETTE IP1 IP2 IP3 DEET NPAS DTY SIZ`

**Default columns** (when `-style` is absent):
`NOMV TV ETIQUETTE NI NJ NK (DATE-O h m s) IP1 IP2 IP3 DEET NPAS DTY SIZ G IG1 IG2 IG3 IG4`

## Tokens that ADD a column (off in the base state)

| Token | Aliases | Effect |
|-------|---------|--------|
| `NINJNK` | | show the dimensions (NI NJ NK) |
| `DATEO` | | show the origin date (DATE-O) |
| `DATESTAMPO` | | show the origin date stamp (STAMP-O) |
| `DATEV` | | show the valid date (DATE-V) and its stamp (STAMP-V) |
| `LEVEL` | | show the vertical level (LEVEL) |
| `DECODE` | `IPALL` | show the decoded IP block |
| `GRID` | `GRIDINFO` | show the decoded grid (XG1..XG4) |
| `IG1234` | | show the grid descriptors (IG1..IG4) |
| `META` | | show the extended metadata (JSON) |
| `FULL` | | `NINJNK+DATEV+DECODE+GRID` (all three IPs stay on) |

## Tokens that REMOVE a column (on in the base state)

| Token | Aliases | Effect |
|-------|---------|--------|
| `NONOMVAR` | `NONOMV` | hide the variable name (NOMV) |
| `NOTYPVAR` | `NOTYPV` | hide the field type (TV) |
| `NOETIKET` | `NOETIQ` | hide the label (ETIQUETTE) |
| `NOIP1` | | hide IP1 |
| `NOIP2` | | hide IP2 |
| `NOIP3` | | hide IP3 |
| `NOIP23` | | hide IP2 and IP3 (the 20.0 token) |
| `NOIPS` | | hide IP1, IP2 and IP3 |
| `NODEET` | | hide DEET |
| `NONPAS` | | hide NPAS |
| `NODATYP` | `NODTY` | hide the data type (DTY) |
| `NOSIZ` | | hide the data size (SIZ) |

# Changes since 20.0

- **`SIZ` column added** (on by default, opt out with `NOSIZ`). In 20.0 the data
  size was printed inside the combined `DTYP` column; 20.1 splits it into a
  `DTY` column (type letter only) and a separate `SIZ` column (data bits).
- **`IP1` is on by default.** In 20.0 `IP1` was off unless requested; 20.1 shows
  it in the base state (opt out with `NOIP1`).
- **`NOSTAMP` removed.** 20.1 uses separate `DATEO` / `DATESTAMPO` / `DATEV`
  tokens instead of a single stamp toggle.
- **20.0 token names are kept as aliases** (`NONOMV`, `NOTYPV`, `NOETIQ`,
  `NODTY`, `NOIP23`, `IPALL`, `GRIDINFO`, ...), so existing `-style` strings
  keep working. The canonical names are the longer forms in the tables above.
- **New tokens** not present in 20.0: `DATESTAMPO`, `NOIP1`, `NOIP2`, `NOIP3`,
  `NOIPS`, `NOSIZ`, `META`.
- **`FULL`** is `NINJNK+DATEV+IPALL+GRIDINFO`. Unlike 20.0 (whose `FULL`
  suppressed IP2/IP3 via `NOIP23`), 20.1's `FULL` keeps all three IPs on.

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
