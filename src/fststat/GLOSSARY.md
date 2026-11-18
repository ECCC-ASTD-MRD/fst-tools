# fststat

The vocabulary for `fststat`, the tool that reads every record in an FST file
and prints one line of statistics (mean, standard deviation, min, max) per
record.

## Language

**Stats line**:
The single line `fststat` prints for a record it successfully reads, carrying
the record's identifying metadata followed by its `Mean:`, `StDev:`, `Min:`,
and `Max:` values.
_Avoid_: output line, result line

**Skip**:
The path taken when `fststat` cannot read a record's data: it logs a warning
and moves on to the next record without printing a stats line. A *skipped*
record produces no stats line.
_Avoid_: error, failure, drop

**Valid type**:
An FST data type and bit-width that `fststat` reads and reports a stats line
for.
_Avoid_: supported type, readable type

**Invalid type**:
An FST data type and bit-width that `fststat` mishandles (e.g. 64-bit data):
it may log warnings or errors and report garbage values. The behavior for an
*invalid type* is observed, not specified.
_Avoid_: unsupported type, unhandled type

**Special field**:
A field that `fststat` mishandles, either because of its type (an *invalid
type*) or because of its data values (NaN, ±Inf). Special fields are tested
separately from the *valid types*.
_Avoid_: edge case, pathological field
