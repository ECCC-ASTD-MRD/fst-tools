# voir `-style`

The vocabulary for `voir`'s `-style` option, which selects which columns appear
in the table of contents.

## Language

**Column**:
One field of a record shown as a vertical slice of the table of contents
(e.g. the variable name, the origin date, `IP1`).
_Avoid_: field, attribute

**Token**:
A keyword in the `+`-separated `-style` string that turns a column on or off.
_Avoid_: flag, option, switch

**Base state**:
The set of columns shown when a *non-empty* `-style` contains no recognized
token. It is the starting point that each token then overrides.
_Avoid_: default, initial state

**Default columns**:
The set of columns shown when `-style` is *absent*. Produced by substituting the
default style string, so it is generally a superset of the base state.
_Avoid_: base state

**Default style string**:
The token string `voir` substitutes when `-style` is absent.
_Avoid_: default columns
**Bare -style**:
A `-style` that is present on the command line but carries no value. It is
distinct from an *absent* `-style`: a bare `-style` shows the 20.0 bare default
(`NINJNK+DATEV+LEVEL+GRIDINFO`), whereas an absent `-style` shows the default
columns.
_Avoid_: empty -style
**Canonical token**:
The 20.1 name for a token, the one shown in `-morehelp` and the documentation.
_Avoid_: primary token

**Alias**:
A 20.0 token name kept for backward compatibility that has the same effect as a
canonical token.
_Avoid_: synonym, legacy name

**Override model**:
The token model in which a non-empty `-style` starts from the base state and each
recognized token then overrides the state of its column (on-tokens turn a column
on, off-tokens turn it off). Unrecognized tokens are ignored.
_Avoid_: explicit-set, flag model

**Expanded date**:
A date column rendered as `YYYYMMDD HHMMSS` (e.g. `20260901 123000`).
_Avoid_: human date

**Stamp date**:
A date column rendered as the raw integer stamp (e.g. `491383250`).
_Avoid_: compact date
