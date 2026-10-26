#!/usr/bin/env python3
"""Generate the input / expected-output FST files for the fst-tools tests.

For each test case it writes an ``_in`` and an ``_out`` file, in both RSF and
XDF formats. Every record goes into ``_in``; only records flagged
``in_output`` go into ``_out``. Files are written to the current working
directory (bare filenames), matching how the test scripts locate them.
"""

import os

import numpy as np
import rmn

# 1x1x1 field of value 1.0 (float32), Fortran-ordered. Shared by every record.
DUMMY = np.array([1.0], dtype="f4", order="F")

# (test_name, test_id, nomvar, typvar, etiket, ip1, dateo, in_output)
#   dateo: origin date as a "YYYY-MM-DD HH:MM:SS" string, or None for "no date".
#   in_output: whether the record is written to the expected-output file.
# **Records for the same test case must be contiguous in this list.**
TEST_RECORDS = [
    ("01_editfst_desire", 1, "A", "B", "FOR_TEST_01", 2, None, 0),
    ("01_editfst_desire", 1, "A", "B", "FOR_TEST_01", 3, None, 1),
    ("01_editfst_desire", 1, "A", "B", "FOR_TEST_01", 4, None, 0),

    ("02_editfst_exclure", 2, "AA", "B", "FOR_TEST_02", 1, None, 1),
    ("02_editfst_exclure", 2, "TT", "B", "FOR_TEST_02", 1, None, 0),
    ("02_editfst_exclure", 2, "BB", "B", "FOR_TEST_02", 1, None, 1),
    ("02_editfst_exclure", 2, "HU", "B", "FOR_TEST_02", 1, None, 0),
    ("02_editfst_exclure", 2, "CC", "B", "FOR_TEST_02", 1, None, 1),

    # Records for the `voir` test. Only the `_in` file is used (voir just
    # prints the table of contents), so in_output is 0 for all of them. The
    # metadata is chosen to be distinct and easy to grep for in voir's output.
    # dateo is set to 2026-09-01 12:30:00 so the DATE-O column shows a
    # readable date and a nonzero time instead of zeros.
    ("03_voir_basic", 3, "VOI", "R", "VOIR_ETI_1", 11, "2026-09-01 12:30:00", 0),
    ("03_voir_basic", 3, "VOI", "R", "VOIR_ETI_2", 22, "2026-09-01 12:30:00", 0),
    ("03_voir_basic", 3, "VOI", "R", "VOIR_ETI_3", 33, "2026-09-01 12:30:00", 0),

    # Records for the `editfst -ds` (sequential destination) regression test.
    # All records are included in the expected output so that a full copy of
    # the input is the expected result.
    ("04_editfst_ds", 4, "DS", "B", "DS_ETI_1", 1, None, 1),
    ("04_editfst_ds", 4, "DS", "B", "DS_ETI_2", 2, None, 1),
    ("04_editfst_ds", 4, "DS", "B", "DS_ETI_3", 3, None, 1),
]


def make_record(nomvar, typvar, etiket, ip1, dateo):
    """Build an ``fst_record``

    Starts from ``default_fst_record`` and overrides most of the fields with either
    the given inputs or hardcoded values. ``grtyp`` and ``datev``
    are deliberately left at their default values.
    """
    # dateo is a "YYYY-MM-DD HH:MM:SS" string in the spec (or None for no date).
    rec = rmn.fst_record(
        data_type=rmn.FstDataType.FST_TYPE_REAL,
        data_bits=32,
        pack_bits=32,
        ni=1, nj=1, nk=1,
        deet=0, npas=0,
        dateo=rmn.encode_date(dateo) if dateo is not None else 0,
        ip1=ip1, ip2=1, ip3=1,
        ig1=1, ig2=1, ig3=1, ig4=1,
        nomvar=nomvar, typvar=typvar, etiket=etiket,
        data=DUMMY,
    )
    return rec


def gen_files(is_rsf):
    """Generate the ``_in``/``_out`` files for one backend (RSF or XDF)."""
    ext = "rsf" if is_rsf else "xdf"
    options = "RSF+R/W" if is_rsf else "XDF+R/W"

    file_in = None
    file_out = None
    previous_test_id = -1

    for (test_name, test_id, nomvar, typvar, etiket, ip1, dateo, in_output) in TEST_RECORDS:
        if test_id < previous_test_id:
            raise ValueError("Test IDs must monotonically increase")
        if test_id > previous_test_id:
            # Close the previous test's files.
            if file_in is not None:
                file_in.close()
                file_out.close()

            # Open the next test's files (removing any stale copies first).
            filename_in = f"{test_name}_in.{ext}"
            filename_out = f"{test_name}_out.{ext}"
            for path in (filename_in, filename_out):
                if os.path.exists(path):
                    os.remove(path)
            file_in = rmn.fst24_file(filename=filename_in, options=options)
            file_out = rmn.fst24_file(filename=filename_out, options=options)

        rec = make_record(nomvar, typvar, etiket, ip1, dateo)
        file_in.write(rec, rewrite=False)
        if in_output:
            file_out.write(rec, rewrite=False)

        previous_test_id = test_id

    if file_in is not None:
        file_in.close()
        file_out.close()


def main():
    gen_files(is_rsf=True)
    gen_files(is_rsf=False)


if __name__ == "__main__":
    main()
