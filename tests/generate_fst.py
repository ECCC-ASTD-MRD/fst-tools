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

# Data arrays for the `fststat` test: a 3-element field with values 1, 2, 3 so
# that min=1, max=3, mean=2 are all distinct and known. The dtype matches the
# record's data_type/data_bits (see fst_type_to_numpy_type).
FSTSTAT_DATA_F4 = np.array([1.0, 2.0, 3.0], dtype="f4")
FSTSTAT_DATA_I4 = np.array([1, 2, 3], dtype="i4")
FSTSTAT_DATA_U4 = np.array([1, 2, 3], dtype="u4")
FSTSTAT_DATA_F8 = np.array([1.0, 2.0, 3.0], dtype="f8")
FSTSTAT_DATA_I8 = np.array([1, 2, 3], dtype="i8")
# 4-element fields with a non-finite value, for the `fststat` "special" test.
FSTSTAT_DATA_PINF = np.array([1.0, 2.0, 3.0, np.inf], dtype="f4")
FSTSTAT_DATA_NINF = np.array([1.0, 2.0, 3.0, -np.inf], dtype="f4")
FSTSTAT_DATA_NAN = np.array([1.0, 2.0, 3.0, np.nan], dtype="f4")
# 4-element fields with NO finite value, for the `fststat` "special" test.
FSTSTAT_DATA_ALLINF = np.array([np.inf, np.inf, np.inf, np.inf], dtype="f4")
FSTSTAT_DATA_ALLNAN = np.array([np.nan, np.nan, np.nan, np.nan], dtype="f4")
FSTSTAT_DATA_MIXNF = np.array([np.nan, np.inf, np.nan, np.inf], dtype="f4")

# 4-element fields for the `fstcomp` non-finite test. File A and file B share
# identical record metadata; only the data of some records differs (see the
# 08_fstcomp_nonfinite_a / _b cases below). The last element is the one that
# varies between the two files.
FSTCOMP_BASE = np.array([1.0, 2.0, 3.0, 4.0], dtype="f4")
FSTCOMP_FIN_DIFF = np.array([1.0, 2.0, 3.0, 9.0], dtype="f4")   # finite vs finite
FSTCOMP_NAN = np.array([1.0, 2.0, 3.0, np.nan], dtype="f4")
FSTCOMP_PINF = np.array([1.0, 2.0, 3.0, np.inf], dtype="f4")
FSTCOMP_NINF = np.array([1.0, 2.0, 3.0, -np.inf], dtype="f4")

# (test_name, test_id, nomvar, typvar, etiket, ip1, dateo, in_output,
#  data_type, data_bits, pack_bits, data)
#   dateo: origin date as a "YYYY-MM-DD HH:MM:SS" string, or None for "no date".
#   in_output: whether the record is written to the expected-output file.
#   data_type/data_bits/pack_bits/data: the record's data parameters. None for
#     the first three means "use the 1x1x1 float32 dummy"; None for data means
#     "use the dummy array". The fststat test sets these explicitly.
# **Records for the same test case must be contiguous in this list.**
TEST_RECORDS = [
    ("01_editfst_desire", 1, "A", "B", "FOR_TEST_01", 2, None, 0, None, None, None, None),
    ("01_editfst_desire", 1, "A", "B", "FOR_TEST_01", 3, None, 1, None, None, None, None),
    ("01_editfst_desire", 1, "A", "B", "FOR_TEST_01", 4, None, 0, None, None, None, None),

    ("02_editfst_exclure", 2, "AA", "B", "FOR_TEST_02", 1, None, 1, None, None, None, None),
    ("02_editfst_exclure", 2, "TT", "B", "FOR_TEST_02", 1, None, 0, None, None, None, None),
    ("02_editfst_exclure", 2, "BB", "B", "FOR_TEST_02", 1, None, 1, None, None, None, None),
    ("02_editfst_exclure", 2, "HU", "B", "FOR_TEST_02", 1, None, 0, None, None, None, None),
    ("02_editfst_exclure", 2, "CC", "B", "FOR_TEST_02", 1, None, 1, None, None, None, None),

    # Records for the `voir` test. Only the `_in` file is used (voir just
    # prints the table of contents), so in_output is 0 for all of them. The
    # metadata is chosen to be distinct and easy to grep for in voir's output.
    # dateo is set to 2026-09-01 12:30:00 so the DATE-O column shows a
    # readable date and a nonzero time instead of zeros.
    ("03_voir_basic", 3, "VOI", "R", "VOIR_ETI_1", 11, "2026-09-01 12:30:00", 0, None, None, None, None),
    ("03_voir_basic", 3, "VOI", "R", "VOIR_ETI_2", 22, "2026-09-01 12:30:00", 0, None, None, None, None),
    ("03_voir_basic", 3, "VOI", "R", "VOIR_ETI_3", 33, "2026-09-01 12:30:00", 0, None, None, None, None),

    # Records for the `editfst -ds` (sequential destination) regression test.
    # All records are included in the expected output so that a full copy of
    # the input is the expected result.
    ("04_editfst_ds", 4, "DS", "B", "DS_ETI_1", 1, None, 1, None, None, None, None),
    ("04_editfst_ds", 4, "DS", "B", "DS_ETI_2", 2, None, 1, None, None, None, None),
    ("04_editfst_ds", 4, "DS", "B", "DS_ETI_3", 3, None, 1, None, None, None, None),

    # Records for the `fststat` test: one field of each FST data type that
    # fststat processes without an error. Only the `_in` file is used (fststat
    # just reads and prints stats), so in_output is 0 for all of them. The
    # nomvar is a distinctive 4-char token the test script greps for. Every
    # record must produce a stats line with the known mean/min/max (the
    # 64-bit REAL_IEEE only logs a warning). The 64-bit SIGNED field, which
    # triggers an RMN error, is in the separate `06_fststat_special` case.
    ("05_fststat_types", 5, "R32F", "B", "FSTSTAT_REAL32", 1, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTSTAT_DATA_F4),
    ("05_fststat_types", 5, "E32F", "B", "FSTSTAT_IEEE32", 2, None, 0, rmn.FstDataType.FST_TYPE_REAL_IEEE, 32, 32, FSTSTAT_DATA_F4),
    ("05_fststat_types", 5, "I32S", "B", "FSTSTAT_SIGNED32", 3, None, 0, rmn.FstDataType.FST_TYPE_SIGNED, 32, 32, FSTSTAT_DATA_I4),
    ("05_fststat_types", 5, "I32U", "B", "FSTSTAT_UNSIG32", 4, None, 0, rmn.FstDataType.FST_TYPE_UNSIGNED, 32, 32, FSTSTAT_DATA_U4),
    ("05_fststat_types", 5, "R24Q", "B", "FSTSTAT_OLDQ24", 5, None, 0, rmn.FstDataType.FST_TYPE_REAL_OLD_QUANT, 32, 24, FSTSTAT_DATA_F4),
    ("05_fststat_types", 5, "R16F", "B", "FSTSTAT_REAL16", 6, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 16, FSTSTAT_DATA_F4),
    ("05_fststat_types", 5, "E64F", "B", "FSTSTAT_IEEE64", 7, None, 0, rmn.FstDataType.FST_TYPE_REAL_IEEE, 64, 64, FSTSTAT_DATA_F8),

    # Records for the `fststat` "special" test: fields that fststat
    # mishandles. The order matters: the NaN field must be last, because it
    # crashes fststat and the process dies before processing any later
    # records. The test documents the current behavior rather than
    # specifying it.
    ("06_fststat_special", 6, "I64S", "B", "FSTSTAT_SIGNED64", 8, None, 0, rmn.FstDataType.FST_TYPE_SIGNED, 64, 64, FSTSTAT_DATA_I8),
    ("06_fststat_special", 6, "PINF", "B", "FSTSTAT_PINF", 9, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTSTAT_DATA_PINF),
    ("06_fststat_special", 6, "NINF", "B", "FSTSTAT_NINF", 10, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTSTAT_DATA_NINF),
    ("06_fststat_special", 6, "NANF", "B", "FSTSTAT_NAN", 11, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTSTAT_DATA_NAN),
    ("06_fststat_special", 6, "AINF", "B", "FSTSTAT_ALLINF", 12, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTSTAT_DATA_ALLINF),
    ("06_fststat_special", 6, "ANAN", "B", "FSTSTAT_ALLNAN", 13, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTSTAT_DATA_ALLNAN),
    ("06_fststat_special", 6, "MIXN", "B", "FSTSTAT_MIXNF", 14, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTSTAT_DATA_MIXNF),

    # Records for the `fstcomp` non-finite test. Two test cases (A and B) hold
    # the SAME seven records (identical nomvar/etiket/ip1/metadata); only the
    # data of some records differs between the two files. The test compares
    # 08_fstcomp_nonfinite_a_in.rsf against 08_fstcomp_nonfinite_b_in.rsf and
    # checks fstcomp's per-record "differs" indicator. Only the `_in` files are
    # used, so in_output is 0 for all of them.
    #
    #   SAME  identical data          -> control: reported as SAME
    #   FINC  4 vs 9 (finite)         -> control: reported as DIFFERENT
    #   NANF  4 vs NaN                -> should be DIFFERENT
    #   INFF  4 vs +Inf               -> should be DIFFERENT
    #   NINF  4 vs -Inf               -> should be DIFFERENT
    #   NANV  NaN vs +Inf             -> should be DIFFERENT
    #   INNV  +Inf vs -Inf            -> should be DIFFERENT
    ("08_fstcomp_nonfinite_a", 7, "SAME", "B", "FSTCMP_SAME", 1, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTCOMP_BASE),
    ("08_fstcomp_nonfinite_a", 7, "FINC", "B", "FSTCMP_FIN", 2, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTCOMP_BASE),
    ("08_fstcomp_nonfinite_a", 7, "NANF", "B", "FSTCMP_NANF", 3, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTCOMP_BASE),
    ("08_fstcomp_nonfinite_a", 7, "INFF", "B", "FSTCMP_INFF", 4, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTCOMP_BASE),
    ("08_fstcomp_nonfinite_a", 7, "NINF", "B", "FSTCMP_NINF", 5, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTCOMP_BASE),
    ("08_fstcomp_nonfinite_a", 7, "NANV", "B", "FSTCMP_NANV", 6, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTCOMP_NAN),
    ("08_fstcomp_nonfinite_a", 7, "INNV", "B", "FSTCMP_INNV", 7, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTCOMP_PINF),

    ("08_fstcomp_nonfinite_b", 8, "SAME", "B", "FSTCMP_SAME", 1, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTCOMP_BASE),
    ("08_fstcomp_nonfinite_b", 8, "FINC", "B", "FSTCMP_FIN", 2, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTCOMP_FIN_DIFF),
    ("08_fstcomp_nonfinite_b", 8, "NANF", "B", "FSTCMP_NANF", 3, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTCOMP_NAN),
    ("08_fstcomp_nonfinite_b", 8, "INFF", "B", "FSTCMP_INFF", 4, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTCOMP_PINF),
    ("08_fstcomp_nonfinite_b", 8, "NINF", "B", "FSTCMP_NINF", 5, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTCOMP_NINF),
    ("08_fstcomp_nonfinite_b", 8, "NANV", "B", "FSTCMP_NANV", 6, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTCOMP_PINF),
    ("08_fstcomp_nonfinite_b", 8, "INNV", "B", "FSTCMP_INNV", 7, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTCOMP_NINF),

    # Records for the `fstcomp` exit-code test. Two test cases (A and B) hold
    # the SAME three records (identical nomvar/etiket/ip1/metadata); only the
    # data of REC3 differs (base in A, diff in B). The records are written in a
    # DIFFERENT order in the two files: in A, REC3 is last; in B, REC3 is first.
    # The test runs fstcomp twice (inverting -a/-b) and checks the exit code.
    # Only the `_in` files are used, so in_output is 0 for all of them.
    ("09_fstcomp_ecode_a", 9, "REC1", "B", "FSTCMP_REC1", 1, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTCOMP_BASE),
    ("09_fstcomp_ecode_a", 9, "REC2", "B", "FSTCMP_REC2", 2, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTCOMP_BASE),
    ("09_fstcomp_ecode_a", 9, "REC3", "B", "FSTCMP_REC3", 3, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTCOMP_BASE),

    ("09_fstcomp_ecode_b", 10, "REC3", "B", "FSTCMP_REC3", 3, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTCOMP_FIN_DIFF),
    ("09_fstcomp_ecode_b", 10, "REC1", "B", "FSTCMP_REC1", 1, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTCOMP_BASE),
    ("09_fstcomp_ecode_b", 10, "REC2", "B", "FSTCMP_REC2", 2, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTCOMP_BASE),

    # Records for the `fstcomp` RSF-vs-XDF comparison of the non-finite records
    # (03_fststat_special_comp.sh). Same 6 non-finite REAL records as
    # 06_fststat_special, but WITHOUT the 64-bit SIGNED record (which XDF cannot
    # read back, so it would make the two files differ). Only the `_in` files
    # are used, so in_output is 0 for all of them.
    ("07_fststat_special_nf", 11, "PINF", "B", "FSTSTAT_PINF", 9, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTSTAT_DATA_PINF),
    ("07_fststat_special_nf", 11, "NINF", "B", "FSTSTAT_NINF", 10, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTSTAT_DATA_NINF),
    ("07_fststat_special_nf", 11, "NANF", "B", "FSTSTAT_NAN", 11, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTSTAT_DATA_NAN),
    ("07_fststat_special_nf", 11, "AINF", "B", "FSTSTAT_ALLINF", 12, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTSTAT_DATA_ALLINF),
    ("07_fststat_special_nf", 11, "ANAN", "B", "FSTSTAT_ALLNAN", 13, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTSTAT_DATA_ALLNAN),
    ("07_fststat_special_nf", 11, "MIXN", "B", "FSTSTAT_MIXNF", 14, None, 0, rmn.FstDataType.FST_TYPE_REAL, 32, 32, FSTSTAT_DATA_MIXNF),
]


def make_record(nomvar, typvar, etiket, ip1, dateo,
                data_type=None, data_bits=None, pack_bits=None, data=None):
    """Build an ``fst_record``

    Starts from ``default_fst_record`` and overrides most of the fields with either
    the given inputs or hardcoded values. ``grtyp`` and ``datev``
    are deliberately left at their default values.

    ``data_type``, ``data_bits``, ``pack_bits`` and ``data`` default to the
    1x1x1 float32 dummy field used by the editfst/voir tests. The fststat test
    passes explicit values to create one field per data type.
    """
    # dateo is a "YYYY-MM-DD HH:MM:SS" string in the spec (or None for no date).
    if data is None:
        data = DUMMY
    if data_type is None:
        data_type = rmn.FstDataType.FST_TYPE_REAL
    if data_bits is None:
        data_bits = 32
    if pack_bits is None:
        pack_bits = 32
    ni = data.shape[0]
    nj = data.shape[1] if data.ndim > 1 else 1
    nk = data.shape[2] if data.ndim > 2 else 1
    rec = rmn.fst_record(
        data_type=data_type,
        data_bits=data_bits,
        pack_bits=pack_bits,
        ni=ni, nj=nj, nk=nk,
        deet=0, npas=0,
        dateo=rmn.encode_date(dateo) if dateo is not None else 0,
        ip1=ip1, ip2=1, ip3=1,
        ig1=1, ig2=1, ig3=1, ig4=1,
        nomvar=nomvar, typvar=typvar, etiket=etiket,
        data=data,
    )
    return rec


def gen_files(is_rsf):
    """Generate the ``_in``/``_out`` files for one backend (RSF or XDF)."""
    ext = "rsf" if is_rsf else "xdf"
    options = "RSF+R/W" if is_rsf else "XDF+R/W"

    file_in = None
    file_out = None
    previous_test_id = -1

    for (test_name, test_id, nomvar, typvar, etiket, ip1, dateo, in_output,
         data_type, data_bits, pack_bits, data) in TEST_RECORDS:
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

        rec = make_record(nomvar, typvar, etiket, ip1, dateo,
                          data_type, data_bits, pack_bits, data)
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
