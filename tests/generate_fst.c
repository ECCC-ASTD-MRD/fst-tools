
#include <string.h>

#include <App.h>
#include <rmn.h>

const float dummy[1] = {1.0};


typedef struct {
    const char* test_name; //!< Base name of the test file (both input and expected output)
    int test_id;    //!< ID of the test (arbitrary, but must be the same for records to go in the same file)

    const char* nomvar;
    const char* typvar;
    const char* etiket;
    const void* data;
    int ip1;

    int in_output; //!< Whether to include the record in the expected output file
} test_record;

// **Records for the same test case must be contiguous in this array**
const test_record test_records[] = {
    {"01_editfst_desire", 1, "A", "B", "FOR_TEST_01", dummy, 2, 0},
    {"01_editfst_desire", 1, "A", "B", "FOR_TEST_01", dummy, 3, 1},
    {"01_editfst_desire", 1, "A", "B", "FOR_TEST_01", dummy, 4, 0},

    {"02_editfst_exclure", 2, "AA", "B", "FOR_TEST_02", dummy, 1, 1},
    {"02_editfst_exclure", 2, "TT", "B", "FOR_TEST_02", dummy, 1, 0},
    {"02_editfst_exclure", 2, "BB", "B", "FOR_TEST_02", dummy, 1, 1},
    {"02_editfst_exclure", 2, "HU", "B", "FOR_TEST_02", dummy, 1, 0},
    {"02_editfst_exclure", 2, "CC", "B", "FOR_TEST_02", dummy, 1, 1},
};

static inline fst_record get_basic_record(void) {
    fst_record rec = default_fst_record;

    rec.data = (void*)dummy;
    rec.data_type = FST_TYPE_REAL;
    rec.data_bits = 32;
    rec.pack_bits = 32;
    rec.ni = 1;
    rec.nj = 1;
    rec.nk = 1;

    rec.deet = 0;
    rec.npas = 0;
    rec.dateo = 0;

    rec.ip1 = 1;
    rec.ip2 = 1;
    rec.ip3 = 1;
    rec.ig1 = 1;
    rec.ig2 = 1;
    rec.ig3 = 1;
    rec.ig4 = 1;

    return rec;
}

static fst_record make_record(const test_record rec_in) {
    fst_record rec = get_basic_record();
    rec.data = (void*)rec_in.data;
    rec.ip1 = rec_in.ip1;
    strncpy(rec.nomvar, rec_in.nomvar, 4);
    strncpy(rec.typvar, rec_in.typvar, 2);
    strncpy(rec.etiket, rec_in.etiket, 12);

    return rec;
}

//! Generate a set of input files and the expected output of their respective test, based on the test records
//! in the `test_records` array.
//! We generate an RSF + XDF version of each file.
static int gen_files(const int is_rsf) {
    const int num_records = sizeof(test_records) / sizeof(test_record);
    fst_file* file_in = NULL;
    fst_file* file_out = NULL;
    int previous_test_id = -1;
    fst_record rec = default_fst_record;
    for (int i = 0; i < num_records; i++) {
        const test_record* test_rec = test_records + i;
        if (test_rec->test_id < previous_test_id) {
            App_Log(APP_ERROR, "%s: Test IDs must monotously increase\n", __func__);
            return -1;
        }
        else if (test_rec->test_id > previous_test_id) {
            // Close previous test file
            if (file_in != NULL) {
                if (fst24_close(file_in) != TRUE || fst24_close(file_out) != TRUE) {
                    App_Log(APP_ERROR, "%s: Unable to close test case file (rec %d)\n", __func__, i);
                    return -1;
                }
            }

            // Open next test file
            char filename_in[1024];
            char filename_out[1024];
            const char* options = is_rsf ? "RSF+R/W" : "XDF+R/W";
            sprintf(filename_in, "%s_in.%s", test_rec->test_name, is_rsf ? "rsf": "xdf");
            sprintf(filename_out, "%s_out.%s", test_rec->test_name, is_rsf ? "rsf": "xdf");
            remove(filename_in);
            remove(filename_out);

            file_in = fst24_open(filename_in, options);
            file_out = fst24_open(filename_out, options);

            if (file_in == NULL || file_out == NULL) {
                App_Log(APP_ERROR, "%s: Unable to create test file(s) %s/%s\n",
                        __func__, filename_in, filename_out);
                return -1;
            }
        }

        // Put record in file
        rec = make_record(*test_rec);
        if (fst24_write(file_in, &rec, FST_NO) != TRUE) {
            App_Log(APP_ERROR, "%s: Unable to write record\n", __func__);
            return -1;
        }
        if (test_rec->in_output && fst24_write(file_out, &rec, FST_NO) != TRUE) {
            App_Log(APP_ERROR, "%s: Unable to write record\n", __func__);
            return -1;
        }

        previous_test_id = test_rec->test_id;
    }

    // Free + close everything
    fst24_record_free(&rec);
    if (fst24_close(file_in) != TRUE || fst24_close(file_out) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to close test case file\n", __func__);
        return -1;
    }

    return 0;
}


int main(void) {
    
    if (gen_files(1) != 0) return -1; // RSF
    if (gen_files(0) != 0) return -1; // XDF

    return 0;
}
