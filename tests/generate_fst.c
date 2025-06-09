
#include <string.h>

#include <App.h>
#include <rmn.h>

const float dummy[1] = {1.0};

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

int gen_file(const char* base_filename, const int is_rsf) {
    
    char filename[256];
    sprintf(filename, "%s.%s", base_filename, is_rsf ? "rsf" : "xdf");
    const char* options = is_rsf ? "RSF+R/W" : "XDF+R/W";

    remove(filename);

    fst_file* test_file = fst24_open(filename, options);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: Unable to create file %s\n", __func__, filename);
        return -1;
    }

    fst_record rec = get_basic_record();

    sprintf(rec.nomvar, "A");
    sprintf(rec.etiket, "FOR_TEST_01");
    for (int i = 0; i < 3; i++) {
        rec.ip1++;
        if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
            App_Log(APP_ERROR, "%s: Unable to write record\n", __func__);
            return -1;
        }
    }

    rec = get_basic_record();
    sprintf(rec.etiket, "FOR_TEST_02");

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to close test file %s\n", __func__, filename);
        return -1;
    }

    return 0;
}

int main(int argc, char** argv) {
    
    const char* base_filename = "test";
    if (argc > 1) base_filename = argv[1];

    if (gen_file(base_filename, 1) != 0) return -1; // RSF
    if (gen_file(base_filename, 0) != 0) return -1; // XDF

    return 0;
}
