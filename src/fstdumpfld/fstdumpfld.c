#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

#include <App.h>
#include <rmn.h>
#include <rmn/convert_ip.h>
#include "fst-tools_build_info.h"

// KIND =0, p est en hauteur (m) par rapport au niveau de la mer (-20,000 -> 100,000)
// KIND =1, p est en sigma                                       (0.0 -> 1.0)
// KIND =2, p est en pression (mb)                               (0 -> 1100)
// KIND =3, p est un code arbitraire                             (-4.8e8 -> 1.0e10)
// KIND =4, p est en hauteur (M) par rapport au niveau du sol    (-20,000 -> 100,000)
// KIND =5, p est en coordonnee hybride                          (0.0 -> 1.0)
// KIND =6, p est en coordonnee theta                            (1 -> 200,000)
// KIND =7, p est en metres (profondeur sous l'eau)              (0 -> 20,000)
// KIND =10, p represente le temps en heure                      (0.0 -> 1.0e10)
// KIND =15, reserve (entiers)
// KIND =17, p represente l'indice x de la matrice de conversion (1.0 -> 1.0e10)
//           (partage avec kind=1 a cause du range exclusif
// KIND =21, p est en metres-pression                            (0 -> 1,000,000) fact=1e4
//           (partage avec kind=5 a cause du range exclusif)
// KIND =23, reserve pour usage futur (partage avec kind=7)

typedef struct {
    int kind;
    char str[16];
    float min;
    float max;
} lvlType;


//! Decode an IP1 string to the corresponding integer value
int decodeIp1(const char * const lvlStr) {
#define NBLVLTYPES 13
    static const lvlType lvlTypes[NBLVLTYPES] = {
        {0, "m", -20000, 100000},
        {1, "sg", 0, 1},
        {2, "mb", 0, 1100},
        {3, "", -4.8e8, 1.0e10},
        {4, "M", -20000, 100000},
        {5, "hy", 0, 1},
        {6, "th", 1, 200000},
        {7, "m-", 0, 20000},
        {10, "H", 0, 1.0e10},
        {15, "_0", 0, 0},
        {17, "[]", 1, 1.0e10},
        {21, "mp", 1, 1000000},
        {23, "_1", 0, 0}
    };

    const char * space = strstr(lvlStr, " ");
    if (space == NULL) {
        return atoi(lvlStr);
    }

    int i = 0;
    while (i < NBLVLTYPES && strcmp(space + 1, lvlTypes[i].str) != 0) {
        i++;
    }
    if (strcmp(space + 1, lvlTypes[i].str) == 0) {
        int fpStrLen = space - lvlStr;
        char * fpStr = malloc(fpStrLen + 1);
        if (!fpStr) {
           App_Log(APP_ERROR,"Could not allocate memory\n");
           App_End(-1);
           exit(1);
        }
        fpStr[fpStrLen] = '\0';
        strncpy(fpStr, lvlStr, fpStrLen);
        float fip1 = atof(fpStr);
        free(fpStr);

        int ip1;
        int kind = lvlTypes[i].kind;
        ConvertIp(&ip1, &fip1, &kind, 1);
        return ip1;
    } else {
        App_Log(APP_ERROR,"Unknown level type \"%s\"\n",(space+1));
        App_End(-1);
        exit(1);
    }
}


void printUsage(const char ** const argv) {
    printf("Dump a field as binary blob\n\n");
    printf("Usage:\n");
    printf("\t%s <fst file path> <dump file path> <nomvar> <etiket> <ip1> <ip2> <ip3>\n", argv[0]);
}


int main(int argc, const char ** const argv) {

    if (argc != 8) {
        printUsage(argv);
        return 1;
    }
    const char * const filePath = argv[1];
    const char * const dumpPath = argv[2];
    const char * const nomvar = argv[3];
    const char * const etiket = argv[4];
    const int ip1 = decodeIp1(argv[5]);
    const int ip2 = atoi(argv[6]);
    const int ip3 = atoi(argv[7]);

    App_Init(APP_MASTER,"fstdumpfld",VERSION,"",BUILD_TIMESTAMP);
    App_Start();

    // Link file to a unit number (Fortran compatible I/O handle)
    int iun = 0;
    int resultCode = c_fnom(&iun, filePath, "STD+OLD+R/O", 0);
    if (resultCode < 0) {
        App_Log(APP_ERROR,"Failed to link file %s !\n", filePath);
        App_End(-1);
        return 1;
    }

    // Open the file
    resultCode = c_fstouv(iun, "RND");
    if (resultCode < 0) {
        App_Log(APP_ERROR,"Failed to open the file %s !\n", filePath);
        App_End(-1);
        return 1;
    }

    // Dimension along X
    int ni;
    // Dimension along Y
    int nj;
    // Dimension along Z
    int nk;

    int handle = c_fstinf (iun, &ni, &nj, &nk, -1, etiket, ip1, ip2, ip3, "", nomvar);
    App_Log(APP_INFO,"Field handle = %d\n", handle);

    const int size = ni * nj * nk * sizeof(int32_t);
    App_Log(APP_INFO,"ni = %d, nj = %d, nk = %d, size = %d bytes\n", ni, nj, nk, size);

    // Allocate memory to hold the field's data
    int32_t *field = calloc(size, sizeof(char));

    resultCode = c_fstluk(field, handle, &ni, &nj, &nk);
    if (resultCode < 0) {
        App_Log(APP_ERROR,"Failed to read the field!\n");
        App_End(-1);
        return 1;
    }

    const int fd = open(dumpPath, O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
    const int sizeWritten = write(fd, field, size);
    if (sizeWritten != size) {
        App_Log(APP_ERROR,"Did not write expected number of bytes (expected = %d, written = %d)!\n", size, sizeWritten);
        App_End(-1);
        return 1;
    }
    close(fd);

    free(field);

    // Close the file
    c_fstfrm(iun);

    App_End(0);
    return 0;
}
