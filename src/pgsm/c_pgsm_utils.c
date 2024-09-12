#include <stdio.h>
#include <stdlib.h>
#include <rmn/rpnmacros.h>
#include <ctype.h>
#include <string.h>

#define NOMVAR 1
#define TYPVAR 2
#define ETIKET 3
#define IP01    4
#define IP02    5
#define IP03    6
#define DATEO  7
#define DATEV  8
#define NI     9
#define NJ    10
#define NK    11
#define LAT   12
#define LON   13

#define NORD   1
#define SUD    2
#define EST    3
#define OUEST  4
#define NONE   5


FILE *ftnUnits[100];


void newdate_(int*, int*, int*, int*);

static int c_dateform = 1;


void strconvdate(char strdate[], int fstdate) {
    int lfstdate, yyyymmdd, hhmmssss, mode;
    int yyyy, month, day, hour, minutes, sec;

    switch(c_dateform) {
        case 0:
            sprintf(strdate, "%08d", fstdate);
            break;

        case 1:
            mode = -3;
            lfstdate = fstdate;

            f77name(newdate)(&lfstdate, &yyyymmdd, &hhmmssss, &mode);
            sprintf(strdate, "%08d.%08d", yyyymmdd, hhmmssss);
            break;

        case 2:
            mode = -3;
            lfstdate = fstdate;

            f77name(newdate)(&lfstdate, &yyyymmdd, &hhmmssss, &mode);

            yyyy = yyyymmdd / 10000;
            month = (yyyymmdd / 100) % 100;
            day   = yyyymmdd % 100;

            hour = hhmmssss / 1000000;
            minutes = (hhmmssss / 10000) % 100;
            sec = (hhmmssss % 100);

            sprintf(strdate, "%04d-%02d-%02dT%02d:%02d:%02dZ", yyyy, month, day, hour, minutes, sec);
            break;
    }
}


int GetIdent(
    char string[],
    int item,
    const char * const nomvar,
    const char * const typvar,
    const char * const etiket,
    int ip1,
    int ip2,
    int ip3,
    int dateo,
    int datev,
    int ni,
    int nj,
    int nk
) {
    switch(item) {
        case NOMVAR:
            strcpy(string, nomvar);
            break;

        case TYPVAR:
            strcpy(string, typvar);
            break;

        case ETIKET:
            strcpy(string, etiket);
            break;

        case IP01:
            sprintf(string, "%5d", ip1);
            break;

        case IP02:
            sprintf(string, "%4d", ip2);
            break;

        case IP03:
            sprintf(string, "%4d", ip3);
            break;

        case DATEO:
            strconvdate(string, dateo);
            break;

        case DATEV:
            strconvdate(string, datev);
            break;

        case NI:
            sprintf(string, "%03d", ni);
            break;

        case NJ:
            sprintf(string, "%03d", nj);
            break;

        case NK:
            sprintf(string, "%03d", nk);
            break;

        default:
            sprintf(string, "%s", "KABOOM!");
            break;
    }
    return 0;
}


void ImprimeIdent(
    char longString[],
    const int items[],
    const char * const separateur,
    const char * const nomvar,
    const char * const typvar,
    const char * const etiket,
    int ip1,
    int ip2,
    int ip3,
    int dateo,
    int datev,
    int ni,
    int nj,
    int nk
) {
    char string[32];
    strcpy(longString, "");

    int i = 0;
    while (items[i] != 0 && i < 16) {
        if (items[i] != LAT || items[i] != LON) {
            GetIdent(string, items[i], nomvar, typvar, etiket, ip1, ip2, ip3, dateo, datev, ni, nj, nk);
            strcat(string, separateur);
            strcat(longString, string);
        }
        i++;
    }
}


void pgsmform(char format[], int * const nrepeats, int lenFormat){
    char cFormat[32];
    char cNrepeats[8];

    *nrepeats = 0;

    for (int i = 0; i < 32; i++) {
        cFormat[i] = '\0';
    }

    int i = lenFormat - 1;
    while (i >= 0 && format[i] == ' ') {
        format[i] = '\0';
        i--;
    }

    i = 0;
    while (isdigit((char)format[i])) {
        cNrepeats[i] = format[i];
        i++;
    }
    cNrepeats[i] = '\0';

    if (i > 0) {
        sscanf(cNrepeats, "%d", nrepeats);
        strcpy(format, &(format[i]));
    }

    cFormat[0] = '%';
    strcpy(&cFormat[1], &format[1]);

    cFormat[strlen(cFormat)] = (char) tolower(format[0]);
    strcpy(format, cFormat);
}

//! Return a format string for integer with the same with as the provided one
//! \note The user of this function must free the returned string after use
//! \return ointer to new format string or NULL on error
const char * convertFmt4Int(char * format) {
    if (format[0] != '%') return NULL;

    size_t len = strlen(format);
    // Not what we expected
    if (format[len - 1] != 'f') return NULL;

    char * const trimStr = malloc(len - 1);
    strncpy(trimStr, &format[1], len - 2);

    int sum = 0;
    char * saveptr;
    char * token = strtok_r(trimStr, ".", &saveptr);
    while(token) {
        if (token == NULL) break;
        int val;
        if (sscanf(token, "%d", &val) == EOF) return NULL;
        sum += val;
        token = strtok_r(NULL, ".", &saveptr);
    }
    char * const newFmt = malloc(sum / 10 + 4);
    snprintf(newFmt, sum / 10 + 4, "%%%dd", sum);

    return newFmt;
}


void f77name(pgsmwr)(
    const int * const iun,
    //! [in] 0 = int32_t, float otherwise
    const int * const dataType,
    const void * const data,
    const int * const ni,
    const int * const nj,
    const int * const nk,
    const char * const format,
    const int * const position,
    const int * const idents,
    const char * const separateur,
    const char * const nomvar,
    const char * const typvar,
    const char * const etiket,
    const int * const dateo,
    const int * const datev,
    const int * const dateform,
    const int * const ip1,
    const int * const ip2,
    const int * const ip3,
    const float * const lat,
    const float * const lon,
    F2Cl len_format,
    F2Cl len_separateur,
    F2Cl len_nomvar,
    F2Cl len_typvar,
    F2Cl len_etiket
) {
    // WARNING: File global var!
    c_dateform = *dateform;

    char c_nomvar[8];
    strncpy(c_nomvar, nomvar, 4);
    char c_typvar[4];
    strncpy(c_typvar, typvar, 2);
    char c_etiket[16];
    strncpy(c_etiket, etiket, 12);
    char internalFormat[17];
    strncpy(internalFormat, format, 16);
#ifndef NDEBUG
    printf("format = \"%s\"\n", format);
#endif
    char c_separateur[2];
    c_separateur[0] = separateur[0];

    if (c_separateur[0] == 'T') {
        c_separateur[0] = '\t';
    }

    c_nomvar[4] = '\0';
    c_typvar[2] = '\0';
    c_etiket[12] = '\0';
    c_separateur[1] = '\0';

    int nrepeats;
    int longform = 16;
    int latlonflag = 0;
    pgsmform(internalFormat, &nrepeats, longform);
#ifndef NDEBUG
    printf("internalFormat = \"%s\"\n", internalFormat);
#endif
    char latlonformat[37];
    sprintf(latlonformat, "%s%s%s%s", "%s", internalFormat, "%s", internalFormat);
    for (int i = 0; i < 16; i++) {
        if (idents[i] == LAT || idents[i] == LON) {
            latlonflag = 1;
        }
    }

    char string[256];
    int npts = *ni * *nj * *nk;
    for (int j = 0; j < npts; j++) {
        if (*position == NORD) {
            if (j == 0) {
                ImprimeIdent(string, idents, c_separateur, c_nomvar, c_typvar, c_etiket, *ip1, *ip2, *ip3, *dateo, *datev, *ni, *nj, *nk);
                string[strlen(string) - 1] = '\n';
                fprintf(ftnUnits[*iun], "%s", string);
            }
        }

        if (*position == OUEST) {
            if (latlonflag) {
                ImprimeIdent(string, idents, c_separateur, c_nomvar, c_typvar, c_etiket, *ip1, *ip2, *ip3, *dateo, *datev, *ni, *nj, *nk);
                fprintf(ftnUnits[*iun], "%s%s", string, c_separateur);
            } else {
                if (j == 0) {
                    ImprimeIdent(string, idents, c_separateur, c_nomvar, c_typvar, c_etiket, *ip1, *ip2, *ip3, *dateo, *datev, *ni, *nj, *nk);
                    fprintf(ftnUnits[*iun], "%s%s", string, c_separateur);
                }
            }
        }

        if (*dataType) {
            const float * const fdata = (float *) data;
            fprintf(ftnUnits[*iun], internalFormat, fdata[j]);
        } else {
            const int32_t * const idata = (int32_t *) data;
            char * const intFmt = convertFmt4Int(internalFormat);
#ifndef NDEBUG
            printf("intFMt = \"%s\"\n", intFmt);
#endif
            if (! intFmt) {
                fprintf(stderr, "Error while converting the format string for integer!\n");
                exit(-1);
            }
            fprintf(ftnUnits[*iun], intFmt, idata[j]);
            free(intFmt);
        }

        if (latlonflag) {
            fprintf(ftnUnits[*iun], latlonformat, c_separateur, lat[j],  c_separateur, lon[j]);
        } else {
            if (j < npts-1) {
                if (nrepeats == 0) {
                    fprintf(ftnUnits[*iun], "%s", c_separateur);
                } else {
                    if (0 == ((j + 1) % nrepeats)) {
                        fprintf(ftnUnits[*iun], "\n");
                    } else {
                        fprintf(ftnUnits[*iun], "%s", c_separateur);
                    }
                }
            }
        }

        if (*position == NORD || *position == OUEST || *position == 0) {
            if (latlonflag || j == npts - 1) {
                fprintf(ftnUnits[*iun], "\n");
            }
        }

        if (*position == SUD) {
            if (latlonflag) {
                if (j < npts - 1) {
                    fprintf(ftnUnits[*iun], "\n");
                }
            }

            if (j == npts - 1) {
                ImprimeIdent(string, idents, c_separateur, c_nomvar, c_typvar, c_etiket, *ip1, *ip2, *ip3, *dateo, *datev, *ni, *nj, *nk);
                string[strlen(string) - 1] = '\n';
                fprintf(ftnUnits[*iun],"\n%s",string);
            }
        }

        if (*position == EST) {
            if (latlonflag || j == npts - 1) {
                ImprimeIdent(string, idents, c_separateur, c_nomvar, c_typvar, c_etiket, *ip1, *ip2, *ip3, *dateo, *datev, *ni, *nj, *nk);
                fprintf(ftnUnits[*iun], "%s%s\n", c_separateur, string);
            }
        }
    }
    printf(" ECRIT -- %s \t %s \t %05d \t %04d \t %03d \t %s \t %08d\n", c_nomvar, c_typvar, *ip1, *ip2, *ip3, c_etiket, *dateo);
}


void f77name(pgsmecho)(int *iun, char message[], int *lenMessage, F2Cl fortranLenMessage) {
    message[*lenMessage] = '\0';
    fprintf(ftnUnits[*iun], "%s\n", message);
}


int f77name(pgsmof)(int *iun, char *nomFichier, F2Cl lenNomFichier) {
    int i = 0;
    while (nomFichier[i] != ' ') {
        i++;
    }
    nomFichier[i] = '\0';

    ftnUnits[*iun] = fopen(nomFichier, "w");
    if (ftnUnits[*iun] == NULL) {
        fprintf(stderr, "Impossible d'ouvrir le fichier: %s\n", nomFichier);
        return -1;
    }

   return 0;
}


void f77name(pgsmcf)(int *iun) {
    fclose(ftnUnits[*iun]);
}
