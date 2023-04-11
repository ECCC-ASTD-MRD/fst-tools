#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <App.h>
#include <rmn.h>
#include "fst-tools_build_info.h"

/*
fst2xml : Convertisseur de fichier standard RPN version 2000 a XML

Auteur: Djamel Bouhemhem, Yves Chartier
Statut: Test
Version: 004
Version: 005, reload avec librmn_014
Version: 006, reload avec librmn_015.1
Version: 007, reload avec librmn_015.2
*/

void xmlconvip(char xmlip1[], int ip1);
void xmlconvdate(char xmldate[], int dateo);
void nettoyer(char chaine[]);

int fst2xml(int argc, char **argv) {
    char fstFile[256];
    char xmlFile[256];
    char encoding[16];
    char format[32];
    int ok, iun, key, tokens_per_line;

    char *liste[5], lcl_liste[5][256], *def[5], lcl_def[5][16], val[5][256];
    int n, npos, nptsRLE;
    int ni, nj, nk;

    int ier, ip1, ip2, ip3, ig1, ig2, ig3, ig4;
    int  dateo, datev, datyp, deet,nbits, npak, npas,swa, lng;
    int dltf, ubc, extra1, extra2, extra3;
    int isallocated, multi;

    char etiket[16], nomvar[8], typvar[4], grtyp[2];
    char xmldateo[32], xmldatev[32];
    char xmlip1[32], xmlip2[32], xmlip3[32];
    double nhours;

    float *fld;
    int *ifld;

    FILE *xmlfd;

    strcpy(lcl_liste[0], "fst.");
    strcpy(lcl_liste[1], "xml.");
    strcpy(lcl_liste[2], "encoding.");
    strcpy(lcl_liste[3], "format.");
    strcpy(lcl_liste[4], "tpl.");

    liste[0] = (char *) lcl_liste[0];
    liste[1] = (char *) lcl_liste[1];
    liste[2] = (char *) lcl_liste[2];
    liste[3] = (char *) lcl_liste[3];
    liste[4] = (char *) lcl_liste[4];

    strcpy(lcl_def[0],"bidon.fst");
    strcpy(lcl_def[1],"bidon.xml");
    strcpy(lcl_def[2],"ascii");
    strcpy(lcl_def[3],"%14.6g");
    strcpy(lcl_def[4],"8");

    def[0] = (char *) lcl_def[0];
    def[1] = (char *) lcl_def[1];
    def[2] = (char *) lcl_def[2];
    def[3] = (char *) lcl_def[3];
    def[4] = (char *) lcl_def[4];

    for (int i = 0; i < 5; i++) {
        strcpy(val[i], def[i]);
    }

    npos = 0;
    c_ccard(argv, argc, (char **) liste, val, (char **) def, 5, &npos);

    App_Init(APP_MASTER,"fst2xml",VERSION,"",BUILD_TIMESTAMP);
    App_Start();

    strcpy(fstFile, val[0]);
    strcpy(xmlFile, val[1]);
    strcpy(encoding, val[2]);
    strcpy(format,  val[3]);
    sscanf(val[4], "%d", &tokens_per_line);

    iun = 1;
    c_fnom(iun, fstFile, "RND+R/O", 0);
    ier = c_fstouv(iun, "RND");

    if (0 == strcmp(val[1], def[1])) {
        xmlfd = stdout;
    } else {
        xmlfd = fopen(xmlFile, "w");
    }

    if (xmlfd == NULL) {
        App_Log(APP_ERROR,"Cannot open output file... Exiting...");
        App_End(13);
        exit(13);
    }

    fprintf(xmlfd, "%s\n", "<?xml version='1.0' encoding='utf-8' standalone='yes'?>");
    fprintf(xmlfd, "%s\n", "<rpn-standard-file>");


    key = c_fstinf(iun, &ni, &nj, &nk, -1,"            ", -1, -1, -1, "  ", "    ");
    do {
        strcpy(etiket, "            ");
        strcpy(nomvar, "    ");
        strcpy(typvar, "  ");
        strcpy(grtyp, " ");

        ier = c_fstprm(key, &dateo, &deet, &npas, &ni, &nj, &nk, &nbits,
                &datyp, &ip1, &ip2, &ip3, typvar, nomvar, etiket,
                grtyp, &ig1, &ig2, &ig3, &ig4, &swa, &lng, &dltf,
                &ubc, &extra1, &extra2, &extra3);
        if (nbits > 32) {
            multi = 2;
        } else {
            multi = 1;
        }
        xmlconvip(xmlip1, ip1);
        nhours = (double) npas * deet / 3600.0;
        f77name(incdatr)(&datev, &dateo, &nhours);
        xmlconvdate(xmldateo, dateo);
        xmlconvdate(xmldatev, datev);

        etiket[12] = '\0';
        nomvar[4] = '\0';
        typvar[2] = '\0';

        /*---------------------------------------------------------------*/
        fprintf(xmlfd, "%s\n", "<fstrecord>");
        fprintf(xmlfd, "\t%s%s%s\n", "<nomvar>", nomvar, "</nomvar>");
        fprintf(xmlfd, "\t\t%s%s%s\n", "<typvar>", typvar, "</typvar>");
        fprintf(xmlfd, "\t\t%s%s%s\n", "<etiket>", etiket, "</etiket>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<ip1>", ip1, "</ip1>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<ip2>", ip2, "</ip2>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<ip3>", ip3, "</ip3>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<ni>", ni, "</ni>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<nj>", nj, "</nj>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<nk>", nk, "</nk>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<dateo>", dateo, "</dateo>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<datev>", datev, "</datev>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<deet>", deet, "</deet>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<npas>", npas, "</npas>");
        fprintf(xmlfd, "\t\t%s%s%s\n", "<grtyp>", grtyp, "</grtyp>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<ig1>", ig1, "</ig1>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<ig2>", ig2, "</ig2>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<ig3>", ig3, "</ig3>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<ig4>", ig4, "</ig4>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<nbits>", nbits, "</nbits>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<datyp>", datyp, "</datyp>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<swa>", swa, "</swa>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<lng>", lng, "</lng>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<dltf>", dltf, "</dltf>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<ubc>", ubc, "</ubc>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<extra1>", extra1, "</extra1>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<extra2>", extra2, "</extra2>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<extra3>", extra3, "</extra3>");

        fprintf(xmlfd, "\t\t%s%s%s\n", "<level>", xmlip1, "</level>");
        fprintf(xmlfd, "\t\t%s%s%s\n", "<date-of-origin>", xmldateo, "</date-of-origin>");
        fprintf(xmlfd, "\t\t%s%s%s\n", "<date-of-validity>", xmldatev, "</date-of-validity>");

        /*---------------------------------------------------------------*/


        fprintf(xmlfd, "\t\t%s\n", "<fstdata encoding='ASCII' format='14.6g' orientation='south-to-north'>");
        fprintf(xmlfd, "\t\t%s", "<values>");

        switch (datyp) {
            case 1:
            case 5:
            case 6:
            case 133:
            case 134:
                fld = (float *) malloc(ni * nj * nk * sizeof(float) * multi);
                isallocated = 1;
                ier = c_fstluk(fld, key, &ni, &nj, &nk);
                for (int i = 0; i < ni * nj * nk; i++) {
                    fprintf(xmlfd, format, fld[i]);
                    if (0 == ((i+1) % tokens_per_line)) {
                        fprintf(xmlfd, "\n");
                    }
                }
                break;

            case 2:
            case 4:
            case 130:
            case 132:
                ifld = (int *) malloc(ni * nj * nk * sizeof(int) * multi);
                isallocated = 2;
                ier = c_fstluk(ifld, key, &ni, &nj, &nk);
                for (int i = 0; i < ni * nj * nk; i++) {
                    fprintf(xmlfd, format, ifld[i]);
                    if (0 == ((i+1) % tokens_per_line)) {
                        fprintf(xmlfd, "\n");
                    }
                }
                break;

            default:
                App_Log(APP_WARNING,"Cannot process Datyp %d , skipping record nomvar=%s\n\n",datyp,nomvar);
                isallocated = 0;
        }
        fprintf(xmlfd, "\t\t%s\n", "</values>");
        fprintf(xmlfd, "\t\t%s\n", "</fstdata>");
        fprintf(xmlfd, "%s\n", "</fstrecord>");

        switch (isallocated) {
            case 0:
                break;
            case 1:
                free(fld);
                break;
            case 2:
                free(ifld);
                break;
        }

        key = c_fstsui(iun, &ni, &nj, &nk);
    } while (key >= 0);

    fprintf(xmlfd, "%s\n", "</rpn-standard-file>");
    fclose(xmlfd);
    ier = c_fstfrm(iun);
    ier = c_fclos(iun);

    App_End(-1);
}


void xmlconvip(char xmlip1[], int ip1)
{
    float niveau;
    int32_t kind, mode;
    int32_t flag = 1;
    int32_t lip1;

    mode = -1;
    lip1 = ip1;
    for (int i = 0; i < 32; i++) xmlip1[i]='\0';

    f77name(convip_plus)(&lip1, &niveau, &kind, &mode, xmlip1, &flag, (F2Cl)32);
    xmlip1[31] = '\0';
    nettoyer(xmlip1);
}


void xmlconvdate(char xmldate[], int dateo)
{
    int mode = -3;
    int ldateo = dateo;

    int yyyymmdd;
    int hhmmssss;
    f77name(newdate)(&ldateo, &yyyymmdd, &hhmmssss, &mode);

    int yyyy = yyyymmdd / 10000;
    int month = (yyyymmdd / 100) % 100;
    int day   = yyyymmdd % 100;

    int hour = hhmmssss / 1000000;
    int minutes = (hhmmssss / 10000) % 100;
    int sec = (hhmmssss % 100);

    sprintf(xmldate, "%04d-%02d-%02dT%02d:%02d:%02dZ", yyyy, month, day, hour, minutes, sec);
}

void nettoyer(char chaine[])
{
    int longueur = strlen(chaine) - 1;
    while (longueur>=0 && (chaine[longueur] == ' ' || chaine[longueur] == '\0')) {
        chaine[longueur] = '\0';
        longueur--;
    }
}

