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

    char encoding[16];
    char format[32];
    int ok, iun, key, tokens_per_line;

    char *liste[5], lcl_liste[5][256], *def[5], lcl_def[5][16], val[5][256];
    int n, npos, nptsRLE;

    int isallocated, multi;

    char xmldateo[32], xmldatev[32];
    char xmlip1[32], xmlip2[32], xmlip3[32];
    double nhours;

    fst_file*   fstfile=NULL;
    fst_record  record= default_fst_record;
    fst_query*  query=NULL; 
    FILE*       xmlfd;

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

    strcpy(encoding, val[2]);
    strcpy(format,  val[3]);
    sscanf(val[4], "%d", &tokens_per_line);

    iun = 1;
    fstfile=fst24_open(val[0],"RND+R/O");

    if (0 == strcmp(val[1], def[1])) {
        xmlfd = stdout;
    } else {
        xmlfd = fopen(val[1], "w");
    }

    if (xmlfd == NULL) {
        App_Log(APP_ERROR,"Cannot open output file... Exiting...");
        App_End(13);
        exit(13);
    }

    fprintf(xmlfd, "%s\n", "<?xml version='1.0' encoding='utf-8' standalone='yes'?>");
    fprintf(xmlfd, "%s\n", "<rpn-standard-file>");

    query = fst24_new_query(fstfile,NULL,NULL);
    while(fst24_find_next(query,&record)) {

        if (record.data_bits > 32) {
            multi = 2;
        } else {
            multi = 1;
        }
        xmlconvip(xmlip1, record.ip1);
        xmlconvdate(xmldateo, record.dateo);
        xmlconvdate(xmldatev, record.datev);

        /*---------------------------------------------------------------*/
        fprintf(xmlfd, "%s\n", "<fstrecord>");
        fprintf(xmlfd, "\t%s%s%s\n", "<nomvar>", record.nomvar, "</nomvar>");
        fprintf(xmlfd, "\t\t%s%s%s\n", "<typvar>", record.typvar, "</typvar>");
        fprintf(xmlfd, "\t\t%s%s%s\n", "<etiket>", record.etiket, "</etiket>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<ip1>", record.ip1, "</ip1>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<ip2>", record.ip2, "</ip2>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<ip3>", record.ip3, "</ip3>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<ni>", record.ni, "</ni>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<nj>", record.nj, "</nj>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<nk>", record.nk, "</nk>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<dateo>", record.dateo, "</dateo>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<datev>", record.datev, "</datev>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<deet>", record.deet, "</deet>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<npas>", record.npas, "</npas>");
        fprintf(xmlfd, "\t\t%s%s%s\n", "<grtyp>", record.grtyp, "</grtyp>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<ig1>", record.ig1, "</ig1>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<ig2>", record.ig2, "</ig2>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<ig3>", record.ig3, "</ig3>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<ig4>", record.ig4, "</ig4>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<nbits>", record.data_bits, "</nbits>");
        fprintf(xmlfd, "\t\t%s%d%s\n", "<datyp>", record.data_type, "</datyp>");
 
        fprintf(xmlfd, "\t\t%s%s%s\n", "<level>", xmlip1, "</level>");
        fprintf(xmlfd, "\t\t%s%s%s\n", "<date-of-origin>", xmldateo, "</date-of-origin>");
        fprintf(xmlfd, "\t\t%s%s%s\n", "<date-of-validity>", xmldatev, "</date-of-validity>");

        /*---------------------------------------------------------------*/


        fprintf(xmlfd, "\t\t%s\n", "<fstdata encoding='ASCII' format='14.6g' orientation='south-to-north'>");
        fprintf(xmlfd, "\t\t%s", "<values>");

        fst24_read_record(&record);
 
        switch (record.data_type) {
            case 1:
            case 5:
            case 6:
            case 133:
            case 134:
                 for (int i = 0; i < record.ni * record.nj * record.nk; i++) {
                    fprintf(xmlfd, format, ((float*)(record.data))[i]);
                    if (0 == ((i+1) % tokens_per_line)) {
                        fprintf(xmlfd, "\n");
                    }
                }
                break;

            case 2:
            case 4:
            case 130:
            case 132:
                for (int i = 0; i < record.ni * record.nj * record.nk; i++) {
                    fprintf(xmlfd, format, ((int*)(record.data))[i]);
                    if (0 == ((i+1) % tokens_per_line)) {
                        fprintf(xmlfd, "\n");
                    }
                }
                break;

            default:
                App_Log(APP_WARNING,"Cannot process data_type %d , skipping record nomvar=%s\n\n",record.data_type,record.nomvar);
        }
        fprintf(xmlfd, "\t\t%s\n", "</values>");
        fprintf(xmlfd, "\t\t%s\n", "</fstdata>");
        fprintf(xmlfd, "%s\n", "</fstrecord>");

    }

    fprintf(xmlfd, "%s\n", "</rpn-standard-file>");
    fclose(xmlfd);
    fst24_close(fstfile);

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

