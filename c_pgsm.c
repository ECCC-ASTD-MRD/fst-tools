#include <stdio.h>
#include <rpnmacros.h>

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

/*
****
****
*/

int f77name(pgsmform)(char format[],int *lenFormat, int fortranLenFormat)
{
   char cFormat[32];
   int i, mode, yyyymmhh, hhmmssss;
   
   for (i=0; i < 32; i++)
      {
      cFormat[i] = '\0';
      }

   i = *lenFormat-1;
   while (i >= 0 && format[i] == ' ')
      {
      format[i] = '\0';
      i--;
      }
   
   cFormat[0] = '%';
   strcpy(&cFormat[1],&format[1]);
   
   cFormat[strlen(cFormat)] = (char) tolower(format[0]);
   strcpy(format, cFormat);
   
}

/*
****
****
*/

int f77name(pgsmecho)(int *iun, char message[],int *lenMessage, int fortranLenMessage)
{
   message[*lenMessage] = '\0';
   fprintf(ftnUnits[*iun],"%s\n",message);
}

/*
****
****
*/

int f77name(pgsmof)(int *iun, char *nomFichier,int lenNomFichier)
{
   int i;

   i = 0;
   while (nomFichier[i] != ' ')
      {
      i++;
      }
   nomFichier[i] = NULL;
      
   ftnUnits[*iun] = fopen(nomFichier, "w");
   if (ftnUnits[*iun] == NULL)
      {
      fprintf(stderr, "Impossible d'ouvrir le fichier: %s\n", nomFichier);
      return -1;
      }
   
   return 0;
   
}

/*
****
****
*/

int f77name(pgsmcf)(int *iun)
{
   fclose(ftnUnits[*iun]);
}

/*
****
****
*/

int f77name(pgsmwr)(int *iun,float *data,int *ni, int *nj, int *nk ,char *format,int *position,int *idents,char *separateur,
             char *nomvar,char *typvar,char *etiket,int *dateo,int *datev,int *ip1,int *ip2,int *ip3,
             float *lat,float *lon)
{
   int i,j;
   char c_etiket[9],c_typvar[2],c_nomvar[3], c_separateur[2];
   char string[256];
   char internalFormat[16],latlonformat[32];
   int latlonflag = 0;
   int longform=16;
   int npts = *ni * *nj * *nk;

   
   strncpy(c_nomvar,nomvar,2);
   strncpy(c_typvar,typvar,1);
   strncpy(c_etiket,etiket,8);
   strncpy(internalFormat,format,16);
   c_separateur[0] = separateur[0];

   if (c_separateur[0] == 'T')
      {
      c_separateur[0] = '\t';
      }

   c_nomvar[2] = '\0';
   c_typvar[1] = '\0';
   c_etiket[8] = '\0';
   c_separateur[1] = '\0';

   f77name(pgsmform)(internalFormat,&longform,16);
   sprintf(latlonformat,"%s%s%s%s","%s",internalFormat,"%s",internalFormat);
   i = 0;
   while (i < 16)
      {
      if (idents[i] == LAT || idents[i] == LON) 
         {
         latlonflag = 1;
         }
      i++;
      }
   
   for (j=0; j < npts; j++)
      {
      if (*position == NORD)
         {
         if (j == 0)
            {
            ImprimeIdent(string,idents,c_separateur,c_nomvar,c_typvar,c_etiket,*ip1,*ip2,*ip3,*dateo,*datev, *ni, *nj, *nk);
            string[strlen(string)-1] = '\n';
            fprintf(ftnUnits[*iun],"%s",string);
            }
         }

      if (*position == OUEST)
         {
         if (latlonflag)
            {
            ImprimeIdent(string,idents,c_separateur,c_nomvar,c_typvar,c_etiket,*ip1,*ip2,*ip3,*dateo,*datev, *ni, *nj, *nk);
            fprintf(ftnUnits[*iun],"%s%s",string, c_separateur);
            }
         else
            {
            if (j == 0)
               {
               ImprimeIdent(string,idents,c_separateur,c_nomvar,c_typvar,c_etiket,*ip1,*ip2,*ip3,*dateo,*datev, *ni, *nj, *nk);
               fprintf(ftnUnits[*iun],"%s%s",string, c_separateur);
               }
            }
         }

      if (j%5 == 0 && j != 0 && (*position == NORD || *position == SUD))
         {
         fprintf(ftnUnits[*iun],"\n");
         }
      fprintf(ftnUnits[*iun], internalFormat, data[j]);
      
      if (latlonflag)
         {
         fprintf(ftnUnits[*iun], latlonformat, c_separateur, lat[j],  c_separateur, lon[j]);
         }
      else
         {
         if (j < npts-1)
            {
            fprintf(ftnUnits[*iun], "%s", c_separateur);
            }
         }
      
      if (*position == NORD || *position == OUEST || *position == 0)
        {
        if (latlonflag || j == npts-1)
          fprintf(ftnUnits[*iun], "\n");
        }
      
      
      if (*position == SUD)
         {
         if (latlonflag) 
            {
            if (j < npts-1) fprintf(ftnUnits[*iun],"\n");
            }
         
         
         if (j == npts-1)
            {
            ImprimeIdent(string,idents,c_separateur,c_nomvar,c_typvar,c_etiket,*ip1,*ip2,*ip3,*dateo,*datev, *ni, *nj, *nk);
            string[strlen(string)-1] = '\n';
            fprintf(ftnUnits[*iun],"\n%s",string);
            }
         }

      if (*position == EST)
         {
         if (latlonflag || j == npts-1)
            {
            ImprimeIdent(string,idents,c_separateur,c_nomvar,c_typvar,c_etiket,*ip1,*ip2,*ip3,*dateo,*datev, *ni, *nj, *nk);
            fprintf(ftnUnits[*iun],"%s%s\n",c_separateur,string);
            }
         }

      }
   printf(" ECRIT -- %s \t %s \t %05d \t %04d \t %03d \t %s \t %08d\n", c_nomvar, c_typvar, *ip1, *ip2, *ip3, c_etiket, *dateo);
}


GetIdent(char string[],int item, char *nomvar, char *typvar, char *etiket, 
         int ip1, int ip2, int ip3, int dateo, int datev, int ni, int nj, int nk)
{
  int mode, yyyymmdd, hhmmssss;   
  switch(item)
      {
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
	mode = -3;
	f77name(newdate)(&dateo, &yyyymmdd, &hhmmssss, &mode);
        sprintf(string, "%08d %08d", yyyymmdd, hhmmssss);
        break;

      case DATEV:
	mode = -3;
	f77name(newdate)(&datev, &yyyymmdd, &hhmmssss, &mode);
        sprintf(string, "%08d %08d", yyyymmdd, hhmmssss);
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
}


ImprimeIdent(char longString[], int items[],char *separateur, char *nomvar, char *typvar, char *etiket, 
             int ip1, int ip2, int ip3, int dateo, int datev, int ni, int nj, int nk)
{
   int i;
   char string[32];
   
   i = 0;
   strcpy(longString,"");

   while (items[i] != 0 && i < 16)
      {
      switch (items[i])
         {
         case LAT:
         case LON:
           break;
           
         default:
           GetIdent(string,items[i],nomvar,typvar,etiket,ip1,ip2,ip3,dateo,datev,ni,nj,nk);
           strcat(string,separateur);
           strcat(longString,string);
           break;
         }
      i++; 
      }
}

