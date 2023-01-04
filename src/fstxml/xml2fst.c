//! \file xml2fst.c Convertisseur de fichier XML en fichier standard RPN version 2000

#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <pwd.h>

#include <App.h>
#include <rmn/rpnmacros.h>
#include <rmn/fnom.h>
#include <rmn/fstd98.h>

#include "fst-tools_build_info.h"

/* introducing a new enum called Bool */
typedef enum
{
    no,
    yes
} Bool;

/* for null pointers */
#define null 0

Bool IsWhite(uint32_t c);

typedef void (ParseProperty)(char *option);

ParseProperty ParseInt;      /* parser for an integer value */
ParseProperty ParseString;   /* a string including whitespace */
ParseProperty SkipTag;       /* skip tag */
ParseProperty ParseValues;   /* parser for array of integer/float values */


static uint32_t c;               /* current char in input stream */
static FILE *fin;            /* file pointer for input stream */
static int MAX_SIZE  = 8192000;
static int MAX_SIZE1 = 2000000;

typedef struct _plist PList;

struct _plist
{
  char *name;                     /* property name */
  ParseProperty *parser;          /* parsing method */
  PList *next;                    /* linear hash chaining */
};

/* #define DEBUG */
#define HASHSIZE 101

static PList *hashtable[HASHSIZE];   /* private hash table */
static Bool initialized = no;

/* used parsing the command line */
static char *config_text;

static struct Flag
{
    char *name;                     /* property name */
    ParseProperty *parser;          /* parsing method */
} flags[] =
{
  {"?xml",               SkipTag},
  {"rpn-standard-file",  SkipTag},
  {"fstrecord",          SkipTag},
  {"fstprm",             SkipTag},
  {"nomvar",             ParseString},
  {"typvar",             ParseString},
  {"etiket",             ParseString},
  {"grtyp",              ParseString},
  {"ip1",                ParseInt},
  {"ip2",                ParseInt},
  {"ip3",                ParseInt},
  {"ni",                 ParseInt},
  {"nj",                 ParseInt},
  {"nk",                 ParseInt},
  {"dateo",              ParseInt},
  {"deet",               ParseInt},
  {"npas",               ParseInt},
  {"npak",               ParseInt},
  {"ig1",                ParseInt},
  {"ig2",                ParseInt},
  {"ig3",                ParseInt},
  {"ig4",                ParseInt},
  {"nbits",              ParseInt},
  {"datyp",              ParseInt},
  {"swa",                ParseInt},
  {"lng",                ParseInt},
  {"dltf",               ParseInt},
  {"ubc",                ParseInt},
  {"extra1",             ParseInt},
  {"extra2",             ParseInt},
  {"extra3",             ParseInt},
  {"level",              ParseInt},
  {"values",             ParseValues},
  {"fstdata",            SkipTag},

  /* this must be the final entry */
  {0,  0}
};

int ni, nj, nk, ier, ip1, ip2, ip3, ig1, ig2, ig3, ig4;
int dateo, datev, datyp, deet, nbits, npak, npas, swa, lng;
int dltf, ubc, extra1, extra2, extra3;


char *nomvar, *etiket, *typvar, *grtyp, *values;

double nhours;
int iun;

int *intdata;
float *floatdata;
char *chardata;

/* type of data in RPN standard files*/
#define RBINARY 0
#define FLOATN  1
#define INTN    2
#define CHR     3
#define SINT    4
#define I3E     5

#define FAILURE 100

void ParseConfigFile(char *xmlFile);
void extract_data(char * buf, int datatype);
void setIntVar(char *option, int number);

static void trimleft(char *string);
static void trimright(char *string);

extern void c_ccard(char **argv, int argc, char **cle, char val[][256],
                    char **def, int n, int *npos);

void xml2fst(int argc, char **argv)
{
  char fstFile[256], xmlFile[256];
  char encoding[16];
  char format[32];

  char *liste[4], lcl_liste[4][256], *def[4], lcl_def[4][256], val[4][256];
  int i, npos;

  strcpy(lcl_liste[0], "xml.");
  strcpy(lcl_liste[1], "fst.");
  strcpy(lcl_liste[2], "encoding.");
  strcpy(lcl_liste[3], "format.");

  /* liste[0] = (char *) lcl_liste[0]; */
  liste[0] = &lcl_liste[0][0];
  liste[1] = &lcl_liste[1][0];
  liste[2] = &lcl_liste[2][0];
  liste[3] = &lcl_liste[3][0];


  strcpy(lcl_def[0], "bidon.xml");
  strcpy(lcl_def[1], "bidon.fst");
  strcpy(lcl_def[2], "ascii");
  strcpy(lcl_def[3], "(8g14.6)");

  def[0] = &lcl_def[0][0];
  def[1] = &lcl_def[1][0];
  def[2] = &lcl_def[2][0];
  def[3] = &lcl_def[3][0];


  for (i = 0; i < 4; i++)
    {
      strcpy(val[i], def[i]);
    }

  npos = 0;

  c_ccard(argv, argc, &liste[0], val, &def[0], 4, &npos);

    App_Init(APP_MASTER,"xml2fst",VERSION,"",BUILD_TIMESTAMP);
    App_Start();

  strcpy(xmlFile, val[0]);
  strcpy(fstFile, val[1]);
  strcpy(encoding, val[2]);
  strcpy(format,  val[3]);

#ifdef DEBGU
  fprintf(stderr, "output fstFile is: %s\n", fstFile);
#endif

  iun = 1;

  c_fnom(&iun, fstFile, "RND+R/W", 0);
  ier = c_fstouv(iun, "RND");

  if (0 == strcmp(val[1], def[1]))
    {
      ier = c_fstopc("MSGLVL", "FATALE", 0);
    }

  else
    {
      ParseConfigFile(xmlFile); /* start parsing xml file to fst file */
    }

  /* close fst file once data has been written */
  ier = c_fstfrm(iun);

  App_End(-1);
  exit(0);
}

/* display warning message for option not listed */
void ReportBadArgument(char *option)
{

#ifdef DEBUG
  if (strstr(option, "/") != null)
    fprintf(stderr, "Closing tag \"%s\" has been found\n", option);
  else if (strstr(option, "           ") == null)
    fprintf(stderr, "Warning - Juste White space !\n");
  else
    fprintf(stderr, "Warning - missing or malformed argument for option: %s\n", option);
#endif
}

/* test if character is a white space */
Bool IsWhite(uint32_t c)
{
  if (c == ' ')
    return yes;
  else
    return no;
}

/* read next character from input file */
static unsigned GetC(FILE *fp)
{
    if (fp)
      {
        return getc(fp);
      }

    if (!config_text)
      {
        return EOF;
      }

    if (*config_text)
      {
        return *config_text++;
      }

    return EOF;
}

static unsigned hash(char *s)
{
    unsigned hashval;
    for (hashval = 0; *s != '\0'; s++)
        hashval = toupper(*s) + 31*hashval;

    return hashval % HASHSIZE;
}

static PList *lookup(char *s)
{
    PList *np;

#ifdef DEBUG
    fprintf(stderr, "lookup(), looking for: \"%s\"\n", s);
#endif

    for (np = hashtable[hash(s)]; np != null; np = np->next)
      {
        if (strcmp(s, np->name) == 0)
          return np;
      }
    return null;
}

static int AdvanceChar()
{
    if (c != EOF)
        c = (uint32_t)GetC(fin);
    return c;
}

void SkipTag(char *option)
{

#ifdef DEBUG
  fprintf(stderr, "SkipTag(), tag: \"%s\"\n", option);
#endif

  while(c != '<' && c != EOF)
    AdvanceChar();
}

static int SkipWhite()
{
    while (IsWhite((uint32_t) c))
        c = (uint32_t)GetC(fin);

    return c;
}

/*
 skip over line continuations
 to start of next property
*/
static int NextProperty()
{
  do
    {
      /* skip to end of line */
      while (c != '\n' && c != '\r' && c != EOF)
        c = (uint32_t)GetC(fin);

      /* treat  \r\n   \r  or  \n as line ends */
      if (c == '\r')
        c = (uint32_t)GetC(fin);

      if (c == '\n')
        c = (uint32_t)GetC(fin);

    }while (IsWhite(c));  /* line continuation? */

   while (IsWhite(c) || c == '\t' || c == '<' || c == '/')
   {
     AdvanceChar();

   }

#ifdef DEBUG
  fprintf(stderr, "NextProperty(), last char found:\"%c\"\n", c);
#endif

  return c;
}

/* unsigned integers */
void ParseInt(char *option)
{
    int number = 0;
    Bool digits = no;

    SkipWhite();

    if (strlen(option) > 1)
      {
        if (c == '>')
          AdvanceChar();

        while(isdigit(c))
          {
            number = c - '0' + (10 * number);
            digits = yes;
            AdvanceChar();
          }

        if (!digits && (strcmp(option, "level") != 0))
          ReportBadArgument(option);
      }

#ifdef DEBUG
    fprintf(stderr, "ParseInt(char *option), tag name = \"%s\", with value = \"%d\"\n", option, number);
#endif

    setIntVar(option, number);
    NextProperty();
}


void write_to_fstFile()
{
  float work[10];

#ifdef DEBUG
  fprintf(stderr, "************** c_fstecr args are: **********************\n");
  fprintf(stderr, "nomvar = %s\n", nomvar);
  fprintf(stderr, "typvar = %s\n", typvar);
  fprintf(stderr, "etiket = %s\n", etiket);
  fprintf(stderr, "ip1    = %d\n", ip1);
  fprintf(stderr, "ip2    = %d\n", ip2);
  fprintf(stderr, "ip3    = %d\n", ip3);
  fprintf(stderr, "ni     = %d\n", ni);
  fprintf(stderr, "nj     = %d\n", nj);
  fprintf(stderr, "nk     = %d\n", nk);
  fprintf(stderr, "dateo  = %d\n", dateo);
  fprintf(stderr, "deet   = %d\n", deet);
  fprintf(stderr, "npas   = %d\n", npas);
  fprintf(stderr, "grtyp  = %s\n", grtyp);
  fprintf(stderr, "ig1    = %d\n", ig1);
  fprintf(stderr, "ig2    = %d\n", ig2);
  fprintf(stderr, "ig3    = %d\n", ig3);
  fprintf(stderr, "ig4    = %d\n", ig4);
  fprintf(stderr, "nbits  = %d\n", nbits);
  fprintf(stderr, "datyp  = %d\n", datyp);
  fprintf(stderr, "swa    = %d\n", swa);
  fprintf(stderr, "lng    = %d\n", lng);
  fprintf(stderr, "dltf   = %d\n", dltf);
  fprintf(stderr, "ubc    = %d\n", ubc);
  fprintf(stderr, "extra1 = %d\n", extra1);
  fprintf(stderr, "extra2 = %d\n", extra2);
  fprintf(stderr, "extra3 = %d\n", extra3);
  fprintf(stderr, "npak = %d\n", npak);
  fprintf(stderr, "iun = %d\n\n", iun);
#endif
/*   nomvar = "\0"; */
/*   nomvar = "ABDC\0"; */

  App_Log(APP_INFO,"nomvar = >%s<\n", nomvar);

  if (datyp == INTN)
    {
      App_Log(APP_INFO,"Writing int values to fst file\n");
      ier = c_fstecr(intdata, work, nbits, iun, dateo, deet, npas, ni, nj, nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, "false");

      if (intdata)
        free(intdata);

    }

  else if (datyp == FLOATN || datyp == I3E)
    {
      App_Log(APP_INFO,"Writing float values to fst file \n");
      ier = c_fstecr(floatdata, work, nbits, iun, dateo, deet, npas, ni, nj, nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, "false");

      if (floatdata)
        free(floatdata);

    }
  else if (datyp == CHR)
    {
      App_Log(APP_INFO,"Writing character values to fst file\n");

      chardata = (char *)malloc(256 * sizeof(char));

      if (chardata)
        {
          chardata = "abcedef";
          ier = c_fstecr(chardata, work, nbits, iun, dateo, deet, npas, ni, nj, nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, "false");

          /* free(chardata); */
        }

      else
        {
          App_Log(APP_ERROR,"Cannot allocate memory for chardata\n");
          App_End(-1);
          exit(FAILURE);
        }

    }

}

void setIntVar(char *option, int number)
{
 if (strcmp(option, "ip1") == 0)
   {
      ip1 = number;

   }
 else if (strcmp(option, "ip2") == 0)
   {
     ip2 = number;

   }

 else if (strcmp(option, "ip3") == 0)
   ip3 = number;

 else if (strcmp(option, "ni") == 0)
   ni = number;

 else if (strcmp(option, "nj") == 0)
   nj = number;

 else if (strcmp(option, "nk") == 0)
   nk = number;

 else if (strcmp(option, "dateo") == 0)
   {
     dateo = number;
   }

 else if (strcmp(option, "deet") == 0)
   deet = number;

 else if (strcmp(option, "ig1") == 0)
   ig1 = number;

 else if (strcmp(option, "ig2") == 0)
   ig2 = number;

 else if (strcmp(option, "ig3") == 0)
   ig3 = number;

 else if (strcmp(option, "ig4") == 0)
   ig4 = number;

  else if (strcmp(option, "nbits") == 0)
    nbits = -number;

  else if (strcmp(option, "datyp") == 0)
    datyp = number;

 else if (strcmp(option, "swa") == 0)
   swa = number;

 else if (strcmp(option, "lng") == 0)
   lng = number;

 else if (strcmp(option, "npas") == 0)
   npas = number;

 else if (strcmp(option, "npak") == 0)
   {
     npak = number;
   }

}

/* set string type variables */
void setStringVar(char *option, char *buf)
{

  if (strcmp(option, "nomvar") == 0)
    {
      nomvar = (char *)malloc(strlen(buf));
      strcpy(nomvar, "    ");
      memcpy(nomvar, buf, strlen(buf));
      if (strstr(buf, ">") != null)
        {
          nomvar = ">>";

          trimright(nomvar);
          App_Log(APP_INFO,"setStringVar(char *option), tag: \"%s\", with value: \"%s\" 3\n", option, nomvar);

        }
      /* trimleft(nomvar); */
#ifdef DEBUG
      fprintf(stderr, "setStringVar(char *option), tag: \"%s\", with value: \"%s\" 3\n", option, nomvar);
#endif
    }

  else if (strcmp(option, "typvar") == 0)
    {
      typvar = (char *)malloc(strlen(buf));

      memcpy(typvar, buf, strlen(buf));
    }

  else if (strcmp(option, "etiket") == 0)
    {
      etiket = (char *)malloc(strlen(buf));

      memcpy(etiket, buf, strlen(buf));

    }

  else if (strcmp(option, "grtyp") == 0)
    {

      grtyp = (char *)malloc(strlen(buf));

      memcpy(grtyp, buf, strlen(buf));
    }

  else if (strcmp(option, "values") == 0)
    {
      values = (char *)malloc(MAX_SIZE);
      values = (char *)buf;

#ifdef DEBUG
      fprintf(stderr, "setStringVar(char *option), tag: \"%s\", with datatype \"%d\" \n", option, datyp);
#endif
      extract_data(values, datyp);

    }

}


/* a string including whitespace */
/* munges whitespace sequences */
void ParseString(char *option)
{
  int MAX_SIZE = 256;
  char buf[MAX_SIZE];
  int i = 0;
  unsigned delim = 0;
  Bool waswhite = yes;

  /* initialize buffer to empty string */
  for (i = 0; i < MAX_SIZE; i++)
    {
      buf[i] = ' ';
    }

  i = 0;

  SkipWhite();

#ifdef DEBUG
  fprintf(stderr, "ParseString(char *option), option = \"%s\"\n", option);
#endif

  if (c == '<' || c == '/')
    delim = c;

  while (i < MAX_SIZE  && c != EOF && c != '<')
    {
      /* treat  \r\n   \r  or  \n as line ends */
      if (c == '\r')
        {
          AdvanceChar();
          if (c != '\n' && !IsWhite(c))
            {
              break;
            }
        }

      if (c == '\n')
        {
          AdvanceChar();

          if (!IsWhite(c))
            {
              break;
            }
        }

      if (c == delim && delim != '\0')
        break;

      if (IsWhite(c))
        {
          if (waswhite)
            {
              AdvanceChar();
              continue;
            }

          c = ' ';
        }
      else
        waswhite = no;

      /* New */
      if (c == '>')
        {
          AdvanceChar();

          if ( i > 0)
            {
              break;
            }
        }
      /*New */
      buf[i++] = c;
      AdvanceChar();

    }

  buf[i] = '\0';
#ifdef DEBUG
  fprintf(stderr, "ParseString(char *option), option = \"%s\", buf = >%s< \n", option, buf);
#endif
  if (strlen(buf) >= 1 && strcmp(buf, "\0") != 0 && strcmp(buf, "    ") != 0)
    {
      setStringVar(option, buf);
    }



#if 0
  if (i == 0)
    ReportBadArgument(option);
#endif

}

/* a string including whitespace */
/* munges whitespace sequences */
void ParseValues(char *option)
{
  char buf[MAX_SIZE1];
  int i = 0;
  unsigned delim = 0;
  Bool waswhite = yes;

  SkipWhite();

#ifdef DEBUG
  fprintf(stderr, "ParseValues(char *option), option = \"%s\"\n", option);
  fprintf(stderr, "ParseValues(char *option), nomvar: \"%s\" \n", nomvar);
  fprintf(stderr, "ParseValues(char *option), typvar: \"%s\" \n", typvar);
  fprintf(stderr, "ParseValues(char *option), grtyp: \"%s\" \n", grtyp);
  fprintf(stderr, "ParseValues(char *option), etiket: \"%s\" \n", etiket);
  fprintf(stderr, "ParseValues(char *option), MAX_SIZE: \"%d\" \n", MAX_SIZE);
#endif

  if (c == '<' || c == '/')
    delim = c;

  while (i < MAX_SIZE1 && c != EOF && c != '<') /* keep reading characters from opening tag "<opening_tag>"
                                                   until closing tag "</closing_tag>"  */
    {
        /* treat  \r\n   \r  or  \n as line ends */
      if (c == '\r')
        {
          AdvanceChar();

          if (c != '\n' && !IsWhite(c))
            {
              break;
            }
        }

      if (c == '\n')
        {
           AdvanceChar();

          if (!IsWhite(c) && c != '-') /* don't skip minus sign, it's part of number */
            {
              break;
            }
        }

      if (c == delim && delim != '\0')
        break;

      if (IsWhite(c))
        {
          if (waswhite)
            {
              AdvanceChar();
              continue;
            }

          c = ' ';
        }
      else
        waswhite = no;

      /* New */
      if (c == '>')
        {
          AdvanceChar();
        }
      /* New */

      buf[i++] = c; /* cumulate charcters into buf */
      AdvanceChar();

    }

  buf[i] = '\0';

  if (i > 0)
    MAX_SIZE = i;

  if (strcmp(buf, " ") > 0)
    {
      setStringVar(option, buf);
    }

#if 0
    if (i == 0)
      ReportBadArgument(option);
#endif

}

/* depending on data type, an array of values
   will be extratced from data buffer using space token
   that separate numbers */
void extract_data(char * buf, int datatype)
{

  int i = 0;
  char *token;
  char *delimiters = " \t\n";

  token = strtok(buf, delimiters);

#ifdef DEBUG
  fprintf(stderr, "extract_data(char *option), before parsing values, nomvar: \"%s\" \n", nomvar);
  fprintf(stderr, "extract_data(char *option), before parsing values, typvar: \"%s\" \n", typvar);
  fprintf(stderr, "extract_data(char *option), before parsing values, grtyp: \"%s\" \n", grtyp);
  fprintf(stderr, "extract_data(char *option), before parsing values, etiket: \"%s\" \n", etiket);
  fprintf(stderr, "extract_data(char *option), parsing first token value = \"%s\"\n", token);
#endif

  if (datatype == INTN)
    {
      intdata = (int *)malloc(MAX_SIZE * sizeof(int));

#ifdef DEBUG
      fprintf(stderr, "extract_data(char *option), parsing int value[%d] = \"%d\"\n", i, atoi(token));
#endif

      if (intdata)
        {
          if (token)
            intdata[i] = atoi(token);
          else
            intdata[i] = 0;
        }
      else
        {
          App_Log(APP_ERROR,"%s: PROBLEM allocating memory for integer data\n",__func__);
          App_End(-1);
          exit(FAILURE);
        }

    }

  else if (datatype == FLOATN || datatype == I3E)
    {
      floatdata = (float *)malloc(MAX_SIZE * sizeof(float));

      if (floatdata)
        floatdata[i] = strtod(token, (char **)null);
      else
        {
          App_Log(APP_ERROR,"%s: PROBLEM allocating memory for float data\n",__func__);
          App_End(-1);
          exit(FAILURE);
        }


#ifdef DEBUG
      if (i == 0 || i%100 == 0)
        {
          fprintf(stderr, "extract_data(char *option), before parsing values, nomvar: \"%s\" \n", nomvar);
          fprintf(stderr, "extract_data(char *option), parsing float value[%d] = \"%12.4f\"\n", i, floatdata[i]);
        }
#endif

    }
  else if (datatype == CHR)
    {
      chardata = (char *)malloc(MAX_SIZE * sizeof(char));

      if (chardata)
        {
          if (token)
            {
              chardata = token;
            }
          else
            {
              chardata = " ";
            }
        }
      else
        {
          App_Log(APP_ERROR,"%s: PROBLEM allocating memory for character data\n",__func__);
          App_End(-1);
          exit(FAILURE);
        }
    }


  while((token = strtok(null, delimiters)) != null)
    {

      if (datatype == INTN)
        {
          intdata[++i] = atoi(token);

#ifdef DEBUG
          fprintf(stderr, "extract_data(char *option), parsing int value[%d] = \"%d\"\n", i, intdata[i]);
#endif

        }
      else if (datatype == FLOATN || datatype == I3E)
        {
          floatdata[++i] = strtod(token, (char **)null);

#ifdef DEBUG
          if (i == 0 || i % 100 == 0)
            {
              fprintf(stderr, "extract_data(char *option), before parsing values, nomvar: \"%s\" \n", nomvar);
              fprintf(stderr, "extract_data(char *option), parsing float value[%d] = \"%10.4f\"\n", i, floatdata[i]);
            }
#endif

        }

      else if (datatype == CHR)
        {
          if (token != null)
            {
              chardata = token;
            }
          else
            {
              chardata = " ";
            }

        }
    }

}


/*
 For expanding ~/foo or ~your/foo according to $HOME and your
 user name. This will only work on Unix systems.
*/
const char *ExpandTilde(const char *filename)
{
    static char *expanded_filename;

    char *home_dir, *p;
    struct passwd *passwd = null;

    if (!filename) return(null);

    if (filename[0] != '~')
        return(filename);

    if (filename[1] == '/')
    {
        home_dir = getenv("HOME");
        filename++;
    }
    else
    {
        const char *s;
        char *t;

        s = filename + 1;

        while(*s && *s != '/') s++;

        if (t = malloc(s - filename))
        {
            memcpy(t, filename+1, s-filename-1);
            t[s-filename-1] = 0;

            passwd = getpwnam(t);

            free(t);
        }

        if (!passwd)
            return(filename);

        filename = s;
        home_dir = passwd->pw_dir;
    }

    if (p = realloc(expanded_filename, strlen(filename)+strlen(home_dir)+1))
    {
        strcat(strcpy(expanded_filename = p, home_dir), filename);
        return(expanded_filename);
    }

    return(filename);
}

static PList *install(char *name, ParseProperty *parser)
{
    PList *np;
    unsigned hashval;

    if ((np = lookup(name)) == null)
    {
        np = (PList *)malloc(sizeof(*np));

        if (np == null )
            return null;

        hashval = hash(name);
        np->name = name;
        np->next = hashtable[hashval];
        hashtable[hashval] = np;

    }

    np->parser = parser;
    return np;
}


void InitConfig(void)
{
    struct Flag *p;

    if (!initialized)
    {
        initialized = yes;

        for(p = flags; p->name != null; ++p)
          {
            install(p->name, p->parser);
          }
    }

    c = 0;  /* init single char buffer */

}


void ParseConfigFile(char *file)
{
    int i;
    char name[256];
    const char *fname;
    PList *entry;

    /* setup property name -> parser table*/

    InitConfig();

#ifdef SUPPORT_GETPWNAM
    /* expand filenames starting with ~ */
    fname = ExpandTilde( file );
#else
    fname = file;
#endif

#ifdef DEBUG
    if (fname != null)
      fprintf(stderr, "ParseConfigFile(), File name received as argument \"%s\"\n", fname);
    else
      fprintf(stderr, "ParseConfigFile(), File name received as argument is null \n");
#endif

    /* open the file and parse its contents */
    if ((fin = fopen(fname, "r")) == null)
      App_Log(APP_WARNING,"bad file: %s\n",(char *)fname);
    else
    {
      config_text = null;
      AdvanceChar();  /* first char */

      while (c != EOF)
        {

          /* // starts a comment */
          while (c == '/')
            {
              NextProperty();

              if (c == '<')
                {
#ifdef DEBUG
                  fprintf(stderr, "ParseConfigFile(), first char found: \"%c\"\n", (char)c);
#endif
                  break;
                }
            }

          i = 0;

          if (c == '<' || c == '/' || c == '>') /* New today */
            {
              AdvanceChar();

            }

          while (c != '>' && c != '<' && c != EOF && i < 200)
            {
              name[i++] = (char)c;
              AdvanceChar();
            }

          name[i] = '\0';
          if (strstr(name, "           ") == null);
            /* fprintf(stderr, "ParseConfigFile(), tag found: \"%s\" \n", name); */

          if (strlen(name) > 1)
            {
              char *delim = "/";

              if (strstr(name, delim) != null && strcmp(name, "/fstrecord") != 0)
                {
                  entry = null;

                  if (strcmp(name, "/rpn-standard-file") != 0) /* Continue if not the last xml closing tag */
                    NextProperty();
                  else
                    exit(0);
                }

              else if (strstr(name, "           ") != null)
                {
                  entry = null;     /* don't parse white space */
                }

              else
                {
                  entry = lookup(name);
                }
            }

          if (entry)
            {

              if (strcmp(name, " ") == 0 || strlen(name) == 1)
                {
                  App_Log(APP_WARNING,"%s: name found is empty \n",__func__);
                }
              else
                {
                  entry->parser(name);
                }

            }
          else
            {

              if (strcmp(name, "/fstrecord") == 0) /* store if data closing tag found */
                {
                  write_to_fstFile();

                  NextProperty();
                }
              else if (strcmp(name, "/rpn-standard-file") != 0)
                {
                  ReportBadArgument(name);
                }
              else if (strcmp(name, " ") == 0)
                NextProperty();
            }
        }

        fclose(fin);
    }
}


//! Strip all leading whitesapce from a string
static void trimleft(
    //! [in] Input string
    char *string
) {
  char *string_dest;

  if (string != NULL)
    {
      string_dest = string;
      while (*string != '\0' && isspace(*string))
        string++;
      while (*string != '\0')
        *string_dest++ = *string++;
      *string_dest = '\0';
    }
}


//! Strip all trailing whitesapce from a string
static void trimright(
        //! [in] Input string
    char *string
) {
    char *string_dest;

    if (string != NULL && string[0] != '\0') {
        string_dest = string + strlen(string) - 1;
        while(isspace(*string_dest)) {
            *string_dest-- = '\0';
        }
    }
}
