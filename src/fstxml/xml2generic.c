#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>

/* -----------------------------------------------------------------------------------------------*/
/*  xml2generic : Convertisseur de fichier XML en fichier standard RPN version 2000

    Auteurs: Djamel Bouhemhem, Yves Chartier
    Statut: Alpha
                                                                                  
    To compile,
    make xml2generic
    
    To run, 
    ./xml2generic < file.xml
                                                                                                  */
/* -----------------------------------------------------------------------------------------------*/


/*   introducing a new enum called Bool */
typedef enum
{
  no,
  yes
} Bool;


Bool IsWhite(uint c);
void SimpleStats(float *fld, int npts);

typedef void (ParseProperty)(char *option);

ParseProperty ParseInt;      /* parser for an integer value */
ParseProperty ParseString;   /* a string including whitespace */
ParseProperty SkipTag;       /* skip tag */
ParseProperty ParseValues;   /* parser for array of integer/float values */


static uint c;               /* current char in input stream */
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

/* #define DEBUG 1 */
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
int rewrit = 0;
int dateo, datev, datyp, deet, nbits, npak, npas, swa, lng;
int dltf, ubc, extra1, extra2, extra3;


char *nomvar, *etiket, *typvar, *grtyp, *values;

double nhours;
int iun;

int *intdata;
float *floatdata;


/* type of data in RPN standard files*/
#define RBINARY 0
#define FLOATN  1
#define INTN    2
#define CHR     3
#define SINT    4
#define I3E     5

void ParseConfigFile();
void extract_data(char * buf, int datatype);
void setIntVar(char *option, int number);

/* -----------------------------------------------------------------------------------------------*/
int main(int argc, char **argv)
{
  int i, npos;

  ParseConfigFile(); /* start parsing xml file to fst file */

  exit(0);
}

/* -----------------------------------------------------------------------------------------------*/
/* display warning message for option not listed */
void ReportBadArgument(char *option)
{

#ifdef DEBUG
  if(strstr(option, "/") != NULL)
    fprintf(stderr, "Closing tag \"%s\" found ******\n", option);
  else if(strstr(option, "           ") == NULL)
    fprintf(stderr, "Warning - Juste White space !!!! *******\n");
  else
    fprintf(stderr, "Warning - missing or malformed argument for option: %s\n", option);
#endif
}

/* -----------------------------------------------------------------------------------------------*/
/* display error message if XML input not found */
void ReportError(char *option)
{
      fprintf(stderr, "Warning - bad file: %s\n", option);
}

/* -----------------------------------------------------------------------------------------------*/
/* test if character is a white space */
Bool IsWhite(uint c)
{
  if(c == ' ')
    return yes;
  else
    return no;
}

/* -----------------------------------------------------------------------------------------------*/
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

/* -----------------------------------------------------------------------------------------------*/
static unsigned hash(char *s)
  {
  unsigned hashval;
  for (hashval = 0; *s != '\0'; s++)
      hashval = toupper(*s) + 31*hashval;

  return hashval % HASHSIZE;
  }

/* -----------------------------------------------------------------------------------------------*/
static PList *lookup(char *s)
{
  PList *np;

#ifdef DEBUG
    fprintf(stderr, "lookup(), looking for: \"%s\"\n", s);
#endif

  for (np = hashtable[hash(s)]; np != NULL; np = np->next)
    {
    if (strcmp(s, np->name) == 0)
    return np;
    }
  return NULL;
}

/* -----------------------------------------------------------------------------------------------*/
static int AdvanceChar()
{
    if (c != EOF)
        c = (uint)GetC(stdin);
    return c;
}

/* -----------------------------------------------------------------------------------------------*/
void SkipTag(char *option)
{

#ifdef DEBUG
  fprintf(stderr, "SkipTag(), tag: \"%s\"\n", option);
#endif

  while(c != '<' && c != EOF)
    AdvanceChar();
}

/* -----------------------------------------------------------------------------------------------*/
static int SkipWhite()
  {
  while (IsWhite((uint) c))
      c = (uint)GetC(stdin);

  return c;
  }

/*
skip over line continuations
to start of next property
*/
/* -----------------------------------------------------------------------------------------------*/
static int NextProperty()
{
  do
    {
    /* skip to end of line */
    while (c != '\n' && c != '\r' && c != EOF) c = (uint)GetC(stdin);

    /* treat  \r\n   \r  or  \n as line ends */
    if (c == '\r')  c = (uint)GetC(stdin);

    if (c == '\n')  c = (uint)GetC(stdin);

    } while (IsWhite(c));  /* line continuation? */

  while (IsWhite(c) || c == '\t' || c == '<' || c == '/')
    {
    AdvanceChar();
    }

#ifdef DEBUG
  fprintf(stderr, "NextProperty(), last char found:\"%c\"\n", c);
#endif

  return c;
}

/* -----------------------------------------------------------------------------------------------*/
/* unsigned integers */
void ParseInt(char *option)
{
    int number = 0;
    Bool digits = no;

    SkipWhite();

    if(strlen(option) > 1)
      {
  if(c == '>')
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


/* -----------------------------------------------------------------------------------------------*/
void write_to_fstFile()
{
  float work[10];

/* #ifdef DEBUG */
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
  fprintf(stderr, "***************** iun = %d *********************\n", iun);
/* #endif */
  if(datyp == INTN)
    {
/*      ier = c_fstecr(intdata, work, nbits, iun, dateo, deet, npas, ni, nj, nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit);*/
      free(intdata);
      intdata = NULL;
    }


  else if(datyp == FLOATN || datyp == I3E)
    {
    SimpleStats(floatdata, ni*nj*nk);
/*      ier = c_fstecr(floatdata, work, nbits, iun, dateo, deet, npas, ni, nj, nk, ip1, ip2, ip3, typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit);*/
      free(floatdata);
      floatdata = NULL;
    }

    free(nomvar);
    free(typvar);
    free(etiket);
    free(grtyp);
    free(values);

    nomvar = NULL;
    typvar = NULL;
    etiket = NULL;
    grtyp  = NULL;
    values = NULL;
}

/* -----------------------------------------------------------------------------------------------*/
void setIntVar(char *option, int number)
{
if(strcmp(option, "ip1") == 0)
  {
      ip1 = number;
#ifdef DEBUG
      fprintf(stderr, "setIntVar(char *option), tag: \"%s\", with value = \"%d\" \n", option, number);
#endif      
  }
else if(strcmp(option, "ip2") == 0)
  {
    ip2 = number;
#ifdef DEBUG
    fprintf(stderr, "setIntVar(char *option), tag: \"%s\", with value = \"%d\" \n", option, number);
#endif      
  }

else if(strcmp(option, "ip3") == 0)
  ip3 = number;

else if(strcmp(option, "ni") == 0)
  ni = number;

else if(strcmp(option, "nj") == 0)
  nj = number;

else if(strcmp(option, "nk") == 0)
  nk = number;

else if(strcmp(option, "dateo") == 0)
  {
    dateo = number;
#ifdef DEBUG
    fprintf(stderr, "setIntVar(char *option), tag: \"%s\", with value = \"%d\" \n", option, number);
#endif
  }

else if(strcmp(option, "deet") == 0)
  deet = number;

else if(strcmp(option, "ig1") == 0)
  ig1 = number;

else if(strcmp(option, "ig2") == 0)
  ig2 = number;

else if(strcmp(option, "ig3") == 0)
  ig3 = number;

else if(strcmp(option, "ig4") == 0)
  ig4 = number;

  else if(strcmp(option, "nbits") == 0)
    nbits = -number;

  else if(strcmp(option, "datyp") == 0)
    datyp = number;

else if(strcmp(option, "swa") == 0)
  swa = number;

else if(strcmp(option, "lng") == 0)
  lng = number;

else if(strcmp(option, "npas") == 0)
  npas = number;

else if(strcmp(option, "npak") == 0)
  {
    npak = number;
#ifdef DEBUG
    fprintf(stderr, "setIntVar(char *option), tag: \"%s\", with value = \"%d\" \n", option, number);
#endif
  }

}

/* -----------------------------------------------------------------------------------------------*/
/* set string type variables */
void setStringVar(char *option, char *buf)
{

  if(strcmp(option, "nomvar") == 0)
    {
      nomvar = (char *)calloc(strlen(buf)+1, sizeof(char));

      memcpy(nomvar, buf, strlen(buf));

      if(strstr(buf, ">") != NULL)
        {
        strcpy(nomvar, ">>");
        }
#ifdef DEBUG
      fprintf(stderr, "setStringVar(char *option), tag: \"%s\", with value: \"%s\" \n", option, nomvar);      
#endif
    }

  else if(strcmp(option, "typvar") == 0)
    {
      typvar = (char *)calloc(strlen(buf)+1,sizeof(char));

      memcpy(typvar, buf, strlen(buf));
#ifdef DEBUG
      fprintf(stderr, "setStringVar(char *option), tag: \"%s\", with value = \"%s\" \n", option, (char *)buf);
#endif
    }

  else if(strcmp(option, "etiket") == 0)
    {
      etiket = (char *)calloc(strlen(buf)+1,sizeof(char));

      memcpy(etiket, buf, strlen(buf));

#ifdef DEBUG
      fprintf(stderr, "setStringVar(char *option), tag: \"%s\", with value = \"%s\" \n", option, (char *)buf);
#endif
    }

  else if(strcmp(option, "grtyp") == 0)
    {

      grtyp = (char *)calloc(strlen(buf)+1,sizeof(char));

      memcpy(grtyp, buf, strlen(buf));
#ifdef DEBUG
      fprintf(stderr, "setStringVar(char *option), tag: \"%s\", with value = \"%s\" \n", option, (char *)buf);
#endif
    }

  else if(strcmp(option, "values") == 0)
    {
      values = (char *)calloc(MAX_SIZE,sizeof(char));
      values = memcpy(values, buf, strlen(buf));
#ifdef DEBUG
      fprintf(stderr, "strlen(buf) : %d\n", strlen(buf));
      fprintf(stderr, "setStringVar(char *option), tag: \"%s\", with datatype \"%d\" \n", option, datyp);
#endif
      extract_data(values, datyp);

    }

}


/* -----------------------------------------------------------------------------------------------*/
/* a string including whitespace */
/* munges whitespace sequences */
void ParseString(char *option)
{
  char buf[MAX_SIZE];
  int i = 0;
  unsigned delim = 0;
  Bool waswhite = yes;

  SkipWhite();

#ifdef DEBUG
  fprintf(stderr, "ParseString(char *option), option = \"%s\"\n", option);
#endif

  memset(buf, (int)NULL, MAX_SIZE);
  if (c == '<' || c == '/')
    delim = c;

  while (i <MAX_SIZE  && c != EOF && c != '<')
    {
      /* treat  \r\n   \r  or  \n as line ends */
      if (c == '\r')
        {
    AdvanceChar();

    if (c != '\n' && !IsWhite(c))
      break;
        }

      if (c == '\n')
        {
    AdvanceChar();

    if (!IsWhite(c))
      break;
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
      if(c == '>')
  {
    AdvanceChar();
    if( i > 0)
      break;
  }
      /*New */

      buf[i++] = c;
      AdvanceChar();
    }

  buf[i] = '\0';

  if(strlen(buf) >= 1 && strcmp(buf, "\0") != 0 && strcmp(buf, "    ") != 0)
    {
      setStringVar(option, buf);
    }



#if 0
  if (i == 0)
    ReportBadArgument(option);
#endif

}

/* -----------------------------------------------------------------------------------------------*/
/* a string including whitespace */
/* munges whitespace sequences */
void ParseValues(char *option)
{
  char buf[MAX_SIZE1];
  int i = 0;
  unsigned delim = 0;
  Bool waswhite = yes;

  memset(buf, (int)NULL, MAX_SIZE1);
  SkipWhite();



#ifdef DEBUG
  fprintf(stderr, "ParseValues(char *option), option = \"%s\"\n", option);
  fprintf(stderr, "ParseValues(char *option), before parsing values, nomvar: \"%s\" \n", nomvar);
  fprintf(stderr, "ParseValues(char *option), before parsing values, typvar: \"%s\" \n", typvar);
  fprintf(stderr, "ParseValues(char *option), before parsing values, grtyp: \"%s\" \n", grtyp);
  fprintf(stderr, "ParseValues(char *option), before parsing values, etiket: \"%s\" \n", etiket);
  fprintf(stderr, "ParseValues(char *option), before parsing values, MAX_SIZE: \"%d\" \n", MAX_SIZE);
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
      if(c == '>')
  {
    AdvanceChar();
  }
      /* New */

      buf[i++] = c; /* cumulate charcters into buf */
      AdvanceChar();

    }

  buf[i] = '\0';

  if(i > 0)
    MAX_SIZE = i;

#ifdef DEBUG
  fprintf(stderr, "ParseValues(char *option), tag: \"%s\", value length = \"%d\" \n", option, i);
#endif

  if(strcmp(buf, "") > 0)
    {
      setStringVar(option, buf);
    }

#if 0
    if (i == 0)
      ReportBadArgument(option);
#endif

}

/* -----------------------------------------------------------------------------------------------*/
/* depending on data type, an array of values
  will be extratced from data buffer using space token 
  that separate numbers */
void extract_data(char *buf, int datatype)
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

  if(datatype == INTN)
    {
      intdata = (int *)calloc(MAX_SIZE,sizeof(int));

#ifdef DEBUG
      fprintf(stderr, "extract_data(char *option), parsing int value[%d] = \"%d\"\n", i, atoi(token));
#endif
      intdata[i] = atoi(token);
    }

  else if(datatype == FLOATN || datatype == I3E)
    {
      floatdata = (float *)calloc(MAX_SIZE,sizeof(float));
      sscanf(token, "%f", &(floatdata[i]));

#ifdef DEBUG
      if(i == 0 || i%100 == 0)
  {
    fprintf(stderr, "extract_data(char *option), before parsing values, nomvar: \"%s\" \n", nomvar);
    fprintf(stderr, "extract_data(char *option), parsing float value[%d] = \"%12.4f\"\n", i, floatdata[i]);
  }
#endif

    }

  token = strtok(NULL, delimiters);
  while(token != NULL)
    {
  if (datatype == INTN)
      {
      intdata[++i] = atoi(token);

#ifdef DEBUG
      fprintf(stderr, "extract_data(char *option), parsing int value[%d] = \"%d\"\n", i, intdata[i]);
#endif

      }
    else if(datatype == FLOATN || datatype == I3E)
      {
//       floatdata[++i] = strtof(token, (char **)NULL);
      sscanf(token, "%f", &(floatdata[++i]));


#ifdef DEBUG
      if(i == 0 || i % 100 == 0)
        {
        fprintf(stderr, "extract_data(char *option), before parsing values, nomvar: %s token : %s \n", nomvar, token);
        fprintf(stderr, "extract_data(char *option), parsing float value[%d] = \"%10.4f\"\n", i, floatdata[i]);
        }
#endif

      }
          token = strtok(NULL, delimiters);

    }

}


/* -----------------------------------------------------------------------------------------------*/
/*
For expanding ~/foo or ~your/foo according to $HOME and your
user name. This will only work on Unix systems.
*/
const char *ExpandTilde(const char *filename)
{
  static char *expanded_filename;

  char *home_dir, *p;
  struct passwd *passwd = NULL;

  if (!filename) return(NULL);

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

/* -----------------------------------------------------------------------------------------------*/
static PList *install(char *name, ParseProperty *parser)
{
  PList *np;
  unsigned hashval;

  if ((np = lookup(name)) == NULL)
    {
    np = (PList *)malloc(sizeof(*np));

    if (np == NULL )
        return NULL;

    hashval = hash(name);
    np->name = name;
    np->next = hashtable[hashval];
    hashtable[hashval] = np;
    }

  np->parser = parser;
  return np;
}


/* -----------------------------------------------------------------------------------------------*/
void InitConfig(void)
  {
  struct Flag *p;

  if (!initialized)
    {
    initialized = yes;

    for(p = flags; p->name != NULL; ++p)
      {
      install(p->name, p->parser);
      }
    }

  c = 0;  /* init single char buffer */

  }

/* -----------------------------------------------------------------------------------------------*/
void ParseConfigFile()
{ 
    char name[256];
    const char *fname;
    PList *entry;
    int i;

    InitConfig();
    config_text = NULL;
    AdvanceChar();  /* first char */

    while (c != EOF)
      {

    /* // starts a comment */
      while (c == '/')
        {
        NextProperty();

        if(c == '<')
          {
#ifdef DEBUG
          fprintf(stderr, "ParseConfigFile(), first char found: \"%c\"\n", (char)c);
#endif
          break;
          }
      }

    i = 0;


    if(c == '<' || c == '/' || c == '>') /* New today */
      {
      AdvanceChar();  
      }

    while (c != '>' && c != '<' && c != EOF && i < 200)
      {
      name[i++] = (char)c;
      AdvanceChar();
      }

    name[i] = '\0';

#ifdef DEBUG	  
    if(strstr(name, "           ") == NULL)
      fprintf(stderr, "ParseConfigFile(), tag found: \"%s\" \n", name);
#endif

    if(strlen(name) > 1)
      {
      char *delim = "/";

      if (strstr(name, delim) != NULL && strcmp(name, "/fstrecord") != 0)
        {
          entry = NULL;

          if(strcmp(name, "/rpn-standard-file") != 0) /* Continue if not the last xml closing tag */
            NextProperty();
          else
            exit(0);
        }

      else if(strstr(name, "           ") != NULL) 
        {
        entry = NULL;     /* don't parse white space */        
        }
      else
        {
        entry = lookup(name);
        }
      }

    if (entry)
      {
      if(strcmp(name, "") == 0 || strlen(name) == 1)
        {
        fprintf(stderr, "ParseConfigFile(), found name is empty \n");
        }
      else
        {
        entry->parser(name);
        }

      }
    else
      {

    if(strcmp(name, "/fstrecord") == 0) /* store if data closing tag found */
        {
        write_to_fstFile();

        NextProperty();
        }
        else if(strcmp(name, "/rpn-standard-file") != 0)
          {
            ReportBadArgument(name);
          }
        else if (strcmp(name, " ") == 0)
    NextProperty();
      }
    }

  fclose(stdin);
  }
/* -----------------------------------------------------------------------------------------------*/
void SimpleStats(float *fld, int npts)
{
  int i=0;

  float rmin, rmax, avg;

  avg = 0.0;
  rmin = fld[0];
  rmax = fld[0];
  for (i=0; i < npts; i++)
    {
    avg += fld[i];
    rmin = fld[i] < rmin ? fld[i] : rmin;
    rmax = fld[i] > rmax ? fld[i] : rmax;
    }
  avg /= (1.0 * npts);
  fprintf(stderr, "--- min: %f max: %f avg: %f   ---\n\n\n", rmin, rmax, avg);
}
