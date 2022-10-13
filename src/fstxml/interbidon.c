#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <rmn.h>
extern void f77name(fst2xml) ();

ftnword f77name(c_to_f_sw)(char *program, int *f_lng)
{
    int ier;
    int lng = *f_lng;
    char *n_program;
    n_program = malloc(lng);
    strncpy(n_program, program, lng); 
    if ( (n_program == "null" || n_program == " " || lng == 0) && ( 1 != 1 ) ){
        fprintf(stdout,"erreur 1: aucun argument specifie\n");
        exit(1);
    } else if ( n_program == "-h" ){
        fprintf(stdout,"programmes reconnus: fst2xml \n");
    } else if ( 0 == 0 ){
        f77name(fst2xml) () ;
    } else{
        fprintf(stdout,"%s : erreur 2: pas dans la liste des programmes connus\n",n_program);
        fprintf(stdout,"Programmes reconnus: fst2xml\n");
        exit(2);
    }
    ier = 0;
    free(n_program);
    return ((ftnword) ier);
}

