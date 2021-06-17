#include <rpnmacros.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

int f77name(findlowcoreindex)(float ax[], int *ni);
int f77name(findhighcoreindex)(float ax[], int *ni);

int c_findLowCoreIndex(float ax[], int ni);
int c_findHighCoreIndex(float ax[], int ni);

int f77name(findlowcoreindex)(float ax[], int *ni)
{
  int lc;
  lc =  c_findLowCoreIndex(ax, *ni);
  return lc+1;
}

int f77name(findhighcoreindex)(float ax[], int *ni)
{
  int hc;
  hc = c_findHighCoreIndex(ax, *ni);
  return hc+1;
}


int c_findLowCoreIndex(float ax[], int ni)
{
  int is,isref;
  float dx, dxref;
  
  is = ni/2;
  dxref = ax[is] - ax[is-1];
  dx = ax[is-1] - ax[is-2];
  is--;
  while (((fabs(dxref-dx)) < (0.001 * dxref)) && (is > 0))
    {
    is--;
    dx = ax[is] - ax[is-1];
    }
  if (is == 1) is = 0;
  return is;
}

int c_findHighCoreIndex(float ax[], int ni)
{
  int is,isref;
  float dx, dxref;

  is = ni/2;
  dxref = ax[is+1] - ax[is];
  dx = ax[is+2] - ax[is+1];
  is++;
  while (((fabs(dxref-dx)) < (0.001 * dxref)) && (is < (ni-1)))
    {
    is++;
    dx = ax[is+1] - ax[is];
    }
  if (is == (ni-2)) is=ni-1;
  return is;
}

