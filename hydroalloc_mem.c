/*-------------------------------------------------------------------------------------------
 *	hydroalloc_mem.c
 *	
 *	Author: Albert Kettner, March 2006
 * 
 *	Allocate memory for 1D, 2D, 3D for any variable: int, fload, double, struct, etc!
 *	Function definitions are in hydroalloc_mem.h
 * 
 *	Variable		Def.Location	Type		Units	Usage
 *	--------		------------	----		-----	-----
 *	
 *-------------------------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#ifdef WITH_UNISTD
# include <unistd.h>
#endif
#include "hydroalloc_mem.h"
  
/*      FUNCTION ALLOCATE_1D FILE */ 
  FILE ** allocate_1d_F (int nrows)
{
  FILE ** i;
  i = (FILE **) malloc (nrows * sizeof (FILE *));
  if (!i)
    {
      perror ("allocate_1d_F");
      exit (-1);
    }
  return i;
}


/*------------------------------------------------------
 *	...for allocating a 1D "matrix" of whatever!
 *	
 *	note that sizeof(void*) is the same as sizeof(double*)
 *	is the same as sizeof(float*), etc. A pointer always
 *	has the same size (the size of the memory address.)
 *------------------------------------------------------*/ 
void *
matrixalloc1D (int max_width, long size) 
{
  void *atemp;
  atemp = (void *) malloc (max_width * size);
  if (!atemp)
    {
      perror ("matrixalloc");
      sleep (5);
      exit (1);
    }
  return atemp;
}


/*------------------------------------------------------
 *	...for allocating a 2D matrix of whatever!
 *	
 *	note that sizeof(void*) is the same as sizeof(double*)
 *	is the same as sizeof(float*), etc. A pointer always
 *	has the same size (the size of the memory address.)
 *------------------------------------------------------*/ 
void **
matrixalloc2D (int max_width, int max_length, long size) 
{
  int i;
  void **atemp;
  atemp = (void **) malloc (max_width * sizeof (void *));
  if (!atemp)
    {
      perror ("matrixalloc");
      sleep (5);
      exit (1);
    }
  for (i = 0; i < max_width; ++i)
    {
      atemp[i] = (void *) malloc (max_length * size);
      if (!atemp[i])
        {
          perror ("matrixalloc");
          sleep (5);
          exit (1);
        }
    }
  return atemp;
}


/*------------------------------------------------------
 *	...for allocating a 3D matrix of whatever!
 *	
 *	note that sizeof(void*) is the same as sizeof(double*)
 *	is the same as sizeof(float*), etc. A pointer always
 *	has the same size (the size of the memory address.)
 *------------------------------------------------------*/ 
void ***
matrixalloc3D (int max_width, int max_length, int max_height, long size) 
{
  int i, j;
  void ***atemp;
  atemp = (void ***) malloc (max_width * sizeof (void *));
  if (!atemp)
    {
      perror ("matrixalloc");
      sleep (5);
      exit (1);
    }
  for (i = 0; i < max_width; ++i)
    {
      atemp[i] = (void **) malloc (max_length * sizeof (void *));
      if (!atemp[i])
        {
          perror ("matrixalloc");
          sleep (5);
          exit (1);
        }
    }
  for (i = 0; i < max_width; ++i)
    for (j = 0; j < max_length; ++j)
      {
        atemp[i][j] = (void *) malloc (max_height * size);
        if (!atemp[i][j])
          {
            perror ("matrixalloc");
            sleep (5);
            exit (1);
          }
      }
  return atemp;
}


