/*-------------------------------------------------------------------------------------------
 *	hydrofree_mem.h
 *
 *	Author: Albert Kettner, March 2006
 *
 *	Function declaration to free allocated memory.
 *
 *	Variable		Def.Location	Type		Units	Usage
 *	--------		------------	----		-----	-----
 *
 *-------------------------------------------------------------------------------------------*/
   
#ifndef HYDROFREE_MEM_H_
#define HYDROFREE_MEM_H_
#include <stdio.h>
  
/*      FUNCTION DEFINITIONS */ 
void freematrix1D (void *);
void freematrix2D (void **, int);
void freematrix3D (void ***, int, int);

#endif  /*  */
