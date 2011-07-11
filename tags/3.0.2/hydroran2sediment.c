/*-------------------------------------------------------------------------------------------
 *  hydroran2sediment.c
 *
 *	Author: Albert Kettner, March 2006
 *
 *  This program contance a couple of functions, hydroran2 till hydroran5. The functions are
 *	the same but made to generate exactly the same numbers for every epoch when it goes 4
 *	times through the loop in hydrotrend.c. (Once for calculating Qbar, second time to
 *	calculate Qs e.d.
 *
 *  Generates uniformly distributed numbers between 0.0 and 1.0.
 *	ran2.c From: "Numerical Recipes in C", p282, 2nd ed.
 *
 *  For the first deviate, use make sure the seed is negative; this initializes ran2
 *	appropriately. For subsequent years, use the generated seed; idum should not
 *  be altered between successive deviates in a sequence.
 *
 *	Variable		Def.Location	Type		Units	Usage
 *	--------		------------	----		-----	-----
 *
 *-------------------------------------------------------------------------------------------*/

#include <math.h>
#include "hydroparams.h"
  
#define	IM1	2147483563
#define	IM2	2147483399
#define	AM	(1.0/IM1)
#define	IMM1	(IM1-1)
#define	IA1	40014
#define	IA2	40692
#define	IQ1	53668
#define	IQ2	52774
#define	IR1	12211
#define	IR2	3791
#define	NTAB	32
#define	NDIV	(1+IMM1/NTAB)
#define	EPS	1.2e-7
#define	RNMX	(1.0-EPS)
  
#include <stdio.h>
  
/*------------------------
 *  Start of HydroRan2sediment.c
 *------------------------*/ 
  float
hydroran2sediment (long *idum) 
{
  
/*-------------------
 *  Local Variables
 *-------------------*/ 
  int jj;
  
  
  
  
  
  
/*----------------------------
 *  Initialize the generator
 *----------------------------*/ 
    if (*idum <= 0)
    {
      
        *idum = 1;              /* prevent idum = 0 */
      
      else
        *idum = -(*idum);
      
      
        {                       /* load the shuffle table */
          
          
          
            *idum += IM1;
          
            iv[jj] = *idum;
        
      
    
  
/*-----------------------
 *  Start the generator
 *-----------------------*/ 
    kk = (*idum) / IQ1;
  
  
    *idum += IM1;
  
  
  
    idum2 += IM2;
  
  
  
  
    iy += IMM1;
  
    return RNMX;
  
  else
    return temp;


