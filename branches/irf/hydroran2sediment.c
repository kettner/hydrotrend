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
  long kk;
  static long idum2 = 123456789;
  static long iy = 0;
  static long iv[NTAB];
  float temp;
  
/*----------------------------
 *  Initialize the generator
 *----------------------------*/ 
    if (*idum <= 0)
    {
      if (-(*idum) < 1)
        *idum = 1;              /* prevent idum = 0 */
      
      else
        *idum = -(*idum);
      idum2 = (*idum);
      for (jj = NTAB + 7; jj >= 0; jj--)
        {                       /* load the shuffle table */
          kk = (*idum) / IQ1;
          *idum = IA1 * (*idum - kk * IQ1) - kk * IR1;
          if (*idum < 0)
            *idum += IM1;
          if (jj < NTAB)
            iv[jj] = *idum;
        }
      iy = iv[0];
    }
  
/*-----------------------
 *  Start the generator
 *-----------------------*/ 
    kk = (*idum) / IQ1;
  *idum = IA1 * (*idum - kk * IQ1) - kk * IR1;
  if (*idum < 0)
    *idum += IM1;
  kk = idum2 / IQ2;
  idum2 = IA2 * (idum2 - kk * IQ2) - kk * IR2;
  if (idum2 < 0)
    idum2 += IM2;
  jj = iy / NDIV;
  iy = iv[jj] - idum2;
  iv[jj] = *idum;
  if (iy < 1)
    iy += IMM1;
  if ((temp = AM * iy) > RNMX)
    return RNMX;
  
  else
    return temp;
}                             /* end of HydroRan2Sediment.c */


