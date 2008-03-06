/*-------------------------------------------------------------------------------------------
 *	hydrorandom.c
 *
 *	Author: Albert Kettner, March 2006
 *
 *  Generates a large array of normally distributed deviates with zero mean and unit variance.
 *	Other routines sequentially pluck numbers from this array. The numbers are stored in
 *	ranarray, and indexed by nran.
 *
 *  Variable	Def.Location	Type	Units	Usage
 *  --------	------------	----	-----	-----
 *  dumlong	HydroRandom.c	long	-	random number generator variable
 *  err		various		int	-	error flag, halts program
 *  fac		HydroRandom.c	float	-	random number generator variable
 *  ii		various		int	-	temporary loop counter
 *  jj		various		int	-	temporary loop counter
 *  rmin	HydroRandom.c	double  -	random number generator stats
 *  rmax	HydroRandom.c	double  -	random number generator stats
 *  rsum	HydroRandom.c	double  -	random number generator stats
 *  rsq		HydroRandom.c	float	-	random number generator variable	
 *  unival[]	HydroRandom.c	float	-	random number generator variable
 *  v1		HydroRandom.c	float	-	random number generator variable
 *  v2		HydroRandom.c	float	-	random number generator variable
 *
 *-------------------------------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include "hydroinout.h"
#include "hydroparams.h"
#include "hydroalloc_mem.h"
#include "hydrofree_mem.h"
#include "hydrornseeds.h"

/*------------------------
 *  Start of HydroRandom
 *------------------------*/
int hydrorandom()
{

/*-------------------
 *  Local Variables
 *-------------------*/
float hydroran2(long *idum);
float fac, rsq, v1, v2, *unival;
double rsum;
int err, ii, jj;

/*------------------------
 *  Initialize Variables
 *------------------------*/
nran = 0;
err = 0;

/*--------------------------------------------------------------------
 *  First generate a set of uniform random numbers in [0.0, 1.0].
 *  ran2 is from "Numerical Recipes in C", p282, 2nd ed.
 *  For the first deviate, use rnseed as the seed, making sure that
 *  it is negative, this initializes ran2 appropriately.
 *  For subsequent years, use the generated seed; dumlong should not
 *  be altered between successive deviates in a sequence.
 *--------------------------------------------------------------------*/
if (yr == syear[ep] )
    rnseed = -INIT_RAN_NUM_SEED - 10 * ep;
unival = malloc1d( 2*maxran, float );
for (ii=0; ii<2*maxran; ii++)
    unival[ii] = hydroran2(&rnseed);

/*---------------------------------------------------------
 *  Next generate Gaussian distributed deviates.
 *  The routine returns two random numbers for each pass,
 *  so loop through the array at a step of 2.
 *  GASDEV, From "Numerical Recipes in C", p.289, 2nd ed.
 *---------------------------------------------------------*/
jj = 0;
for (ii=0; ii<maxran-1; ii+=2){
   do {
      v1 = 2.0 * unival[jj]   - 1.0;
      v2 = 2.0 * unival[jj+1] - 1.0;
      rsq = sq(v1) + sq(v2);
      jj+=2;
   } while( rsq >= 1.0 || rsq == 0.0 );
   fac = sqrt(-2.0*log(rsq)/rsq);
   ranarray[ii]   = (double)v1*fac;
   ranarray[ii+1] = (double)v2*fac;
}

/*-------------------
 *  Check the stats
 *-------------------*/
rmin = 0;
rmax = 0;
rsum = 0;
for (ii=0; ii<maxran-1; ii+=2) {
    rmin = mn(rmin, ranarray[ii]);
    rmax = mx(rmax, ranarray[ii]);
    rsum += ranarray[ii];
}
freematrix1D( (void*) unival );
return(err);
}  /* end of HydroRandom */


