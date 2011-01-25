/*-------------------------------------------------------------------------------------------
 *	hydroshuffle.c
 *
 *	Author: Albert Kettner, March 2006
 *
 *	Randomly shuffles the days of the month for climate simulations
 *
 * Variable		Def.Location	Type	Units	Usage
 * --------		------------	----	-----	-----
 * dumflt		HydroShuffle.c	float	-		temporary float
 * dvals[31]	HydroShuffle.c	int		-		shuffled array of daily index values
 * err			HydroShuffle.c	int		-		error flag, halts program
 * ii			HydroShuffle.c	int		-		temporary loop counter
 * mnth			HydroShuffle.c	int		-		month of the year
 * yy			HydroShuffle.c	int		-		temporary integer
 * zz			HydroShuffle.c	int		-		temporary integer
 *
 *-------------------------------------------------------------------------------------------*/

#include "hydroclimate.h"
#include "hydroparams.h"
#include "hydrodaysmonths.h"
#include "hydrornseeds.h"
#define ntot (93)

/*---------------------------
 *  Start of HydroShuffle.c
 *---------------------------*/
int
hydroshuffle (int dvals[31], int mnth)
{

  float hydroran3 (long *idum);
  float dumflt;
  double dummy_double;
  int yy, zz, ii, err;

/*------------------------
 *  Initialize variables
 *------------------------*/
  err = 0;
  for (ii = 0; ii < daysim[mnth]; ii++)
    dvals[ii] = ii + 1;

/*---------------------------------
 *  shuffle the days of the month
 *---------------------------------*/
  if (yr == syear[ep] && mnth == 0)
    rnseed3 = -INIT_RAN_NUM_SEED;

  for (ii = 0; ii < daysim[mnth]; ii++)
    {
      dumflt = hydroran3 (&rnseed3);    /* get a uniform random number [0:1] */
      if (0 > dumflt || dumflt > 1)
        {
          fprintf (stderr,
                   "A function in HydroRan2 failed in HydroShuffle.c \n");
          fprintf (stderr,
                   " \t dumflt = %f: \t setting value to 0.5, ii = %d \n",
                   dumflt, ii);
          dumflt = 0.5;
        }

      dummy_double = dumflt * (float) daysim[mnth];
      yy = (int) rnd (dummy_double);    /* scale to a random day */
      if (yy == 0)
        yy += 1;
      zz = dvals[yy - 1];       /* swap random day with day ii */
      dvals[yy - 1] = dvals[ii];
      dvals[ii] = zz;
    }

  return (err);
}                               /* end of HydroShuffle.c */
