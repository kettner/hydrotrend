/*-------------------------------------------------------------------------------------------
 *	hydrocalqsnew.c
 *
 *  Author:    Albert Kettner, March 2006
 *
 *	Subroutine to compensate for the difference between long term suspended sediment load (ART, QRT)
 *	and the sum of the daily suspended sediment load. 
 *
 *  Variable	Def.Location	Type	Units	Usage
 *  --------	------------	----	-----	-----
 *  err			various			int		-		error flag, halts program
 *  p			various			int		-		counter for the outlets  
 *  Qsgrandtotaloutlettot Hydrocalqsnew.c double kg/s total Qs of all outlets
 *
 *-------------------------------------------------------------------------------------------*/

#include <stdio.h>
#include "hydroparams.h"
#include "hydrotimeser.h"
#include "hydroclimate.h"
#include "hydroinout.h"

/*----------------------
 *  Start main program
 *----------------------*/
int
hydrocalqsnew (void)
{

/*-------------------
 *  Local Variables
 *-------------------*/
  int err, p;
  double Qsgrandtotaloutlettot;

/*------------------------
 *  Initialize variables
 *------------------------*/
  err = 0;
  Qsgrandtotaloutlettot = 0.0;

/*--------------------------------------------------
 *  Calculate mean Qs and calculate the difference
 *  between mean Qs and (Qsbar+ glacier part).
 *--------------------------------------------------*/
  if (Qsglacierbar[ep] > 0.0)
    {
      Qsmean[ep] = Qspsigrandtotal[ep] / (nyears[ep] * daysiy * dTOs);
      Qsbarnew[ep] =
        ((1.0 - sedfilter[ep]) * (Qsbartot[ep] +
                                  (fractionglaciersediment[ep] *
                                   Qsglacierbar[ep]))) / Qsmean[ep];
    }

  else
    {
      Qsmean[ep] = Qsgrandtotal[ep] / (nyears[ep] * daysiy * dTOs);
      Qsbarnew[ep] = ((1.0 - sedfilter[ep]) * (Qsbartot[ep])) / Qsmean[ep];
    }
  return (err);
}                               /* end of Hydrocalqsnew */
