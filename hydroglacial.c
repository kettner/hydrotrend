/*-------------------------------------------------------------------------------------------
 *  hydroglacial.c
 *
 *	Author: Albert Kettner, March 2006
 *
 *  Calculates the daily ice accumulation or melt. Also calculates the groundwater flow and
 *	time lag when glaciers are melting. And the sediment produced only by glaciers.
 *
 *  Variable	Def.Location	Type	Units	Usage
 *  --------	------------	----	-----	-----
 *  Mgw		HydroGlacial.c	double m^3/a Annual mass of Ice discharge going to GW
 * Mice		HydroGlacial.c	double	m^3/a	Annual mass of Ice derived discharge
 * Minput	HydroGlacial.c	double	m^3/a	Annual mass input into the Glacial routine
 * Mout		HydroGlacial.c	double	m^3/a	Annual mass output from the Glacial routine
 * Mwrap	HydroGlacial.c	double	m^3/a	Annual mass of discharge carried over from the previous year
 * Parea	HydroGlacial.c	double	m^2	daily area over which "ice" precipitation occurs
 * Tcorrection	HydroGlacial.c	double	degC	Temperature correction to ice melt applied for days with rain
 * Tfix		HydroGlacial.c	double	degC	Temperature correction for cold years
 * approxarea	HydroGlacial.c	double	m^2	the elevation binned area for the glacier
 * elabin	HydroGlacial.c	double	-	the closest elevbin to the ela
 * elaerror	HydroGlacial.c	double	m	the height difference between the actual and binned ela
 * err		various		int	-	error flag, halts program
 * glacierind	HydroGlacial.c	int	-	the elevbin index at the toe of the glacier
 * ii		various		int	-	temporary loop counter
 * indx		HydroGlacial.c	int	-	temporary elevbin index
 * jj		various		int	-	temporary loop counter
 * kk		various		int	-	temporary loop counter
 * massavailable	HydroGlacial.c	double	m^3	total water available for E,Q and Gw=sum(ice balance+Precip)
 * maxmelt	HydroGlacial.c	double	degC	warmest melt day, used to prevent oversized floods
 * meltday[maxday]HydroGlacial.c	double	m^3	the amount of ice melt occuring on a given day
 * meltflag	HydroGlacial.c	int	-	flag to check for good ice melt years
 * shldday[maxday]HydroGlacial.c	double	m^3	the discharge going into shoulder events
 * smallgapprox	HydroGlacial.c	double	m^2	binned glaciated area above the elabin
 * totalmelt	HydroGlacial.c	double	m^3	scale factor for distributing the melt events
 *
 *-------------------------------------------------------------------------------------------*/

#include "hydroclimate.h"
#include "hydroparams.h"
#include "hydrotimeser.h"
#include "hydroinout.h"
#include "hydrodaysmonths.h"

/*-------------------------
 *  Start of HydroGlacial
 *-------------------------*/
int
hydroglacial ()
{

        /*-------------------
	 *  Local Variables
	 *-------------------*/
  int err, ii, jj, kk, indx, glacierind, meltflag;
  int dumint;
  double elabin, elaerror, approxarea, Parea;
  double massavailable, maxmelt, meltday[maxday], shldday[maxday];
  double smallgapprox;
  double totalmelt, Tcorrection, Tfix, Mice, Mgw, Mout, Mwrap, Minput;
  double Tmean;
  double Volumelast;
  double lastareakm, glacierareakm;
  double yieldQsbar, yieldnoglaciers, yielddifference, yield;
  double correctedVolumeglacierarea;
  err = 0;
  glacierind = 0;
  elabin = 0.0;
  Tmean = 0.0;

        /*---------------------------------------------------------
	 *  Approximate the ELA and glaciated area for the
	 *  year prior to the model run.
	 *  If (floodtry > 0) then keep that lastela and lastarea
	 *  from the first time through
	 *---------------------------------------------------------*/
  if (floodtry == 0)
    {
      if (ep == 0 && yr == syear[ep])
        {

                        /*----------------------------------------
			 *  Find the closest elevbin to the ela;
			 *  this may be above or below the ela
			 *----------------------------------------*/
          if (ELAstart[ep] > maxalt)
            {
              lastela = ELAstart[ep];
              elaerror = 0.0;
              elabin = ELAstart[ep];
              ELAindex = 2 * nelevbins;
              smallg = 0.0;
              bigg = 0.0;
              lastarea = 0.0;
            }
          else
            {
              lastela = ELAstart[ep];
              elaerror = maxalt;
              for (kk = 0; kk < nelevbins; kk++)
                if (fabs (lastela - elevbins[kk]) < elaerror)
                  {
                    elabin = elevbins[kk];
                    ELAindex = kk;
                    elaerror = fabs (lastela - elevbins[kk]);
                  }

                                /*-------------------------------------------------
				 *  Calculate the glaciated area above the elabin
				 *  by summing the areas above that point
				 *-------------------------------------------------*/
              smallgapprox = 0.0;
              for (kk = ELAindex; kk < nelevbins; kk++)
                smallgapprox += areabins[kk];

                                /*---------------------------------------------------
				 *  Determine which bin the ela actually resides in
				 *---------------------------------------------------*/
              if (elabin > lastela)
                indx = ELAindex - 1;
              else
                indx = ELAindex;

                                /*--------------------------------------------
				 *  Correct the area by linear interpolation
				 *--------------------------------------------*/
              smallg =
                smallgapprox + areabins[indx] * (elabin -
                                                 lastela) / elevbinsize;

                                /*--------------------------------------------------------
				 *  Calculate the glaciated area below the ela
				 *  Assume area below ela = 35% of total area
				 *  G = 0.35*Atotal	 g = 0.65*Atotal	G = (0.35/0.65)*g
				 *--------------------------------------------------------*/
              bigg = (0.35 / 0.65) * smallg;

                                /*------------------------------------
				 *  Find the estimate glaciated area
				 *------------------------------------*/
              lastarea = smallg + bigg;
            }                   /* end if ELA > maxalt */
        }                       /* end if ep == 0 %% yr == syear[ep]

                                   /*-----------------------------------------------------------------
                                   *  Keep last years Glaciated area for Mass Balance and Discharge
                                   *----------------------------------------------------------------- */
      if (ep != 0)
        {
          if (yr == syear[ep] && setstartmeanQandQs == 0)
            {
              initiallastela = ela;
              initiallastarea = glacierarea;
              initialVolumelast = Volumeglacierarea;
              lastela = ela;
              lastarea = glacierarea;
              Volumelast = initialVolumelast;
            }
          else if (yr == syear[ep] && setstartmeanQandQs > 0)
            {
              lastela = initiallastela;
              lastarea = initiallastarea;
              Volumelast = initialVolumelast;
            }
        }
      if (yr != syear[ep])
        {
          lastela = ela;
          lastarea = glacierarea;
          Volumelast = Volumeglacierarea;
        }
    }                           /* endif floodtry==0 */


        /*-------------------------
	 *  Calculate the new ELA
	 *-------------------------*/
  ela = ELAstart[ep] + ELAchange[ep] * (1 + (yr - syear[ep]));
        /*-------------------------------------
	 *  Simulate a basin with NO glaciers
	 *-------------------------------------*/
  if (ela > maxalt)
    {
      if (lastela < maxalt)
        {
          fprintf (stderr, "\nHydroGlacial WARNING: epoch = %d, year = %d \n",
                   ep + 1, yr);
          fprintf (stderr, "   The Glacier completely melted. \n");
          fprintf (stderr, "   This has not been accounted for yet. \n");
          fprintf (stderr, "   There will be a mass balance error for \n");
          fprintf (stderr, "   the remaining part of the glacier. \n");
        }
      glacierelev = maxalt + elevbinsize;       /* make sure it is above the basin */
      glacierarea = 0.0;
      approxarea = 0.0;
      bigg = 0.0;
      smallg = 0.0;
      smallgapprox = 0.0;
      if (setstartmeanQandQs == 0)
        for (ii = 0; ii < daysiy; ii++)
          Qsglacier[ii] = 0.0;
    }

        /*----------------------------------
	 *  Simulate a basin with glaciers
	 *----------------------------------*/
  else
    {
      for (ii = 0; ii < maxday; ii++)
        {
          meltday[ii] = 0.0;
          shldday[ii] = 0.0;
        }

                /*----------------------------------------
		 *  Find the closest elevbin to the ela;
		 *  this may be above or below the ela
		 *----------------------------------------*/
      elaerror = maxalt;
      for (kk = 0; kk < nelevbins; kk++)
        if (fabs (ela - elevbins[kk]) < elaerror)
          {
            elabin = elevbins[kk];
            ELAindex = kk;
            elaerror = fabs (ela - elevbins[kk]);
          }

                /*--------------------------------------------------
		 *  Calculate the glaciated area above the elabin
		 *  by summing the areas above that point
		 *--------------------------------------------------*/
      smallgapprox = 0.0;
      for (kk = ELAindex; kk < nelevbins; kk++)
        smallgapprox += areabins[kk];

                /*---------------------------------------------------
		 *  Determine which bin the ela actually resides in
		 *---------------------------------------------------*/
      if (elabin > ela)
        indx = ELAindex - 1;
      else
        indx = ELAindex;

                /*--------------------------------------------
		 *  Correct the area by linear interpolation
		 *--------------------------------------------*/
      smallg = smallgapprox + areabins[indx] * (elabin - ela) / elevbinsize;

                /*--------------------------------------------------------
		 *  Calculate the glaciated area below the ela
		 *  Assume area below ela = 35% of total area
		 *  G = 0.35*Atotal	 g = 0.65*Atotal	G = (0.35/0.65)*g
		 *--------------------------------------------------------*/
      bigg = (0.35 / 0.65) * smallg;

                /*--------------------------------------------------
		 *  Find the actual glaciated area (glacierarea)
		 *  and elevation of the glacier toe (glacierelev)
		 *  and glacierelev's elevbins index (glacierind)
		 *--------------------------------------------------*/
      glacierarea = smallg + bigg;
      approxarea = 0.0;
      kk = nelevbins - 1;
      while (approxarea <= glacierarea)
        {
          approxarea += areabins[kk];
          glacierelev = elevbins[kk];
          glacierind = kk;
          kk--;
        }

                /*---------------------------------------------------------------------------
		 *  Sum the Precip which is above BOTH:
		 *  i) the ELA
		 *  ii) the Freezing line altitude (FLAindex)
		 *
		 *  if( FLAindex == FLAflag ) then no T<0 occured on that day at any elev.
		 *---------------------------------------------------------------------------*/
      MPglacial = 0.0;
      for (ii = 0; ii < daysiy; ii++)
        {
          /*  Find area above the ELA */
          if (FLAindex[ii] < ELAindex)
            Parea = smallgapprox;
          /* Find area above the FLA */
          else if (FLAindex[ii] < FLAflag)
            {
              Parea = 0.0;
              for (kk = nelevbins - 1; kk >= FLAindex[ii]; kk--)
                Parea += areabins[kk];
            }
          else
            Parea = 0.0;        /* FLA is above the basin */
          MPglacial += Pdaily[ii] * Parea;
        }

                /*---------------------------------------------------------
		 *  Track the actual changes in glacier mass so there are
		 *  no step changes in mass between years
		 *---------------------------------------------------------*/
      lastareakm = (lastarea / 1e6);
      glacierareakm = (glacierarea / 1e6);
      if (yr == syear[ep] && ep == 0)
        {
          Volumelast = bethaglacier * pow (lastareakm, bethaexpo);
        }                       /* you might need to put a end run value here for the next HT run */
      Volumeglacierarea = bethaglacier * pow (glacierareakm, bethaexpo);
      Gmass = ((Volumelast * 1e6) - (Volumeglacierarea * 1e6));

                /*--------------------------------------------------------
		 *  Calculate the total water available for E, Q, and Gw
		 *  = sum( ice balance + Precip in )
		 *--------------------------------------------------------*/
      massavailable = Gmass + MPglacial;
      glacierareakmreset = glacierareakm;
      glacierareakmpotential = glacierareakm;
      if (massavailable < 0.0)
        {
          massavailable = 0.2 * MPglacial;
          correctedVolumeglacierarea =
            ((0.8 * MPglacial) + (Volumelast * 1e6)) / 1e6;
          Gmass = -0.8 * MPglacial;
          if (setstartmeanQandQs == 4)
            {
              fprintf (stderr, "HydroGlacial WARNING: year = %d, ep =%d \n", yr,
                       ep);
              fprintf (stderr,
                       " \t Insufficient precipitation to grow glacier. \n");
              fprintf (stderr, " \t Volume glacier correction: \n");
              fprintf (stderr, " \t Volume should be \t%e m^3\n",
                       Volumeglacierarea * 1e6);
              fprintf (stderr, " \t Volume corrected to \t%e m^3\n",
                       correctedVolumeglacierarea * 1e6);
              fprintf (stderr, " \t The rest is carried over to next year\n\n");
              if (ep == (nepochs - 1) && yr == (syear[ep] + (nyears[ep] - 1)))
                fprintf (stderr, "\t Volume of glacier at last year = %f \n",
                         correctedVolumeglacierarea);

              fprintf (fidlog, "HydroGlacial WARNING: year = %d, ep =%d \n", yr,
                       ep);
              fprintf (fidlog,
                       " \t Insufficient precipitation to grow glacier. \n");
              fprintf (fidlog, " \t Volume glacier correction: \n");
              fprintf (fidlog, " \t Volume should be \t%e m^3\n",
                       Volumeglacierarea * 1e6);
              fprintf (fidlog, " \t Volume corrected to \t%e m^3\n",
                       correctedVolumeglacierarea * 1e6);
              fprintf (fidlog, " \t The rest is carried over to next year \n");
            }
          Volumeglacierarea = correctedVolumeglacierarea;
          glacierareakmreset =
            pow ((Volumeglacierarea / bethaglacier), (1 / bethaexpo));
          glacierarea = glacierareakmreset * 1e6;
        }

                /*-----------------------------------------------------------------------
		 *  Calculate the amount of ice lost to evaporation
		 *  divide by the glaciated area, to get units of m of water equivelant
		 *-----------------------------------------------------------------------*/
      Eiceannual = massavailable * dryevap[ep] / glacierarea;

                /*------------------------------------------------------------------
		 *  Loop through the year and determine the melt days
		 *  scale each event by the Temperature and Precip
		 *  Later the scaled days will be assigned the appropriate
		 *  amount of runoff
		 *
		 *  Also check to insure that we melt enough ice, and do not flood
		 *  the basin.  Note that meltday will be scaled down further
		 *  by the shoulder events.
		 *
		 *  The ranarray factor will add some randomness to the events
		 *  ranarry has a mean of zero and std of 1
		 *------------------------------------------------------------------*/
      meltflag = 1;
      maxmelt = 0.0;
      Tfix = 0.0;
      while (meltflag == 1)
        {
          totalmelt = 0.0;
          for (ii = 0; ii < daysiy; ii++)
            {
              Tcorrection = 0.0;
              if (Pdaily[ii] > 0.0)
                Tcorrection = 1.0;
              if (Televday[glacierind][ii] + Tfix > 0.0)
                {
                  meltday[ii] =
                    mx (Televday[glacierind][ii] + ranarray[nran] -
                        Tcorrection + Tfix, 0.0);
                  nran++;
                  totalmelt += meltday[ii];
                  maxmelt = mx (meltday[ii], maxmelt);
                }
            }
          if (totalmelt == 0.0)
            {
              Tmean = 0.0;
              for (ii = daystrm[5]; ii < dayendm[7]; ii++)
                Tmean += Televday[glacierind][ii];
              Tmean /= (dayendm[7] - daystrm[5]);
              Tfix += mx (-Tmean, 1.0);
            }
          else
            meltflag = 0;
        }

      if (Tfix > 0.0)
        {
          fprintf (stderr, "\n HydroGlacial Warning: epoch = %d, year = %d \n",
                   ep + 1, yr);
          fprintf (stderr,
                   " \t The basin was too cold to melt enough glacial ice. \n");
          fprintf (stderr,
                   " \t The daily temperatures used to melt ice were increased. \n");
          fprintf (stderr, " \t Tfix = %f (degC) \n", Tfix);
          fprintf (stderr, " \t Tmean = %f (degC) \n", Tmean);
          fprintf (fidlog, " HydroGlacial Warning: \n");
          fprintf (fidlog,
                   " \t The basin was too cold to melt enough glacial ice. \n");
          fprintf (fidlog,
                   " \t The daily temperatures used to melt ice were increased. \n");
          fprintf (fidlog, " \t Tfix = %f (degC) \n", Tfix);
          fprintf (fidlog, " \t Tmean = %f (degC) \n", Tmean);
        }

                /*---------------------------------------------------------------------------
		 *  Create the shoulder events (Murray's version of flood wave attenuation)
		 *  there is one left (preceeding) day scaled as:
		 *   shoulderleft*event
		 *  the main event is scaled down to:
		 *   shouldermain*event
		 *  there are 1 or more right (following days) scaled to:
		 *   shoulderright[]*event
		 *  1.0 = Sum(shoulderleft+shouldermain+shoulderright[])
		 *---------------------------------------------------------------------------*/
      ii = 0;
      if (meltday[ii] > 0.0)
        {
          shldday[ii] += shoulderleft * meltday[ii];
          for (jj = 0; jj < shouldern - 2; jj++)
            shldday[ii + jj + 1] += shoulderright[jj] * meltday[ii];
          meltday[ii] = shouldermain * meltday[ii];
        }

      for (ii = 1; ii < daysiy + 1; ii++)
        {
          Qice[ii - 1] = 0.0;
          if (meltday[ii] > 0.0)
            {
              shldday[ii - 1] += shoulderleft * meltday[ii];
              for (jj = 0; jj < shouldern - 2; jj++)
                shldday[ii + jj + 1] += shoulderright[jj] * meltday[ii];
              meltday[ii] = shouldermain * meltday[ii];
            }
        }

                /*-----------------------------------------------------------
		 *  Add the shoulder events and the main events
		 *  to get the total ice derived discharge.
		 *  Also scale the discharges to match the actual ice melt.
		 *  Convert to m^3/s
		 *-----------------------------------------------------------*/
      for (ii = 0; ii < maxday - distbins[ELAindex]; ii++)
        {

                        /*---------------------------------------------------------------------
			 *  (Mark's version of routing)
			 *  Add the time lag for the distance up the basin (distbins[elabin])
			 *  Convert to m^3/s
			 *---------------------------------------------------------------------*/
          dumint = distbins[ELAindex];
          Qice[ii + dumint] += (meltday[ii] + shldday[ii])
            * (massavailable - Eiceannual * glacierarea) / (totalmelt * dTOs);
        }

                /*---------------------------------------
		 *  Add the carryover from the previous
		 *  year and track it's mass
		 *---------------------------------------*/
      Mwrap = 0.0;
      for (ii = 0; ii < maxday - daysiy; ii++)
        {
          Qice[ii] += Qicewrap[ii];
          Mwrap += Qicewrap[ii] * dTOs;
        }

                /*---------------------------------------------------------
		 *  Add to the flux to the Groundwater pool
		 *  Actual addition to the GW pool is done in HydroRain.c
		 *---------------------------------------------------------*/
      for (ii = 0; ii < daysiy; ii++)
        {
          Qicetogw[ii] += percentgw * Qice[ii];
          Qice[ii] -= Qicetogw[ii];
        }

                /*-------------------------------------------------------------
		 *  Calculate total Qice per epoch for distributing suspended
		 *  sediment created by glaciers. Distribution is done in
		 *  hydrosedload.c
		 *-------------------------------------------------------------*/
      if (setstartmeanQandQs == 0)
        for (ii = 0; ii < daysiy; ii++)
          Qicetotal[ep] += (Qice[ii] * dTOs);

      if (setstartmeanQandQs == 3)
        {
                        /*--------------------------------------------------------------
			 *  Compute the total suspended sediment created by glaciers 
			 *  (first step) based on B. Hallet et al., 1996.
			 *  Global and Planetary Change; (figure 4., by Guymon).
			 *  y = 1.7846 + 0.99126x, where y = Log Suspended Sediment
			 *  yield (ton/yr/km2) and x = Log % Glacial Cover in the 
			 *  drainage basin.
			 *  Now Guymon uses a fixed yield point for if there are no 
			 *  glaciers in the basin (x=0; y=1.7846). Since HydroTrend is 
			 *  calculating yield we can modify this starting point by 
			 *  calculating the difference and use the hydrotrend yield 
			 *  as a startpoint. Since it is a log log scale we can't just 
			 *  substitute the 1.7846 value!!!
			 *  In HydroSedload.c the actual distribution over the days is
			 *  calculated.
			 *--------------------------------------------------------------*/
          if ((100 * glacierarea / totalarea) > 1.0)
            {
              yieldQsbar =
                (Qsbartot[ep] * dTOs * 365) / (1000 * (totalarea / 1e6));

              /* yield Hallet if there are no glaciers in the basin */
              yieldnoglaciers = pow (10, 1.7846);

              /* Difference between hallet en HydroTrend yield if there is no glacier */
              yielddifference = yieldQsbar - yieldnoglaciers;

              /* yield if there are glaciers; with new startpoint; only glacier sediment! */
              yield =
                (pow
                 (10,
                  (1.7846 +
                   (0.99126 * log10 ((100 * (glacierarea / totalarea)))))) +
                 yielddifference) - yieldQsbar;

              /* annual glacier influence; Qs only from glaciers */
              Qsglacierannual = yield * (totalarea / 1e6) * 1000;
              if (Qsglacierannual < 0.0)
                Qsglacierannual = 0.0;
            }
          else
            Qsglacierannual = 0.0;
          Qsglaciertotal[ep] += Qsglacierannual;
        }                       /* end setstartmeanQandQs == 3 */

                /*--------------------------
		 *  Check the mass balance
		 *--------------------------*/
      Mice = 0.0;
      Mgw = 0.0;
      for (ii = 0; ii < maxday; ii++)
        Mice += Qice[ii] * dTOs;
      for (ii = 0; ii < daysiy; ii++)
        Mgw += Qicetogw[ii] * dTOs;

      Mout = Mice + Mgw + Eiceannual * glacierarea;
      Minput = massavailable + Mwrap;

      if ((fabs (Mout - Minput) / Minput) > masscheck)
        {
          fprintf (stderr, "ERROR in HydroGlacial: \n");
          fprintf (stderr, "  Mass Balance error: Mout != Minput \n\n");
          fprintf (stderr, "\t fabs(Mout-Minput)/Minput > masscheck \n");
          fprintf (stderr, "\t note: masscheck set in HydroParams.h \n");
          fprintf (stderr, "\t masscheck = %f (%%) \n", masscheck);
          fprintf (stderr, "\t fabs(Mout-Minput)/Minput = %f (%%) \n\n",
                   fabs (Mout - Minput) / Minput);
          fprintf (stderr, " \t Minput = massavailable + Mwrap \n");
          fprintf (stderr, " \t Minput \t\t = %e \n", Minput);
          fprintf (stderr, " \t massavailable \t = %e \n", massavailable);
          fprintf (stderr, " \t Mwrap \t\t = %e \n\n", Mwrap);
          fprintf (stderr, " \t Mout = Mice + Mgw + Eiceannual*glacierarea \n");
          fprintf (stderr, " \t Mout \t\t = %e \n", Mout);
          fprintf (stderr, " \t Mice \t\t = %e \n", Mice);
          fprintf (stderr, " \t Mgw \t\t = %e \n", Mgw);
          fprintf (stderr, " \t Eiceannual \t = %e \n\n",
                   Eiceannual * glacierarea);
          exit (-1);
        }
                /*---------------------------------------------
		 *  Calculate the total mass balance
		 *  per epoch to see howmuch water is storage
		 *  and how much is released.
		 *  This all to come up with a fraction value
		 *  that is used to temper the sedimentation
		 *  rate as glaciers are growing
		 *  (see hydrocalqsnew.c)
		 *---------------------------------------------*/
      if (setstartmeanQandQs == 0)
        {
          if ((Volumelast - Volumeglacierarea) < 0.0)
            {
              GlacierMstorage[ep] += fabs (Gmass);
              GlacierMinput[ep] += MPglacial;
            }
          if (GlacierMinput[ep] > 0.0 && (yr == (syear[ep] + nyears[ep]) - 1))
            {
              fractionglaciersediment[ep] =
                1.0 - (fabs (GlacierMstorage[ep]) / GlacierMinput[ep]);
            }
          else
            fractionglaciersediment[ep] = 1.0;
        }
    }                           /* endif glacial */
  return (err);
}                               /* end of HydroGlacial */
