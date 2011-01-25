/*-------------------------------------------------------------------------------------------
 *  hydroclimate.c
 *
 *	Author: Albert Kettner, March 2006
 *
 *	Sets the annual climate values of P and T using the starting values, the trends and the
 *	Std. Dev. The temperature values are assumed to be at the river mouth. The lapse rate is
 *	used later to calculate the temperature for each altitude bin. Precipitation values should
 *	be representative for the whole basin.
 *
 *  Variable	Def.Location	Type	Units	Usage
 *  --------	------------	----	-----	-----
 *  err			various			int		-		error flag, halts program
 *  jj			various			int		-		temporary loop counter
 *  dumdbl		various			double	-		temporary double
 *  sumt		HydroClimate.c	double	-		total temperature
 *  sump		HydroClimate.c	double	-		total precipitation
 *
 *-------------------------------------------------------------------------------------------*/

#include "hydroclimate.h"
#include "hydrotimeser.h"
#include "hydroinout.h"
#include "hydroparams.h"
#include "hydroreadclimate.h"
#include "hydrodaysmonths.h"

int nelevbins, shouldern;

double Eiceannual, Enivalannual, *ELAchange;
double *ELAstart, Ewetannual;
double glacierarea, glacierelev, *lapserate, MEtotal;
double Minput, Moutput, MQprevious, MQnext, Pannual;
double *Pchange, *Pmassbal, *Pexponent;
double Pmonth[nmonth];
double **Pnominal, **Pnomstd;
double *Prange;
double *Pstart, *Pstd;
double Qbedannual, *Qbedannualoutlet, *Qgrandtotal, ***Qgrandtotaloutlet, Qpeak,
  *Qpeakevents, *Qpeakperoutlet, **Qpeakperoutletall, Qtotal, **Qtotaloutlet,
  *Qtotaloutletannual, ***Qbar, *Qbartotal, *Qicebartotal, *Qpeakall,
  **Qpeakallevents, *Qicetotal, *Qsglaciertotal;
double *Qsgrandtotal, *Qsgrandtotaldelta, **Qsgrandtotaloutlet,
  **Csgrandtotaloutlet, *Qsbarnew, *Qsbarnew1, Qsannual, *Qsannualoutlet,
  *Csannualoutlet, *Qsbartot, *Qsglacierbar, *Qsbar, *Qsmean, *Qsglaciersmean,
  **Coutlettotal;
double *baseflowtot, Csannual, *Csgrandtotal, *totpercentageQ;
double shoulderright[maxshoulder], shoulderleft, shouldermain;
double Snowremains;
double Tannual, *Tchange;
double **Televday, Tmonth[nmonth], **Tnominal;
double **Tnomstd, *Tstart, *Tstd;
double **Qpeakfloodtemp;
double *Qsglaciersgrandtotal, *Qspsigrandtotal;
double Qsglacierannual, Qspsiannual;
double *GlacierMstorage, *GlacierMinput, *fractionglaciersediment;

/*-------------------------
 *  Start of HydroClimate
 *-------------------------*/
int
hydroclimate (gw_rainfall_etc * gw_rain)
{

/*-------------------
 *  Local Variables
 *-------------------*/
  int err, jj, ii;
  double dumdbl, sumt, sump;
  err = 0;

/*----------------------------------------
 *  Calculate Average Annual Temperature
 *  Either by file or by climate generator.
 *----------------------------------------*/
  Tannual = 0.0;
  if (raindatafile == 1)
    Tannual = gw_rain->Tperyear[yr - syear[ep]];
  else
    {
      dumdbl = ranarray[nran];
      nran++;
      if (dumdbl > Tmaxstd)
        dumdbl = Tmaxstd;
      if (dumdbl < -Tmaxstd)
        dumdbl = -Tmaxstd;
      Tannual = Tstart[ep] + Tchange[ep] * (yr - syear[ep]) + dumdbl * Tstd[ep];
    }

/*------------------------------------------
 *  Calculate Average Monthly Temperatures
 *  and scale to actual annual temperature.
 *  Either by file or by climate generator.
 *------------------------------------------*/
  if (raindatafile == 1)
    for (jj = 0; jj < 12; jj++)
      {
        Tmonth[jj] = 0.0;
        for (ii = daystrm[jj] - 1; ii < dayendm[jj]; ii++)
          Tmonth[jj] += gw_rain->T[yr - syear[ep]][ii];
        Tmonth[jj] = (Tmonth[jj] / daysim[jj]);
      }
  else
    {
      sumt = 0.0;
      for (jj = 0; jj < 12; jj++)
        sumt += Tnominal[jj][ep] * daysim[jj];
      for (jj = 0; jj < 12; jj++)
        Tmonth[jj] = Tnominal[jj][ep] - (sumt / daysiy) + Tannual;
    }

/*--------------------------------------------
 *  Calculate Total Annual Precipitation (m)
 *  Either by file or by climate generator. 
 *--------------------------------------------*/
  Pannual = 0.0;
  if (raindatafile == 1)
    for (ii = 0; ii < daysiy; ii++)
      Pannual += gw_rain->R[yr - syear[ep]][ii];
  else
    {
      dumdbl = ranarray[nran];  /* Get one random value. */
      nran++;
      if (dumdbl > Pmaxstd)
        dumdbl = Pmaxstd;
      if (dumdbl < -Pmaxstd)
        dumdbl = -Pmaxstd;
      Pannual =
        Pmassbal[ep] * Pstart[ep] + Pchange[ep] * (yr - syear[ep]) +
        dumdbl * Pstd[ep];
      if (Pannual < 0)
        Pannual = 0.;
    }

/*----------------------------------------------------
 *  Calculate Total Monthly Precipitation and
 *  scale to actual annual precipitation
 *  Pmonth (m/mnth)= (m/mth) * ((m/y-m/y) / (m/y))
 *  Either by file or by climate generator.
 *----------------------------------------------------*/
  if (raindatafile == 1)
    for (jj = 0; jj < 12; jj++)
      {
        Pmonth[jj] = 0.0;
        for (ii = daystrm[jj] - 1; ii < dayendm[jj]; ii++)
          Pmonth[jj] += gw_rain->R[yr - syear[ep]][ii];
      }
  else
    {
      sump = 0.0;
      for (jj = 0; jj < 12; jj++)
        sump += Pnominal[jj][ep];
      for (jj = 0; jj < 12; jj++)
        Pmonth[jj] = Pnominal[jj][ep] * (Pannual / sump);
    }

  return (err);
}                               /* end of HydroClimate */

