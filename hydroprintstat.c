/*-------------------------------------------------------------------------------------------
 *	hydroprintstat.c
 * 
 *	Author: Albert Kettner, March 2006
 * 
 *	Prints the statistics of the stochastic models used to calculate sediment discharge and
 *	discharge.
 *
 * Variable		Def.Location		Type	Units	Usage
 * --------		------------		----	-----	-----
 * A			HydroPrintStat.c	double	km^2	total basin area
 * cbar			HydroPrintStat.c	double	-		mean of normal random variable
 * err			various				int		-		error flag, halts program
 * H			HydroPrintStat.c	double	m		max relief basin area
 * sigmapsi		HydroPrintStat.c	double	-		sigma of psi
 * Tbar			HydroPrintStat.c	double	Celsius	mean basin temperature
 * s			HydroPrintStat.c	double	-		sigma C
 * 
 *-------------------------------------------------------------------------------------------*/

#include <math.h>
#include "hydroclimate.h"
#include "hydroparams.h"
#include "hydroinout.h"

/*-----------------------------
 *  Start of HydroPrintStat
 *-----------------------------*/
int
hydroprintstat ()
{

/*-------------------
 *  Local Variables
 *-------------------*/
  int err, kk;
  double A, H, sigmapsi, cbar, s;
  double Tbar, Tmean, Tend;

/*------------------------
 *  Initialize variables
 *------------------------*/
  err = 0;
  Tbar = 0.0;

  Tend = Tstart[ep] + (Tchange[ep] * nyears[ep]);
  Tmean = (Tstart[ep] + Tend) / 2;
//      Tbar = Tmean - ( (lapserate[ep] * maxalt) / 3.0 );
  for (kk = 0; kk < nhypts; kk++)
    {
      if (kk == 0)
        Tbar +=
          (Tmean -
           (((hypselev[kk] -
              hypselev[0])) * lapserate[ep])) * (hypsarea[kk] / totalarea);
      else
        Tbar +=
          (Tmean -
           (((hypselev[kk] - hypselev[0])) * lapserate[ep])) * ((hypsarea[kk] -
                                                                 hypsarea[kk -
                                                                          1]) /
                                                                totalarea);
    }


/*----------------------------------
 *  Calculate the needed variables
 *----------------------------------*/
  A = (totalarea / 1e6);
  H = maxalt;
  sigmapsi = 0.763 * pow (0.99995, Qbartotal[ep]);
  cbar =
    (1.4 - (0.025 * Tbar) + (0.00013 * H) + (0.145 * log10 (Qsbartot[ep])));
  s = 0.17 + (0.0000183 * Qbartotal[ep]);

/*--------------------
 *  Print statistics
 *--------------------*/
  fprintf (fidstat,
           " Values used to calculate discharge and sediment discharge\n");
  fprintf (fidstat, " for the stochastic model in hydrotrend run: \n");
  fprintf (fidstat, " %s\n", title);
  fprintf (fidstat, "epoch: %d \n", ep + 1);
  fprintf (fidstat,
           "*****************************************************************\n");
  if (Qsbarformulaflag[ep] == 2)
    {
      if (Tmean >= 2.0)
        {
          fprintf (fidstat,
                   "Qsbar = alpha9 * Lithology * (1-TE) * Eh * pow(A,alpha11) * pow((Q*yTOs/1e9),alpha10) * H * Tbar\n");
          fprintf (fidstat, "\t A = %.2f; river basin area (km2)\n", A);
          fprintf (fidstat, "\t H = %.2f; maxalt = basin relief (km)\n\n",
                   H / 1000);
          fprintf (fidstat, "\t Tbar =  %.2f\n", Tbar);
          fprintf (fidstat, "\t Lithology = %.2f (-)\n", lithology[ep]);
          fprintf (fidstat, "\t Eh = %.2f; Anthropogenic factor (-)\n",
                   anthro[ep]);
          fprintf (fidstat,
                   "\t alpha9, alpha10, alpha11 are global constants:\n");
          fprintf (fidstat, "\t alpha9 = %.2e\n", alpha9);
          fprintf (fidstat, "\t alpha10 = %.2e\n", alpha10);
          fprintf (fidstat, "\t alpha11 = %.2e\n", alpha11);
          fprintf (fidstat, "Qsbar = %.2f (kg/s)\n", Qsbartot[ep]);
          fprintf (fidstat,
                   "Glacier sediment flux is calculated seperately. The mean suspended sediment\n");
          fprintf (fidstat, "flux of glaciers is: %.2f (kg/s)\n\n\n",
                   fractionglaciersediment[ep] * Qsglacierbar[ep]);
          fprintf (fidstat, "Qbar = (sumQ(daily))/number of days\n");
          fprintf (fidstat, "Qbar = %.2f (m3/s)\n\n\n", Qbartotal[ep]);
        }
      if (Tmean < 2.0)
        {
          fprintf (fidstat,
                   "Qsbar = 2 * alpha9 * Lithology * (1-TE) * Eh * pow(A,alpha11) * pow((Q*yTOs/1e9),alpha10) * H\n");
          fprintf (fidstat, "\t A = %.2f; river basin area (km2)\n", A);
          fprintf (fidstat, "\t H = %.2f; maxalt = basin relief (km)\n\n",
                   H / 1000);
          fprintf (fidstat, "\t Lithology = %.2f (-)\n", lithology[ep]);
          fprintf (fidstat, "\t Eh = %.2f; Anthropogenic factor (-)\n",
                   anthro[ep]);
          fprintf (fidstat,
                   "\t alpha9, alpha10, alpha11 are global constants:\n");
          fprintf (fidstat, "\t alpha9 = %.2e\n", alpha9);
          fprintf (fidstat, "\t alpha10 = %.2e\n", alpha10);
          fprintf (fidstat, "\t alpha11 = %.2e\n", alpha11);
          fprintf (fidstat, "Qsbar = %.2f (kg/s)\n", Qsbartot[ep]);
          fprintf (fidstat,
                   "Glacier sediment flux is calculated seperately. The mean suspended sediment\n");
          fprintf (fidstat, "flux of glaciers is: %.2f (kg/s)\n\n\n",
                   fractionglaciersediment[ep] * Qsglacierbar[ep]);
          fprintf (fidstat, "Qbar = (sumQ(daily))/number of days\n");
          fprintf (fidstat, "Qbar = %.2f (m3/s)\n\n\n", Qbartotal[ep]);
        }
    }
  if (Qsbarformulaflag[ep] == 1)
    {
      fprintf (fidstat,
               "Qsbar =  alpha3 * pow(A,alpha4) * pow(H,alpha5) * exp(k1 * Tbar)\n");
      fprintf (fidstat, "\t A = %.2f; river basin area (km2)\n", A);
      fprintf (fidstat, "\t H = %.2f; maxalt = basin relief (m)\n\n", H);
      fprintf (fidstat, "\t T = %.2f; Tbar = mean basin temp (C)\n", Tbar);
      fprintf (fidstat,
               "\t\t T =Tstart[ep](=%.2f) - ((lapserate[ep](=%f) * maxalt(=%.5f))/3.0\n\n",
               Tstart[ep], lapserate[ep], maxalt);
      fprintf (fidstat,
               "\t alpha3, alpha4, alpha5 and k are set by temperature and\n");
      fprintf (fidstat,
               "\t latitude geographic position of the river mouth, lat=%.2f\n ",
               lat);
      fprintf (fidstat, "\t alpha3 = %.2e\n", alpha3);
      fprintf (fidstat, "\t alpha4 = %.2e\n", alpha4);
      fprintf (fidstat, "\t alpha5 = %.2e\n", alpha5);
      fprintf (fidstat, "\t k= %.2e\n", k1);
      fprintf (fidstat, "Qsbar = %.2f (kg/s)\n", Qsbartot[ep]);
      fprintf (fidstat,
               "Glacier sediment flux is calculated seperately. The mean suspended sediment\n");
      fprintf (fidstat, "flux of glaciers is: %.2f (kg/s)\n\n\n",
               fractionglaciersediment[ep] * Qsglacierbar[ep]);
      fprintf (fidstat, "Qbar = (sumQ(daily))/number of days\n");
      fprintf (fidstat, "Qbar = %.2f (m3/s)\n\n\n", Qbartotal[ep]);
    }
  if (Qsbarformulaflag[ep] == 0)
    {
      fprintf (fidstat,
               "Qsbar =  alpha6 * pow(Qbar,alpha7) * pow(H,alpha8) * exp(k2 * Tbar)\n");
      fprintf (fidstat, "\t Qbar = %.2f; long-term average of Q (m3/s)\n",
               Qbartotal[ep]);
      fprintf (fidstat, "\t H = %.2f; maxalt = basin relief (m)\n\n", H);
      fprintf (fidstat, "\t T = %.2f; Tbar = mean basin temp (C)\n", Tbar);
      fprintf (fidstat,
               "\t\t T =Tstart[ep](=%.2f) - ((lapserate[ep](=%.5f) * maxalt(=%.2f))/3.0\n\n",
               Tstart[ep], lapserate[ep], maxalt);
      fprintf (fidstat,
               "\t alpha6, alpha7, alpha8 and k are set by temperature and\n");
      fprintf (fidstat,
               "\t latitude geographic position of the river mouth, lat=%.2f\n ",
               lat);
      fprintf (fidstat, "\t alpha6 = %.2e\n", alpha6);
      fprintf (fidstat, "\t alpha7 = %.2e\n", alpha7);
      fprintf (fidstat, "\t alpha8 = %.2e\n", alpha8);
      fprintf (fidstat, "\t k= %.2e\n", k2);
      fprintf (fidstat, "Qsbar = %.2f (kg/s)\n", Qsbartot[ep]);
      fprintf (fidstat,
               "Glacier sediment flux is calculated seperately. The mean suspended sediment\n");
      fprintf (fidstat, "flux of glaciers is: %.2f (kg/s)\n\n\n",
               fractionglaciersediment[ep] * Qsglacierbar[ep]);
    }
  fprintf (fidstat, "(Qs(daily)/Qsbar) = psi(daily) * (Q(daily)/Qbar)^C\n");
  fprintf (fidstat, "\t Qs(daily) = daily sediment discharge (kg/s)\n");
  fprintf (fidstat,
           "\t Qsbar = %.2f (kg/s); used long-term average of Qs (fractions of Qs(daily)/nr. of fractions)\n\n",
           Qsmean[ep] * Qsbarnew[ep]);
  fprintf (fidstat, "\t psi = log-normal random variable,\n");
  fprintf (fidstat,
           "\t\t psi   = a random number from a lognormal distribution with\n");
  fprintf (fidstat, "\t\t mean 1 and sigma psi = 0.763 * (0.99995^Qbar)\n");
  fprintf (fidstat, "\t sigma psi = %f\n\n", sigmapsi);
  fprintf (fidstat, "\t Q(daily) = daily discharge (m3/s)\n");
  fprintf (fidstat,
           "\t Qbar = %.2f (m3/s); long-term average of Q, calculated by formula explained above\n\n",
           Qbartotal[ep]);
  fprintf (fidstat,
           "\t C\t= a random number from a distribution with mean E(C)\n");
  fprintf (fidstat, "\t\t and standard deviation sigma-C, where:\n");
  fprintf (fidstat,
           "\t\t E(C) = (1.4 - (0.025*T) + (0.00013*H) + (0.145*log10(Qsbar))\n");
  fprintf (fidstat, "\t\t E(C) = %f\n", cbar);
  fprintf (fidstat, "\t\t sigma-C = 0.17 + (0.0000183 * Qbar)\n");
  fprintf (fidstat, "\t\t sigma-C = %f\n\n", s);
  fprintf (fidstat,
           "*****************************************************************\n\n\n");
  return (err);
}                               /* end of hydroprintstat.c */
