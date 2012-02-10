/*-------------------------------------------------------------------------------------------
 *	hydrosedload.c
 *
 *	Author: Albert Kettner, April 2006
 *
 *	Calculates the sediment load (suspended and bedload) discharging from the river mouth for
 *	each day, using new formula from:
 *  Morehead, M.D., Syvitski, J.P.M., Hutton, E.W.H., Peckham, S.D., 2003. Modeling the temporal
 *  variability in the flux of sediment from ungauged river basins. Global and Planetary
 *  Change 39, 95-110.
 *
 *  The formula used:
 *
 *	Qs(i) = Psi * Qsbar * (Q/Qbar)^C
 *
 *	Qsbar = long-term average of Qs (for an epoch)
 *	1) Qsbar = alpha3 * (1-TE) * pow(A,alpha4) * pow(H,alpha5) * exp(k * Tbar)
 *	2) Qsbar = alpha6 * (1-TE) * pow(Qbar,alpha7) * pow(H,alpha8) * exp(k * Tbar)
 *  (See: Syvitski, J.P.M., Peckham, S.D., Hilberman, R., Mulder, T., 2003. Predicting the
 *  terrestrial flux of sediment to the global ocean: a planetary perspective. Sedimentary
 *  Geology 162, 5-24).
 *  if T>=2.0
 *  3) Qsbar = alpha9 * B * pow(A,alpha11) * pow((Q*yTOs/1e9),alpha10) * H/1000 * Tbar
 *  else (T <2.0)
 *  3) Qsbar = 2 * alpha9 * B * pow(A,alpha11) * pow((Q*yTOs/1e9),alpha10) * H/1000
 *  (See: Syvitski, J.P.M. and Milliman, J.D., 2007. Geology, Geography and Hyumans
 *  battle for Dominance over the delivery of fluvial sediment to the coastal ocean. Journal of
 *  Geology 115, 1-19).
 * 
 *	Qbar  = long-term average of Q  (for an epoch)
 *	Tbar  = Tmean - ((lapserate[ep] * maxalt)/3.0);
 *	Tmean = (Tstart[ep] + (Tstart[ep] + (Tchange[ep]*nyears[ep])))/2;
 * 
 *	Psi   = a random number from a lognormal distribution with mean 1 and sigma = 0.763 *
 *	(0.99995)^Qbar
 *	C     = a random number from a distribution with mean cbar and standard deviation s, where:
 *	cbar = (1.4 - (0.025*T) + (0.00013*H) + (0.145*ln(Qsbar))and s = 0.17 + (0.0000183 * Qbar)
 *	i  = subscript for instantaneous or daily values
 *	A  = totalarea = basin area (km^2)
 *	H  = maxalt = basin relief (m)
 *	T  = Tbar = mean basin temp (C)
 *	Q  = daily discharge (m^3/s)
 *	Qs = daily sediment flux (kg/s)
 *	Cs = daily sediment concentration (kg sediment/ m^3 water)
 * 
 *	If reservoir capacity is larger than 0.5km^3, trapping efficiency (TE) calculated, based on
 *	paper of:
 *	Charles J. Vorosmarty, Michel Meybeck, Balazs Rekete & Keshav Sharma: The potential impact
 *	of neo-Castorization on sediment transport by the global network of rivers (1997) in Human
 *	Impact on erosion and Sedimentation (Proceedings of the Rabat Symposium April 1997).
 *	Based on the Brune equation:
 *
 *	TE =  Is only effective as lakes of reservoirs are turned on in the input file.
 *	TEbasin = 1.0 - (0.05 / exp(Rvol/RQbar)0.5
 *	Rvol = Volume of the reservoir
 *	RQbar= discharge at the basin mouth of the reservoir 
 *
 *	If reservoir capacity is smaller than 0.5km^3; trapping efficiency (TE) calculated, based on
 *	paper of:
 *	Gert Verstraeten and Jean Poesen: Estimating trap efficiency of small reservoirs and ponds:
 *	methods and implications for the assessment of sediment yield. Progress in Physical
 *	Geography 24,2 (2000) pp.219-251
 *
 *	TEbasin = ( 1.0 - (1.0 / (1 + 0.00021 * ((Rvol[ep] * 1e9) / Rarea[ep]))))
 *
 *	Variable		Def.Location	Type		Units	Usage
 *	--------		------------	----		-----	-----
 *
 *-------------------------------------------------------------------------------------------*/

#include <math.h>
#include <stdlib.h>
#include "hydroclimate.h"
#include "hydroinout.h"
#include "hydroparams.h"
#include "hydrotimeser.h"
#include "hydroreadclimate.h"
#include "hydroalloc_mem.h"
#include "hydrofree_mem.h"
#include "hydrornseeds.h"

/*-------------------------
 *  Start of HydroSedLoad
 *-------------------------*/
int
hydrosedload (gw_rainfall_etc * gw_rain)
{

/*-------------------
 *  Local Variables
 *-------------------*/
  int err, i, p, kk, y, set_event;
  long j;
  double A, H, RQbar, Hr;
  double Tbar, Tmean, Tend, Tdummy;
  double Psi[daysiy], mu, sigma;
  double cbar, s;
  double ratio1;
  double unit_normal, normal;
  double unit_normal2, normal2;
  double trnfac;
  double *Coutletannual;
  float dumflt;
  double massdummy;
  double decay_multiplier;

/*------------------------
 *  Initialize Variables
 *------------------------*/
  err = 0;
  y = 0;
  annualhyperpycnalflag = 0;
  Qshyperpycnal = 0.0;
  Qspeak = 0.0;
  Cspeak = 0.0;
  A = (totalarea[ep] / 1e6);    /****  FORMULA USES AREA in km^2  ****/
  H = maxalt[ep];

/*-------------------------------------------------------
 *  Allocate memory for possible multiple outlet module
 *-------------------------------------------------------*/
  Coutletannual = malloc1d (maxnoutlet, double);
  Qsbartotoutlet = malloc2d (nepochs, maxnoutlet, double);

/*-------------------------------------
 *  Calculate Tbar for drainage basin
 *-------------------------------------*/
  if (raindatafile == 1)
    {
      Tdummy = 0.0;
      for (i = 0; i < nyears[ep]; i++)
        Tdummy += gw_rain->Tperyear[i];
      Tmean = Tdummy / nyears[ep];
    }
  else
    {
      Tend = Tstart[ep] + (Tchange[ep] * nyears[ep]);
      Tmean = (Tstart[ep] + Tend) / 2;
    }
//Tbar = Tmean - ( (lapserate[ep] * maxalt) / 3.0 );

  if (yr == syear[ep])
    {
      Tbar = 0.0;
      for (kk = 0; kk < nhypts[ep]; kk++)
        {
          if (kk == 0)
            Tbar +=
              (Tmean -
               (((hypselev[ep][kk] -
                  hypselev[ep][0])) * lapserate[ep])) * (hypsarea[ep][kk] /
                                                         totalarea[ep]);
          else
            Tbar +=
              (Tmean -
               (((hypselev[ep][kk] -
                  hypselev[ep][0])) * lapserate[ep])) * ((hypsarea[ep][kk] -
                                                          hypsarea[ep][kk -
                                                                       1]) /
                                                         totalarea[ep]);
        }
    }

/*-------------------------------------------------------
 *  Calculate trapping efficiency (TE) of the reservoir
 *-------------------------------------------------------*/
  if (yr == syear[ep])
    {
      if (Rvol[ep] != 0.0)
        {

                /*----------------------------------------
 		 *  Calculate area upstream of reservoir
		 *----------------------------------------*/
          if (Rarea[ep] == 0.0)
            for (kk = 0; kk < nhypts[ep]; kk++)
              if (hypselev[ep][kk] == Ralt[ep] || hypselev[ep][kk] > Ralt[ep])
                {
                  Rarea[ep] = A - (hypsarea[ep][kk] / 1e6);
                  kk = nhypts[ep];
                }

          if (Rvol[ep] < 0.5)
            {
        /*-------------------------------------
	 *  TE Calculated with Browns methode 
	 *-------------------------------------*/
              TEsubbasin[ep] =
                (1.0 - (1.0 / (1 + 0.00021 * ((Rvol[ep] * 1e9) / Rarea[ep]))));
              TE[ep] = (Rarea[ep] / A) * TEsubbasin[ep];
            }

          else if (Rvol[ep] >= 0.5)
            {
        /*----------------------------------------------------
	 *  TE Calculated with Charles J. Vorosmarty methode 
	 *----------------------------------------------------*/
              RQbar = Qbartotal[ep] * (Rarea[ep] / A);
              RQbar *= 0.031536;        /* FORMULA USES QBAR in KM3/YEAR */
              TEsubbasin[ep] = 1.0 - (0.05 / pow (((Rvol[ep] / RQbar)), 0.5));
              TE[ep] = (Rarea[ep] / A) * TEsubbasin[ep];
            }
        }
      else if (Rvol[ep] == 0.0)
        TE[ep] = 0.0;

/*--------------------------------
 *  Compute Qsbar for this epoch
 *--------------------------------*/
      if (Qsbarformulaflag[ep] == 2)
        {                       //BQART
          if (Tbar >= 2.0)
            Qsbartot[ep] =
              alpha9 * lithology[ep] * (1 - TE[ep]) * anthro[ep] * pow (A,
                                                                        alpha11)
              * pow (((Qbartotal[ep] * yTOs) / 1e9),
                     alpha10) * (H * 0.001) * Tbar;

          if (Tbar < 2.0)
            Qsbartot[ep] =
              2.0 * alpha9 * lithology[ep] * (1 - TE[ep]) * anthro[ep] * pow (A,
                                                                              alpha11)
              * pow (((Qbartotal[ep] * yTOs) / 1e9), alpha10) * (H * 0.001);
        }
      if (Qsbarformulaflag[ep] == 1)
        {                       //ART
          Qsbartot[ep] =
            alpha3 * (1.0 - TE[ep]) * pow (A, alpha4) * pow (H,
                                                             alpha5) * exp (k1 *
                                                                            Tbar);
        }

      if (Qsbarformulaflag[ep] == 0)
        {                       //QRT
          Qsbartot[ep] =
            (alpha6 * (1.0 - TE[ep]) * pow (Qbartotal[ep], alpha7) *
             pow (H, alpha8) * exp (k2 * Tbar));
        }
    }
/*-------------------------------------------
 *  Compute Qsbar per outlet for this epoch
 *-------------------------------------------*/
  if (outletmodelflag == 1 && setstartmeanQandQs > 2)
    {
      Qsoutletdummy = 0.0;
      for (p = 0; p < maxnoutlet; p++)
        {
          Qsbartotoutlet[ep][p] =
            pow (Qbar[ep][p][eventcounter - eventsperyear], alpha7);
          Qsoutletdummy += Qsbartotoutlet[ep][p];
        }
    }
  
/*--------------------------------------------
 *	Compute the bedload transport factor
 *--------------------------------------------*/
  trnfac = (rhowater * rhosed * trneff) /
    ((rhosed - rhowater) * tan (anglerep * degTOr));

/*----------------------------------------------
 *  Get parameters for random numbers Psi & C.
 *  Qsum is passed in.
 *----------------------------------------------*/
  mu = 0.0;
  s = 0.17 + (0.0000183 * Qbartotal[ep]);
  sigma = 0.763 * pow (0.99995, Qbartotal[ep]);
  cbar = 1.4 - (0.025 * Tbar) + (0.00013 * H) + (0.145 * log10 (Qsbartot[ep]));

/*------------------------------------------
 *  Initialize the annual values to zero.
 *  The Cs[i], Qs[i], and Qb[i] arrays are
 *  initialized to zero in HydroTrend.c.
 *------------------------------------------*/
  Qsannual = 0.0;
  Qspsiannual = 0.0;
  Csannual = 0.0;
  Qbedannual = 0.0;
  if (outletmodelflag == 1 && setstartmeanQandQs > 2)
    for (p = 0; p < maxnoutlet; p++)
      {
        Qsannualoutlet[p] = 0.0;
        Csannualoutlet[p] = 0.0;
        Qbedannualoutlet[p] = 0.0;
        Coutletannual[p] = 0.0;
      }
  if (yr == syear[ep])
    {
      Qsgrandtotal[ep] = 0.0;
      Qspsigrandtotal[ep] = 0.0;
      Csgrandtotal[ep] = 0.0;
      if (outletmodelflag == 1 && setstartmeanQandQs > 2)
        for (p = 0; p < maxnoutlet; p++)
          {
            Qsgrandtotaloutlet[ep][p] = 0.0;
            Csgrandtotaloutlet[ep][p] = 0.0;
          }
      start_decay_year = 0;
      end_decay_year = 0;
      
    }

  /*-------------------------------------------------
   *	Compute the decay function given the duration
   *  This is done for the quake sub routine
   *-------------------------------------------------*/
  for (kk = 0; kk < quakeeventcounter[ep]; kk++)
    if (quakeeventyear[ep][kk] == yr && quakeeventenergy[ep][kk] > quakethresholdenergy){
      if (quakeeventenergy[ep][kk] > quakethresholdenergy_max){
        quakeeventenergy[ep][kk] = quakethresholdenergy_max;
      }
      start_decay_year = yr;
      end_decay_year = yr+quakeeventduration[ep][kk];
      set_event = kk;
    }
  if(yr >= start_decay_year && yr <= end_decay_year && start_decay_year != end_decay_year){
    max_quake_erosion = ((5.3 * quakeeventenergy[ep][set_event])/quakeeventdistance[ep][set_event]) * (exp(quakeeventdistance[ep][set_event]/quakedampingfactor)); 
    decay_multiplier = 1+(((end_decay_year - yr) * (max_quake_erosion-1)) / (end_decay_year - start_decay_year));
  }
  else
    decay_multiplier = 1;
  
  
    /*-----------------------------------
     *  Generate the random number, C
     *  (Can be normal or uniform.)
     *-----------------------------------*/
  if (setstartmeanQandQs == 2 && yr == syear[ep])
    {
      C = malloc1d (nyears[ep], double);
      for (j = 0; j < nyears[ep]; j++)
        {
          dumflt = ranarraysediment[j];
          unit_normal2 = (double) dumflt;
          normal2 = (s * unit_normal2) + cbar;
          C[j] = normal2;
        }
    }

/*--------------------------------------
 *  Loop through each day of year and
 *  calculate total sediment discharge.
 *  (daysiy defined in hydrotimeser.h)
 *--------------------------------------*/
  for (i = 0; i < daysiy; i++)
    {

    /*-----------------------------------
     *  Generate the random number, Psi
     *-----------------------------------*/
      unit_normal = ranarray[i];
      normal = (sigma * unit_normal) + mu;
      Psi[i] = exp (normal);

    /*----------------------------------------------
     *  Compute daily sediment discharge, Qs.
     *  Save both ratio's in memory or if there is
     *  not enough memory, save it to temp file.
     *----------------------------------------------*/
      if (setstartmeanQandQs < 4)
        {
          if ((100 * glacierarea / totalarea[ep]) > 1.0)
          {
            Qspsi[i] = decay_multiplier * Psi[i] * pow ((Qsumtot[i]) / (Qbartotal[ep]), C[yr - syear[ep]]);
            Qs[i] = Qspsi[i];
          }
          else
          {
            Qs[i] = decay_multiplier * Psi[i] * pow ((Qsumtot[i]) / (Qbartotal[ep]), C[yr - syear[ep]]);
            Qspsi[i] = Qs[i];
          }
        }
      if (setstartmeanQandQs == 4)
        {
          if ((100 * glacierarea / totalarea[ep]) > 1.0)
            {
        /*----------------------------------------------------------------
		 *  Calculate the PSI part of the suspended sediment per day
		 *----------------------------------------------------------------*/
              Qspsi[i] = (decay_multiplier * Psi[i] * Qsbarnew[ep] * pow ((Qsumtot[i]) / (Qbartotal[ep]), C[yr - syear[ep]]));
              
        /*--------------------------------------------------
		 *  Calculate the total suspended sediment per day
		 *--------------------------------------------------*/
              Qs[i] = Qspsi[i];
            }
          else
            {
                /*---------------------------------------------------------------------------
		 *  Calculate the PSI part (= total part) of the suspended sediment per day
		 *---------------------------------------------------------------------------*/
              Qs[i] = (decay_multiplier * Psi[i] * Qsbarnew[ep] * pow ((Qsumtot[i]) / (Qbartotal[ep]), C[yr - syear[ep]]));
              Qspsi[i] = Qs[i];
            }

    /*-------------------------
     *  Check for NaNs in Qs
     *-------------------------*/
          if (isnan (Qs[i]))
            {
              fprintf (stderr, "\nHydroSedload ERROR: Qs = %e \n", Qs[i]);
              fprintf (stderr, " yr = %d, day = %d \n", yr, i);
              fprintf (stderr, "   Qice[i]      = %e \n", Qice[i]);
              fprintf (stderr, "	Qicebart.[ep]= %e \n",
                       Qicebartotal[ep]);
              fprintf (stderr, "   baseflow[ep] = %e \n", baseflowtot[ep]);     /*  variables below here should be deleted */
              fprintf (stderr, "	Qsum[i]		 = %e \n", Qsumtot[i]);
              fprintf (stderr, "	Qbar[ep]	 = %e \n",
                       Qbartotal[ep]);
              fprintf (stderr, "   Psi[i]       = %e \n", Psi[i]);
              fprintf (stderr, "   Qsbarnew[ep] = %e \n", Qsbarnew[ep]);
              fprintf (stderr, "   C[i]         = %e \n", C[yr - syear[ep]]);
              fprintf (stderr, "   ep           = %d \n", ep);
              fprintf (stderr, "   cbar         = %e \n", cbar);
              fprintf (stderr, "	Qsglacier[i] = %e \n", Qsglacier[i]);
              fprintf (stderr, "	QsART/QRT[i] = %e \n", Qspsi[i]);
              fprintf (stderr, "	Qsum-Qice[i] = %e \n",
                       (Qsumtot[i] - Qice[i]));
              fprintf (stderr, "	Qbar-Qicebart= %e \n",
                       (Qbartotal[ep] - Qicebartotal[ep]));
              err = 1;
            }
        }                       //( end setstartmeanQandQs == 4 )

    /*---------------------------------
     *  Compute Qsannual and daily Cs
     *---------------------------------*/
      if (Qs[i] > 0.0)
        {
          Cs[i] = Qs[i] / Qsumtot[i];
          Qsannual += Qs[i] * dTOs;
          Qspsiannual += Qspsi[i] * dTOs;
        }
      else
        Cs[i] = 0.0;

      if (Qs[i] > Qspeak)
        {
          Qspeak = Qs[i];
        }
      if (Cs[i] > Cspeak)
        {
          Cspeak = Cs[i];
        }
    /*---------------------------------
     *  Flag hyperpycnal Cs events + 
     *  sum there contribution to the 
     *  annual sediment load
     *---------------------------------*/
      if (Cs[i] > hyperpycnalvalue)
        {
          annualhyperpycnalflag++;
          Qshyperpycnal += (Qs[i] * dTOs);
        }
      Csannual += Cs[i] * dTOs;

    /*---------------------------
     *  Compute bedload (kg/s)
     *--------------------------*/
      Qb[i] = trnfac * rslope[ep] * pow (Qsumtot[i], alphabed[ep]);
      Qbedannual += Qb[i] * dTOs;

    /*-------------------------------
     *  Compute sediment per outlet
     *-------------------------------*/
      if (outletmodelflag == 1 && (setstartmeanQandQs > 2))
        {

        /*----------------------------------------------------------------
	 *  If there is more than 1 event occuring during a single year,
	 *  keep tracking on what they it is, to change the Qbar on that
	 *  day.
	 *----------------------------------------------------------------*/
          if (numberday[y] == i && eventsperyear > 0)
            {
              y++;
              eventsperyear--;
            }
          massdummy = 0.0;
          for (p = 0; p < maxnoutlet; p++)
            {
              if (outletpct[p][ep][eventcounter - eventsperyear] != 0.0)
                {
    /*-------------------------
     *  Compute Qs per outlet
     *-------------------------*/
                  ratio1 =
                    Qsum[i][p] / Qbar[ep][p][eventcounter - eventsperyear];
                  if (setstartmeanQandQs == 3)
                    Qsoutlet[i][p] =
                      Psi[i] *
                      pow (((Qsbartotoutlet[ep][p] / Qsoutletdummy) *
                            Qsbartot[ep]), C[yr - syear[ep]]) * pow (ratio1,
                                                                     C[yr -
                                                                       syear
                                                                       [ep]]);
                  if (setstartmeanQandQs == 4)
                    {
                      Qsoutlet[i][p] =
                        Psi[i] *
                        pow (((Qsbartotoutlet[ep][p] / Qsoutletdummy) *
                              Qsbartot[ep]),
                             C[yr - syear[ep]]) * Qsbarnew[ep] * pow (ratio1,
                                                                      C[yr -
                                                                        syear
                                                                        [ep]]);

    /*-------------------------
     *  Check for NaNs in Qs
     *-------------------------*/
                      if (isnan (Qsoutlet[i][p]))
                        {
                          fprintf (stderr,
                                   "\nHydroSedload ERROR: Qsoutlet[day][nr] = nan \n");
                          fprintf (stderr, " yr = %d, day = %d, outlet = %d \n",
                                   yr, i, p);
                          fprintf (stderr, "   Qnival[i]    = %e \n",
                                   Qnival[i]);
                          fprintf (stderr, "   Qrain[i]     = %e \n", Qrain[i]);
                          fprintf (stderr, "   Qexceedgw[i] = %e \n",
                                   Qexceedgw[i]);
                          fprintf (stderr, "   Qice[i]      = %e \n", Qice[i]);
                          fprintf (stderr, "   Qss[i]       = %e \n", Qss[i]);
                          fprintf (stderr, "   baseflow[ep] = %e \n", baseflowtot[ep]); /*  variables below here should be deleted */
                          fprintf (stderr, "   ratio        = %e \n", ratio1);
                          fprintf (stderr, "	Qsumoutlet	 = %e \n",
                                   Qsum[i][p]);
                          fprintf (stderr, "	Qbar[ep]	 = %e \n",
                                   Qbartotal[ep]);
                          fprintf (stderr, "   Psi[i]       = %e \n", Psi[i]);
                          err = 1;
                        }
                    }
                  massdummy += Qsoutlet[i][p];
                }
            }
                /*--------------
		 *  Check mass
		 *--------------*/
          if (setstartmeanQandQs == 4)
            {
              for (p = 0; p < maxnoutlet; p++)
                {
                  Qsoutlet[i][p] = Qsoutlet[i][p] * (Qs[i] / massdummy);
                }
            }

          for (p = 0; p < maxnoutlet; p++)
            {
              if (outletpct[p][ep][eventcounter - eventsperyear] != 0.0)
                {
                  Qsannualoutlet[p] += Qsoutlet[i][p] * dTOs;

    /*--------------------------------------------
     *  Compute Qsannual and daily Cs per outlet
     *--------------------------------------------*/
                  if (Qsoutlet[i][p] > 0.0)
                    Csoutlet[i][p] = Qsoutlet[i][p] / Qsum[i][p];
                  else
                    Csoutlet[i][p] = 0.0;
                  Csannualoutlet[p] += Csoutlet[i][p] * dTOs;

    /*-------------------------------------
     *  Compute bedload (kg/s) per outlet
     *-------------------------------------*/
                  Qboutlet[i][p] =
                    trnfac * rslope[ep] * pow (Qsum[i][p], alphabed[ep]);
                  Qbedannualoutlet[p] += Qboutlet[i][p] * dTOs;
                }
              else
                Qsoutlet[i][p] = 0.0;
            }                   /* end for outlets */
        }
    }  /* end for loop over days */
//  printf(", %lf\n",Qsannual);
  Qsgrandtotal[ep] += Qsannual;
  Csgrandtotal[ep] += Csannual;
  Qspsigrandtotal[ep] += Qspsiannual;
  if (outletmodelflag == 1 && setstartmeanQandQs > 2)
    for (p = 0; p < maxnoutlet; p++)
      {
        Qsgrandtotaloutlet[ep][p] += Qsannualoutlet[p];
        Csgrandtotaloutlet[ep][p] += Csannualoutlet[p];
        Coutlettotal[ep][p] += Coutletannual[p];
      }

/*---------------
 *  Free memory
 *---------------*/
  freematrix1D ((void *) Coutletannual);
  freematrix2D ((void **) Qsbartotoutlet, nepochs);

  return (err);
}                               /* end of HydroSedLoad */
