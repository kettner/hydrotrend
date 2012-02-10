/*-------------------------------------------------------------------------------------------
 *	HydroSetParams.c
 *
 *	Author: Albert Kettner, March 2006
 *
 *	In hydroreadinput.c, units are converted as follows:
 *  Multiply totalarea by 1e6.  (km^2 -> m^2)
 *  Multiply basinlength by 1000.  (km -> m)
 *  Divide Ko by 1000.  (mm/day -> m/day)
 *  Divide Lapse rate by 1000.  (mm/day -> m/day)
 *  Some others are done by this routine.
 * 
 *	Variable		Def.Location	Type		Units	Usage
 *	--------		------------	----		-----	-----
 *
 *-------------------------------------------------------------------------------------------*/

#include "hydroparams.h"
#include "hydroclimate.h"

/*-------------------
 *  Time Parameters
 *-------------------*/
int ep, nepochs;
int yr, total_yr;
int *nyears, *syear;
char timestep[2];

/*---------------------------------
 *  Sediment Transport Parameters
 *---------------------------------*/
int ngrain;                     /* number of grain sizes */
double **grainpct;              /* % of each grain size */

/*--------------------
 *  Event Parameters
 *--------------------*/
int eventcounter;               /* keeps track of the events per epoch */
long *numberday;                /* parameter to store temperatly which day of the year the event occured */
int eventsperyear;              /* number of events that occur per year */
int *eventsnr;
double *floodvalue;
int floodcounter;
int eventnrflag;                /* indicates is number of events is given or is Qpeak which triggers events is given */

/*--------------------
 *  Delta Parameters
 *--------------------*/
int noutlet;                    /* number of outlets */
int minnoutlet;                 /* min number of outlets in a range */
int maxnoutlet;                 /* max number of outlets in a range */
double ***outletpct;            /* % of each outlet, HARDWIRED */
double *sedfilter;              /* filter variable for the delta outlets */
double **outletpcttotevents;    /* total average percentages per outlet */
int *nroutlets;
int outletmodelflag;            /* 1 if delta, 0 for no delta (no multiple outlets) */
int nooutletpctflag;            /* indicator if Q fractions are given by user or not */
int noutletflag;
int steadyoutletpctflag;        /* indicator if Q fractions has to be kept the same or change per event */
double **outletpctdummy;

/*----------------------------
 *  Sediment Load Parameters
 *----------------------------*/
double *alphabed;
double *C;
int annualhyperpycnalflag;
double Qshyperpycnal;
double hyperpycnalvalue;
double Cspeak;
double Qspeak;

/*------------------------
 *  Hydraulic Parameters
 *------------------------*/
double *depcof, *deppow;        /* d = (c * Q^f) */
double *velcof, *velpow;        /* v = (k * Q^m)  */
double *widcof, *widpow;        /* w = (a * Q^b) */
double *avgvel;                 /* Avg. river vel. (m/s) */
double *rslope;                 /* Riverbed avg. slope (deg) */

/*----------------------
 *  Terrain Parameters
 *----------------------*/
double elevbinsize;
int *nhypts;                    /* size of hypsometry array per epoch */
double *areabins;
double *elevbins;
double **hypsarea;
double **hypselev;
double *basinlength;            /* River basin length (meters) */
double *maxalt;                 /* Now computed in hydroreadhypsom.c  */
double *totalarea;              /* Now computed in hydroreadhypsom.c  */

/*-------------------------
 *  Earthquake Parameters
 *-------------------------*/
int *earthquakedatafile;      /* flag to indicate if there are earthquake files*/
int *quakeeventcounter;
int **quakeeventyear;         /*read in from file*/
int start_decay_year;
int end_decay_year;
double max_quake_erosion;         /* maximum erosion factor that can occur, set below*/
double quakethresholdenergy;      /* below this threshold quake will not be taken into account, set below)*/
double quakethresholdenergy_max;  /* max mag earthquake we are taking into account*/
double quakedampingfactor;        /* */
double **quakeeventenergy;       /*read in from file*/
double **quakeeventdistance;      /*read in from file*/
double **quakeeventduration;     /*read in from file*/

/*---------------------------------
 *  General Hydrologic Parameters
 *---------------------------------*/
int *distbins;                  /* days to discharge a bin */
int exceedflood;                /* flag for whether maxflood exceeded */
int floodtry;
double maxflood;                /* theoretical max flood size */
double *Rvol;                   /* storage capacity of lake/reservoir */
double *Ralt;                   /* alitude of the lake/reservoir */
double *Rarea;                  /* drainage area above the reservoir */
double *TE;                     /* Trapping efficiency */
double *TEsubbasin;
char *Rparamcheck;              /* indicator if alt. or drainage area is used as input */
double alphac;                  /*  OBSOLETE ?? */
double betac;                   /*  OBSOLETE ?? */
double rhosed;    /***  HARDWIRE  ***/
double rhowater;  /***  HARDWIRE  ***/

/*------------------------
 *  Rainfall Parameters
 *------------------------*/
double *alphag;         /***  experimental added to the input file  ***/
double *betag;         /***  experimental added to the input file  ***/
double pcr;            /***  HARDWIRE  ***/
double pmax;           /***  HARDWIRE  ***/
double MPrain;

/*--------------------------
 *  Groundwater Parameters
 *--------------------------*/
double *Ko;                     /* sat. hydr. cond. (mm/day) */
double *alphass;
double *betass;
double gwinitial;               /* initial GW storage (m^3) */
double gwlast;
double *gwmin, *gwmax;          /* min/max storage (m^3) */
double percentgw;               /* % of snow/ice melt to GW */
double *alphagwe;               /* evaporation coeff */
double *betagwe;                /* evaporation exponent */
int evaporationdatafile;

/*-----------------------------
 *  Snowmelt/Nival Parameters
 *-----------------------------*/
double *dryevap;
double Meltrate;
double Msnowstart, Msnowend;
double MPnival;

/*---------------------------
 *  Glacier Melt Parameters
 *---------------------------*/
int ELAindex;
double bigg, smallg, lastarea, initiallastarea;
double ela, lastela, initiallastela;
double Gmass;
double MPglacial;
double bethaexpo, bethaglacier;
double initialVolumelast, Volumeglacierarea;
double glacierareakmreset, glacierareakmpotential;

/*----------------------------
 *  Random Number Parameters
 *----------------------------*/
int nran;
double *ranarray;
double rmin, rmax;

/*-------------------------------------
 *  Random Number Parameters sediment
 *-------------------------------------*/
double *ranarraysediment;
int nransediment;

/*--------------------------
 *  Mass Check Parameters
 *--------------------------*/
double maxerr;
double totalmass;

/*-------------------------
 *  Geographic Parameters
 *-------------------------*/
double lat, lon;
double alpha3, alpha4, alpha5;
double alpha6, alpha7, alpha8;
double alpha9, alpha10, alpha11;
double k1, k2;
int *Qsbarformulaflag;

/*-----------------------
 *  Rainfile parameters
 *-----------------------*/
int raindatafile;

/*------------------------
 *  Lithology parameters
 *------------------------*/
double *lithology;

/*----------------------------
 *  Anthropogenic parameters
 *----------------------------*/
double *anthro;

/*---------------------------------------------
 *  Parameters to set Qsbarnew for each epoch
 *---------------------------------------------*/
int setstartmeanQandQs;         /* loop counter for each epoch (count to 3) */

/*-------------------------------------------------
 *  Variables to set ASCII write option ON or OFF
 *-------------------------------------------------*/
char asciioutput[MAXCHAR];

/*---------------------------------------------------------
 *  Set the file name and directory parameters + security
 *---------------------------------------------------------*/
char startname[80];
char directory[100];
char chrdump[80];
char commandlinearg[2][2048];
int globalparflag;
int lapserateflag;
int verbose;
double *Qgrandtotaltotoutlet, Qgrandtotaltot, **Qdummy, **Qgrandtotalperepoch;
double Qsgrandtotaltot, *Qsgrandtotaltotoutlet, Qpeakmax, TEtot;
double Qsoutletdummy;
double **Qsbartotoutlet;

/*---------------------------
 *  Start of HydroSetParams
 *---------------------------*/
void
hydrosetparams ()
{

/*---------------------------------------
 *  Hardwired Parameters for all Epochs
 *---------------------------------------*/
  rhowater = 1000.0;
  rhosed = 2670.0;
  alphac = 0.98;                /* saturation excess coeff */
  betac = 1.00;                 /* saturation excess exponent */

//    alphag       = -0.0001;    /* groundwater precip offset (m/day) */
//      betag        = 0.85;    /* groundwater precip slope */

  //   alphagwe     = 0.0020;     /* groundwater evap coeff (m/day)*/
  //   betagwe      = 1.0;     /* groundwater evap exponent */
  pmax = 0.400;                 /* precip need to reach max cond. (m/day) */
  Meltrate = 0.003;             /* (m/degC) */
  percentgw = 0.15;             /* percent of nival&ice as groundwater */
  pcr = 0.010;                  /* crit. precip for infilt. excess (m/day) */
  bethaexpo = 1.38;             /* volume-surface area exponent glaciers */
  bethaglacier = 31.11716;      /* volume-surface area multiplier glaciers */
  hyperpycnalvalue = 40.0;      /* (in kg/m3) rough value; above this value and river will go hyperpycnal */
  quakethresholdenergy = 6.0;   /* Below this value, quake will not be taken into account*/
  quakethresholdenergy_max = 9.0; /* above this value, quake will same as 9.0 */
  quakedampingfactor = 80.0;     /* If the distance is larger than x km, than the earth quake doesn't have any impact anymore */
  return;
}                               /* end of HydroSetParams */
