/*-------------------------------------------------------------------------------------------
 *  hydroparams.h
 *
 *	Author: Albert Kettner, March 2006
 *
 *  Define input parameters and common constants
 *
 *	Variable		Def.Location	Type		Units	Usage
 *	--------		------------	----		-----	-----
 *
 *-------------------------------------------------------------------------------------------*/

#if !defined( HYDROPARAMS_H )
#define HYDROPARAMS_H

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <time.h>

/*--------------------------
 *  Mathematical Constants
 *--------------------------*/
#define PI	(3.1415926535)

/*------------------------
 *  Conversion Constants
 *------------------------*/
#define dTOs	(86400.0)       /* days to seconds (# s/day) = 60.*60.*24. */
#define degTOr	(1.745329252e-2)        /* 2.0*PI/360.0 convert degrees to radians */
#define yTOs	(31536000.0)    /* years to seconds (# s/year) = 60.*60.*24.*365. */

/*--------------------
 *  Global Constants
 *--------------------*/
#define	FLAflag	(9999)          /* used in FLAindex, indicates no freezing T's on a given day */
#define DAYSINYEAR (365)

/*--------------------
 *  Global Functions
 *--------------------*/
#define sq(a)	((a)*(a))
#define mn(a,b) ((a)<(b)?(a):(b))
#define mx(a,b) ((a)>(b)?(a):(b))
#define rnd(a)  ((fmod(a,1.0)>(0.5)?((double)((int)(a)+1)):((double)((int)(a)))))

/*-------------------
 *  Time Parameters
 *-------------------*/
extern int ep, nepochs;
extern int yr, total_yr;
extern int *nyears, *syear;
extern char timestep[2];

/*---------------------------------
 *  Sediment Transport Parameters
 *---------------------------------*/
#define	maxgrn	(10)            /* maximum number of grain sizes */
extern int ngrain;                     /* number of grain sizes */
extern double **grainpct;              /* % of each grain size */

/*--------------------
 *  Event Parameters
 *--------------------*/
extern int eventcounter;               /* keeps track of the events per epoch */
extern long *numberday;                /* parameter to store temperatly which day of the year the event occured */
extern int eventsperyear;              /* number of events that occur per year */
extern int *eventsnr;
extern double *floodvalue;
extern int floodcounter;
extern int eventnrflag;                /* indicates is number of events is given or is Qpeak which triggers events is given */

/*--------------------
 *  Delta Parameters
 *--------------------*/
extern int noutlet;                    /* number of outlets */
extern int minnoutlet;                 /* min number of outlets in a range */
extern int maxnoutlet;                 /* max number of outlets in a range */
extern double ***outletpct;            /* % of each outlet, HARDWIRED */
extern double *sedfilter;              /* filter variable for the delta outlets */
extern double **outletpcttotevents;    /* total average percentages per outlet */
extern int *nroutlets;
extern int outletmodelflag;            /* 1 if delta, 0 for no delta (no multiple outlets) */
extern int nooutletpctflag;            /* indicator if Q fractions are given by user or not */
extern int noutletflag;
extern int steadyoutletpctflag;        /* indicator if Q fractions has to be kept the same or change per event */
extern double **outletpctdummy;

/*----------------------------
 *  Sediment Load Parameters
 *----------------------------*/
#define	trneff (0.1)
#define	anglerep (32.21)
extern double *alphabed;
extern double *C;
extern int annualhyperpycnalflag;
extern double Qshyperpycnal;
extern double hyperpycnalvalue;
extern double Cspeak;
extern double Qspeak;

/*------------------------
 *  Hydraulic Parameters
 *------------------------*/
extern double *depcof, *deppow;        /* d = (c * Q^f) */
extern double *velcof, *velpow;        /* v = (k * Q^m)  */
extern double *widcof, *widpow;        /* w = (a * Q^b) */
extern double *avgvel;                 /* Avg. river vel. (m/s) */
extern double *rslope;                 /* Riverbed avg. slope (deg) */

/*----------------------
 *  Terrain Parameters
 *----------------------*/
extern double elevbinsize;
extern int *nhypts;                    /* size of hypsometry array per epoch */
extern double *areabins;
extern double *elevbins;
extern double **hypsarea;
extern double **hypselev;
extern double *basinlength;            /* River basin length (meters) */
extern double *maxalt;                 /* Now computed in hydroreadhypsom.c  */
extern double *totalarea;              /* Now computed in hydroreadhypsom.c  */

/*-------------------------
 *  Earthquake Parameters
 *-------------------------*/
extern int *earthquakedatafile;      /* flag to indicate if there are earthquake files*/
extern int *quakeeventcounter;
extern int **quakeeventyear;         /*read in from file*/
extern int start_decay_year;
extern int end_decay_year;
extern double max_quake_erosion;      /*the maximum erosion multiply factor value, a constant */
extern double quakethresholdenergy;   /*threshold, below quake will not be taken into account*/
extern double quakethresholdenergy_max;  /* max mag earthquake we are taking into account*/
extern double quakedampingfactor;        /* */
extern double **quakeeventenergy;       /*read in from file*/
extern double **quakeeventdistance;      /*read in from file*/
extern double **quakeeventduration;     /*read in from file*/

/*---------------------------------
 *  General Hydrologic Parameters
 *---------------------------------*/
extern int *distbins;                  /* days to discharge a bin */
extern int exceedflood;                /* flag for whether maxflood exceeded */
extern int floodtry;
extern double maxflood;                /* theoretical max flood size */
extern double *Rvol;                   /* storage capacity of lake/reservoir */
extern double *Ralt;                   /* alitude of the lake/reservoir */
extern double *Rarea;                  /* drainage area above the reservoir */
extern double *TE;                     /* Trapping efficiency */
extern double *TEsubbasin;
extern char *Rparamcheck;              /* indicator if alt. or drainage area is used as input */
extern double alphac;                  /*  OBSOLETE ?? */
extern double betac;                   /*  OBSOLETE ?? */
extern double rhosed;    /***  HARDWIRE  ***/
extern double rhowater;  /***  HARDWIRE  ***/

/*------------------------
 *  Rainfall Parameters
 *------------------------*/
extern double *alphag;         /***  experimental added to the input file  ***/
extern double *betag;         /***  experimental added to the input file  ***/
extern double pcr;            /***  HARDWIRE  ***/
extern double pmax;           /***  HARDWIRE  ***/
extern double MPrain;

/*--------------------------
 *  Groundwater Parameters
 *--------------------------*/
extern double *Ko;                     /* sat. hydr. cond. (mm/day) */
extern double *alphass;
extern double *betass;
extern double gwinitial;               /* initial GW storage (m^3) */
extern double gwlast;
extern double *gwmin, *gwmax;          /* min/max storage (m^3) */
extern double percentgw;               /* % of snow/ice melt to GW */
extern double *alphagwe;               /* evaporation coeff */
extern double *betagwe;                /* evaporation exponent */
extern int evaporationdatafile;

/*-----------------------------
 *  Snowmelt/Nival Parameters
 *-----------------------------*/
extern double *dryevap;
extern double Meltrate;
extern double Msnowstart, Msnowend;
extern double MPnival;

/*---------------------------
 *  Glacier Melt Parameters
 *---------------------------*/
extern int ELAindex;
extern double bigg, smallg, lastarea, initiallastarea;
extern double ela, lastela, initiallastela;
extern double Gmass;
extern double MPglacial;
extern double bethaexpo, bethaglacier;
extern double initialVolumelast, Volumeglacierarea;
extern double glacierareakmreset, glacierareakmpotential;

/*----------------------------
 *  Random Number Parameters
 *----------------------------*/
#define	maxran	2200
#define	INIT_RAN_NUM_SEED (850)
extern int nran;
extern double *ranarray;
extern double rmin, rmax;

/*-------------------------------------
 *  Random Number Parameters sediment
 *-------------------------------------*/
extern double *ranarraysediment;
extern int nransediment;

/*--------------------------
 *  Mass Check Parameters
 *--------------------------*/
#define	masscheck (1e-5)        /* mass balance check (%) SHOULD BE 1e-5 */
extern double maxerr;
extern double totalmass;

/*-------------------------
 *  Geographic Parameters
 *-------------------------*/
extern double lat, lon;
extern double alpha3, alpha4, alpha5;
extern double alpha6, alpha7, alpha8;
extern double alpha9, alpha10, alpha11;
extern double k1, k2;
extern int *Qsbarformulaflag;

/*-----------------------
 *  Rainfile parameters
 *-----------------------*/
extern int raindatafile;

/*------------------------
 *  Lithology parameters
 *------------------------*/
extern double *lithology;

/*----------------------------
 *  Anthropogenic parameters
 *----------------------------*/
extern double *anthro;

/*---------------------------------------------
 *  Parameters to set Qsbarnew for each epoch
 *---------------------------------------------*/
extern int setstartmeanQandQs;         /* loop counter for each epoch (count to 3) */

/*-------------------------------------------------
 *  Variables to set ASCII write option ON or OFF
 *-------------------------------------------------*/
#define	MAXCHAR (5)
#define	ON "ON"
#define	OFF "OFF"
extern char asciioutput[MAXCHAR];

/*---------------------------------------------------------
 *  Set the file name and directory parameters + security
 *---------------------------------------------------------*/
#define	DUMMY "HYDRO"
#define verbosearg "VERBOSE"
extern char startname[80];
extern char directory[100];
extern char chrdump[80];
extern char commandlinearg[2][2048];
extern int globalparflag;
extern int lapserateflag;
extern int verbose;
extern double *Qgrandtotaltotoutlet, Qgrandtotaltot, **Qdummy, **Qgrandtotalperepoch;
extern double Qsgrandtotaltot, *Qsgrandtotaltotoutlet, Qpeakmax, TEtot;
extern double Qsoutletdummy;
extern double **Qsbartotoutlet;

#endif /*  */
