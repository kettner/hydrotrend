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
   
  
#include <stdio.h>
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
#define dTOs	(86400.0)		/* days to seconds (# s/day) = 60.*60.*24. */
#define degTOr	(1.745329252e-2)	/* 2.0*PI/360.0 convert degrees to radians */
#define yTOs	(31536000.0)	/* years to seconds (# s/year) = 60.*60.*24.*365. */
  
/*--------------------
 *  Global Constants
 *--------------------*/ 
#define	FLAflag	(9999)		/* used in FLAindex, indicates no freezing T's on a given day */
#define TMLEN (100)		/* length of the time stamp */
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
int nepochs, ep;
int yr, total_yr;
int *nyears, *syear;
char timestep[2];

/*---------------------------------
 *  Sediment Transport Parameters
 *---------------------------------*/ 
#define	maxgrn	(10)				/* maximum number of grain sizes */
int ngrain;                     /* number of grain sizes */
double **grainpct;             /* % of each grain size */

/*--------------------
 *  Event Parameters
 *--------------------*/ 
int eventcounter;               /* keeps track of the events per epoch */
long *numberday;               /* parameter to store temperatly which day of the year the event occured */
int eventsperyear;             /* number of events that occur per year */
int *eventsnr;
double *floodvalue;
int floodcounter;
int eventnrflag;               /* indicates is number of events is given or is Qpeak which triggers events is given */

/*--------------------
 *  Delta Parameters
 *--------------------*/ 
int noutlet;                    /* number of outlets */
int minnoutlet;                /* min number of outlets in a range */
int maxnoutlet;                /* max number of outlets in a range */
double ***outletpct;           /* % of each outlet, HARDWIRED */
double *sedfilter;             /* filter variable for the delta outlets */
double **outletpcttotevents;   /* total average percentages per outlet */
int *nroutlets;
int outletmodelflag;           /* 1 if delta, 0 for no delta (no multiple outlets) */
int nooutletpctflag;           /* indicator if Q fractions are given by user or not */
int noutletflag;
int steadyoutletpctflag;       /* indicator if Q fractions has to be kept the same or change per event */
double **outletpctdummy;

/*----------------------------
 *  Sediment Load Parameters
 *----------------------------*/ 
#define	alphabed  (1.0)
#define	trneff (0.1)
#define	anglerep (32.21)
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
double *velcof, *velpow;       /* v = (k * Q^m)  */
double *widcof, *widpow;       /* w = (a * Q^b) */
double *avgvel;                /* Avg. river vel. (m/s) */
double *rslope;                /* Riverbed avg. slope (deg) */

/*----------------------
 *  Terrain Parameters
 *----------------------*/ 
double elevbinsize;
int nhypts;                    /* size of hypsometry array per epoch */
double *areabins;
double *elevbins;
double *hypsarea;
double *hypselev;
double *basinlength;           /* River basin length (meters) */
double maxalt;                 /* Now computed in hydroreadhypsom.c  */
double totalarea;              /* Now computed in hydroreadhypsom.c  */

/*---------------------------------
 *  General Hydrologic Parameters
 *---------------------------------*/ 
int *distbins;                  /* days to discharge a bin */
int exceedflood;               /* flag for whether maxflood exceeded */
int floodtry;
double maxflood;               /* theoretical max flood size */
double *Rvol;                  /* storage capacity of lake/reservoir */
double *Ralt;                  /* alitude of the lake/reservoir */
double *Rarea;                 /* drainage area above the reservoir */
double *TE;                    /* Trapping efficiency */
double *TEsubbasin;
char *Rparamcheck;             /* indicator if alt. or drainage area is used as input */
double alphac;                 /*  OBSOLETE ?? */
double betac;                  /*  OBSOLETE ?? */
double rhosed;    /***  HARDWIRE  ***/
double rhowater;  /***  HARDWIRE  ***/

/*------------------------
 *  Rainfall Parameters
 *------------------------*/ 
double alphag;          /***  HARDWIRE  ***/
double betag;          /***  HARDWIRE  ***/
double pcr;            /***  HARDWIRE  ***/
double pmax;           /***  HARDWIRE  ***/
double MPrain;

/*--------------------------
 *  Groundwater Parameters
 *--------------------------*/ 
double *Ko;                     /* sat. hydr. cond. (mm/day) */
double *alphass;
double *betass;
double gwinitial;              /* initial GW storage (m^3) */
double gwlast;
double *gwmin, *gwmax;         /* min/max storage (m^3) */
double percentgw;              /* % of snow/ice melt to GW */
double alphagwe;               /* evaporation coeff */
double betagwe;                /* evaporation exponent */
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
#define	maxran	2200
#define	INIT_RAN_NUM_SEED (850)
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
#define	masscheck (1e-5)		/* mass balance check (%) SHOULD BE 1e-5 */
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
#define	MAXCHAR (5)
#define	ON "ON"
#define	OFF "OFF"
char asciioutput[MAXCHAR];

/*---------------------------------------------------------
 *  Set the file name and directory parameters + security
 *---------------------------------------------------------*/ 
#define	DUMMY "HYDRO"
#define verbosearg "VERBOSE"
char startname[80];
char directory[100];
char chrdump[80];
char commandlinearg[2][20];
int globalparflag;
int lapserateflag;
int verbose;
double *Qgrandtotaltotoutlet, Qgrandtotaltot, **Qdummy, **Qgrandtotalperepoch;
double Qsgrandtotaltot, *Qsgrandtotaltotoutlet, Qpeakmax, TEtot;
double Qsoutletdummy;
double **Qsbartotoutlet;

