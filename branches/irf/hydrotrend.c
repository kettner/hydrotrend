/*-------------------------------------------------------------------------------------------
 *	hydrotrend.c
 *
 *	Author: Albert Kettner, March 2006
 *
 *	HydroTrend.c is the backbone of the program. This file is calling all the subroutines
 *	HydroTrend is a climate-driven hydrological transport model that generates synthetic water
 *	discharge and sediment load of any river in the world. The model is discussed in the next
 *	papers:
 *
 * 		Kettner, A.J., Syvitski, J.P.M., submitted 2006. HydroTrend version 3.0: a Climate-Driven 
 *	Hydrological Transport Model that Simulates Discharge and Sediment Load leaving a River
 *	System. Computers and Geosciences.
 *		Syvitski, J.P., Morehead, M.D., Nicholson, M., 1998. HydroTrend: A climate-Driven Hydrologic-
 *	Transport model for Predicting discharge and sediment load to lakes or Oceans. Computers and
 *	Geosciences 24, 51-68.
 *		Syvitski, J.P.M., Alcott, J.M., 1995. RIVER3: Simulation of water and sediment river discharge
 *	from climate and drainage basin variables. Computers and Geosciences 21, 89-101.
 *	
 *	After an exe file is created (hydrotrend.exe) HydroTrend can be start in the next modes:
 *	1) "./hydrotrend"
 *		No filename is given so all files start with "HYDRO" followed by their extention.
 *		Old "HYDRO" files will be overwritten..
 *	2) "./hydrotrend EXAMPLE"
 *		Filename will start with "EXAMPLE" or any name you gave it, followed by its extention.
 *		Old "EXAMPLE" files will be overwritten.
 *	3) "./hydrotrend EXAMPLE VERBOSE"
 *		Filename will start with "EXAMPLE" or any name you gave it, followed by its extention.
 *		Old "EXAMPLE" files will be overwritten.
 *		Program window will show which subroutines are called during the simulation with the VERBOSE option.
 *
 *	Variable		Def.Location	Type		Units	Usage
 *	--------		------------	----		-----	-----
 *	**argv		HydroTrend.c    char		-		command line string capture variable
 *	argc			HydroTrend.c    int			-		command line word counter
 *	err			various			int			-		error flag, halts program
 *	dumdbl		various			double		-		temporary double
 *	i			various         int			-		temporary loop counter
 *	ii			various			int			-		temporary loop counter
 *	jj			various			int			-		temporary loop counter
 *	setstartmeanQandQs HydroParams.h int		-		temporary loop counter
 *	logarea		HydroTrend.c	double		m^2		log10 of the basin area
 *	lyear		HydroTrend.c	int			yr		last year of an epoch
 *	maxnran		HydroTrend.c	int			-		max # of random number used/year
 *	pst[TMLEN]	HydroTrend.c	char		-		time stamp
 *	Qgrandtotaltot HydroTrend.c	double		m^3/s	discharge for all run
 *	Qgrandtotalperepoch HydroTrend.c double	m^3/s	discharge per epoch
 *	Qpeakmax		HydroTrend.c	double		m^3/s	all time max. peak for all run
 *	Qsbarnewtot	HydroTrend.c	double		kg/s	total of all Qsbarnew's
 *	Qsgrandtotaltot HydroTrend.c	double		kg/s	total of all Qsbrandtotal's
 *	tloc			HydroTrend.c	struct		-		time stamp structure
 *	tm timept	HydroTrend.c	struct		-		time stamp structure
 *
 *-------------------------------------------------------------------------------------------*/

#include "hydrofree_mem.h"
#include "hydroclimate.h"
#include "hydroinout.h"
#include "hydroparams.h"
#include "hydrotimeser.h"
#include "hydroalloc_mem.h"
#include "hydroreadclimate.h"
#include "hydrotrend.h"
#include <stdlib.h>
#include <time.h>
#include <string.h>

/*---------------------
 *  Start the program
 *---------------------*/
int main(int argc,char **argv)
{

/*-------------------
 *  Local Variables
 *-------------------*/
char    pst[TMLEN];
int     err, ii, lyear, maxnran, p,k,x;
long totaldays;
double 	*totpercentageQ, logarea, baseflowpercentage;
struct  tm *timeptr;
gw_rainfall_etc* gw_rain;
time_t  tloc;

/*------------------------
 *  Initialize Variables
 *------------------------*/
err             = 0;
verbose			= 0;
maxnran         = 0;
maxerr          = 0.0;
totalmass       = 0.0;
globalparflag	= 0;
Qgrandtotaltot  = 0.0;
Qsgrandtotaltot = 0.0;
TEtot			= 0.0;
gw_rain = (gw_rainfall_etc*)malloc(sizeof(gw_rainfall_etc));

/*----------------------
 *  Get the start time
 *----------------------*/
time( &tloc );
timeptr = localtime(&tloc);

/*-----------------------------------------------------------------
 *  Check the command line input; get file name or directory name
 *-----------------------------------------------------------------*/
err = hydrocommandline(&argc, argv);
if (err) {
    fprintf( stderr, " ERROR in HydroCommandLine: HydroTrend Aborted \n\n" );
    exit(1);
}

/*--------------------------------
 *  Read the main input file.
 *  This reads all of the epochs,
 *  at the very begining
 *--------------------------------*/
if (verbose) printf("Reading input data from: HYDRO.IN... \n");
err = hydroreadinput();
if (err) {
    fprintf( stderr, " ERROR in HydroReadInput: HydroTrend Aborted \n\n" );
    exit(1);
}

/*--------------------
 *  Allowcate memory
 *--------------------*/
totpercentageQ	= malloc1d( ep, double );

/*---------------------------------------
 *  Set the output path name + filename
 *---------------------------------------*/
strcpy(startname,directory);
strcat(startname,commandlinearg[1]);

/*---------------------
 *  Open the log file
 *---------------------*/
if (verbose) printf("Opening the log file (to append)... \n");
strcpy(ffnamelog,startname);
strcat(ffnamelog,fnamelog);
if ((fidlog = fopen(ffnamelog,"w+")) == NULL) {
    printf("  HydroTrend WARNING: Unable to open the log file %s \n", ffnamelog);
    printf("     non-fatal error, continuing. \n\n");
}

/*--------------------------------
 *  Print the program start time
 *--------------------------------*/
strftime( pst, TMLEN, "%X  %x", timeptr );
fprintf(fidlog," =====================================\n\n");
fprintf(fidlog," ----- HydroTrend 3.0 Model Run ----- \n\n");
fprintf(fidlog," \tStart: %s \n\n", pst);

/*---------------------------------
 *  Read the hypsometric curve
 *  and set maxalt and totalarea.
 *---------------------------------*/
if (verbose) printf("Reading hypsometric data from: HYDRO.HYPS... \n");
err = hydroreadhypsom();
if (err) {
    fprintf( stderr, " ERROR in HydroReadHypsom: HydroTrend Aborted \n\n" );
    fprintf( fidlog, " ERROR in HydroReadHypsom: HydroTrend Aborted \n\n" );
    exit(1);
}
	
/*---------------------------------------------
 *  Read climate file if its their.
 *---------------------------------------------*/
if (verbose) printf("Reading live climate data files... \n");
err = hydroreadclimate(gw_rain);
if (err) {
    fprintf( stderr, " ERROR in HydroReadClimate: HydroTrend Aborted \n\n" );
    fprintf( fidlog, " ERROR in HydroReadClimate: HydroTrend Aborted \n\n" );
    exit(1);
}

/*--------------------------------
 *  Set the hardwired parameters
 *--------------------------------*/
if (verbose) printf("Setting hardwired parameters... \n");
hydrosetparams();
if (err) {
    fprintf( stderr, " ERROR in HydroSetParams: HydroTrend Aborted \n\n" );
    exit(1);
}

/*------------------------------------------
 *  Set global values for those parameters
 *  which doesn't have any input
 *------------------------------------------*/
if ( globalparflag > 0 ){
	if (verbose) printf("Set global values for not filed out input parameters... \n");
	err = hydrosetglobalpar();
	if (err) {
    	fprintf( stderr, " ERROR in HydroSetGlobalPar: HydroTrend Aborted \n\n" );
	    fprintf( fidlog, " ERROR in HydroSetGlobalPar: HydroTrend Aborted \n\n" );
    	exit(1);
	}
}

/*---------------------------------------------
 *  Check all of the input values.
 *  This also checks to make sure the climate
 *  variables match at the epoch breaks.
 *---------------------------------------------*/
if (verbose) printf("Checking all input parameters... \n");
err = hydrocheckinput();
if (err) {
    fprintf( stderr, " ERROR in HydroCheckInput: HydroTrend Aborted \n\n" );
    fprintf( fidlog, " ERROR in HydroCheckInput: HydroTrend Aborted \n\n" );
    exit(1);
}

/*-----------------------
 *  Open the data files
 *-----------------------*/
if (verbose) printf("Opening output data files... \n");
err = hydroopenfiles();
if (err) {
    fprintf( stderr, " ERROR in HydroOpenFiles: HydroTrend Aborted \n\n" );
    fprintf( fidlog, " ERROR in HydroOpenFiles: HydroTrend Aborted \n\n" );
    exit(1);
}

/*--------------------------
 *  Run each epoch of data
 *--------------------------*/
if (verbose) printf(" \nStarting epoch loop... \n"); 
for (ep=0; ep<nepochs; ep++) {
	total_yr += nyears[ep];
	ranarray = malloc1d( 2*maxran, double );
	ranarraysediment = malloc1d( 2*nyears[ep] , double );
	
/*-----------------------------------------------------------------
 *  Read Qs constant parameters set by geolocation of river mouth
 *-----------------------------------------------------------------*/
if (verbose) printf("Calling HydroSetGeoParams... \n");
err = hydrosetgeoparams( gw_rain );
if (err) {
    fprintf( stderr, " ERROR in HydroSetGeoParams: HydroTrend Aborted \n\n" );
    fprintf( fidlog, " ERROR in HydroSetGeoParams: HydroTrend Aborted \n\n" );
    exit(1);
}
	
/*----------------------------------------------------------------------------
 *  Run each epoch 5 times. This to set a couple of parameters or averages of
 *	parameters with exactly the same random numbers.
 *  If setstartmeanQandQs ==:
 *  0) calculate the long term mean discharge (Qbartotal[ep]); daily discharge
 *     is calculated without any baseflow.
 *  1) calculate daily discharge (taking baseflow into account) + calculate
 *     discharge multiple outlets.
 *  2) calculate the long term mean suspended sediment load (Qsbartot[ep]).
 *  3) calculate the constant of proportionality so that the mean of the sum
 *     of daily suspended sediment is equal to the long term mean suspended
 *     sediment (Qsbarnew[ep]). Glacier created sediment is added to the long
 *     term sediment during this step.
 *  4) calculate the daily suspended sediment (Qs[i]); write to output files.
 * 
 *----------------------------------------------------------------------------*/
for ( setstartmeanQandQs = 0; setstartmeanQandQs < 5; setstartmeanQandQs++ ){
	yr = syear[ep];
	if (setstartmeanQandQs == 0){
		fprintf( stderr, " Calculate mean discharge, epoch: %d\n",(ep+1));
		fprintf( fidlog, " Calculate mean discharge, epoch: %d\n",(ep+1));
	}
	if (setstartmeanQandQs == 1){
		fprintf( stderr, " Calculate daily discharge, epoch: %d\n",(ep+1));
		fprintf( fidlog, " Calculate daily discharge, epoch: %d\n",(ep+1));
	}
	if (setstartmeanQandQs == 2){
		fprintf( stderr, " Calculate mean suspended sediment load, epoch: %d\n",(ep+1));
		fprintf( fidlog, " Calculate mean suspended sediment load, epoch: %d\n",(ep+1));
	}
	if (setstartmeanQandQs == 4){	
		fprintf( stderr, " Calculate daily suspended sediment load, epoch: %d\n",(ep+1));
		fprintf( fidlog, " Calculate daily suspended sediment load, epoch: %d\n",(ep+1));
	}
	
/*---------------------------------------------------
 *  Free memory for possible multiple outlet module
 *---------------------------------------------------*/
	if ( ( ep > 0 && setstartmeanQandQs == 0 ))
		hydrofreememoutlet(nyears[ep-1]);		
	if ( ( ep > 0 && setstartmeanQandQs == 0 ))
		hydrofreememoutlet1(ep);

/*-------------------------------------------------------
 *  Allocate memory for possible multiple outlet module
 *-------------------------------------------------------*/
	if ( setstartmeanQandQs == 0 )
		hydroallocmemoutlet(ep, nepochs);
	if ( setstartmeanQandQs == 1 )
		hydroallocmemoutlet1(ep, nepochs);

/*----------------------------------
 *  Initialize Variables per epoch
 *----------------------------------*/
	if( (nooutletpctflag == 0) && ( setstartmeanQandQs > 0) )
		for (p=0; p<maxnoutlet; p++){
			Qdummy[ep][p] = 0.0;		
			for (k=0; k<eventsnr[ep]; k++){
				Qbar[ep][p][k]   = 1.0;	    /* Just a start value */
				outletpct[p][ep][k] = outletpctdummy[p][ep];
				Qbar[ep][p][k] = outletpct[p][ep][k] * Qbartotal[ep];
			}
		}
	if ( setstartmeanQandQs == 0 ){
		for (p=0; p<nyears[ep]; p++)
			for (k=0; k<daysiy; k++)
				Qpeakfloodtemp[p][k] = 0.0;
		Qicetotal[ep] = 0.0;
		Qsglaciertotal[ep] = 0.0;
		GlacierMstorage[ep] = 0.0;
		fractionglaciersediment[ep] = 0.0;
		GlacierMinput[ep] = 0.0;
	}

	 /*-----------------------------------------------------------------
	  *  Start new random number sequence.
	  *  Get 'maxran' worth of random numbers and pluck them as needed
	  *  nran counts through the numbers stored in ranarray
	  *-----------------------------------------------------------------*/
		if (verbose) printf("Calling HydroRandom... \n");
		err = hydrorandom();
		if (err) {
			fprintf( stderr, " ERROR in HydroRandom: HydroTrend Aborted \n\n" );
			fprintf( fidlog, " ERROR in HydroRandom: HydroTrend Aborted \n\n" );
			exit(1);
		}
		nran=0;

 /*-----------------------------------------------------------------
  *  Start new random number sequence for sediment.
  *  Get 'maxran' worth of random numbers and pluck them as needed
  *  nransediment counts through the numbers stored in ranarraysediment
  *-----------------------------------------------------------------*/
		if (verbose) printf("Calling HydroRandomsed... \n");
		err = hydrorandomsediment();
		if (err) {
			fprintf( stderr, " ERROR in HydroRandomsed: HydroTrend Aborted \n\n" );
			fprintf( fidlog, " ERROR in HydroRandomsed: HydroTrend Aborted \n\n" );
			exit(1);
		}
		nransediment=0;

/*-------------------------------------------------
 *  Initialize Variables per loop through program
 *-------------------------------------------------*/
        Qpeakall[ep] = 0.0;
        eventcounter = 0;
		for (p=0; p<maxnoutlet; p++)
        	Qpeakperoutletall[ep][p] = 0.0;

/*-------------------------------------------
 *  Set the number of outlets for 10 events
 *  if number of outlets is not specified, 
 *  or given in a range.
 *-------------------------------------------*/
		if ( noutletflag == 1 && setstartmeanQandQs == 1){
			for (x=0; x<eventsnr[ep]; x++){
				nroutlets[x] = hydrosetnumberoutlet(x);
				noutlet = nroutlets[x];
				if (verbose) printf("Calling hydrooutletfraction... \n");
				err = hydrooutletfraction(x);
				if (err) {
					fprintf( stderr, " ERROR in HydroOutletFraction (HydroOutlet): HydroTrend Aborted \n\n" );
					fprintf( fidlog, " ERROR in HydroOutletFraction (HydroOutlet): HydroTrend Aborted \n\n" );
					exit(1);
				}				
			}
			noutlet = nroutlets[eventcounter];
		}
		if ( noutletflag == 0 && outletmodelflag == 1 && steadyoutletpctflag == 1 && setstartmeanQandQs == 1)
			for (x=0; x<eventsnr[ep]; x++){				
				nroutlets[x] = noutlet;
				if (verbose) printf("Calling hydrooutletfraction... \n");
				err = hydrooutletfraction(x);
				if (err) {
					fprintf( stderr, " ERROR in HydroOutletFraction (HydroOutlet): HydroTrend Aborted \n\n" );
					fprintf( fidlog, " ERROR in HydroOutletFraction (HydroOutlet): HydroTrend Aborted \n\n" );
					exit(1);
				}
			}

/*-----------------------------------------------------------------------
 *  Set the discharge fraction per outlet if there are multiple outlets
 *  and if the discharge fraction is not set yet in the outlet file
 *-----------------------------------------------------------------------*/
	if ( outletmodelflag == 1 && nooutletpctflag == 1 && steadyoutletpctflag == 0 && setstartmeanQandQs == 1){
		x=0;		
		err = hydrooutletfraction(x);
		if (verbose) printf("Calling hydrooutletfraction... \n");
		if (err) {
			fprintf( stderr, " ERROR in HydroOutletFraction (HydroOutlet): HydroTrend Aborted \n\n" );
			fprintf( fidlog, " ERROR in HydroOutletFraction (HydroOutlet): HydroTrend Aborted \n\n" );
			exit(1);
		}		
    }

   /*---------------------------------------------------------
    *  Calculate the maximum predicted flood size.
    *  Since the basin area does not change within an epoch,
    *  only need to do this once for each epoch.
    *
    *  Maximum Flood Size for a Basin:
    *  Mulder T. and Syvitski J.P.M., 1995.
    *  Turbidity currents generated at river mouths
    *  during exceptional discharge to the world oceans.
    *  Journal of Geology, 103: 285-298.
    *
    *  The equation wants area in km^2
    *
    *---------------------------------------------------------*/
        if (verbose) printf("  Epoch = %d \n", ep+1);
        logarea  = log10( totalarea / 1e6 );
        maxflood = pow(10.0, (2.084 + 0.865*logarea - 0.07*sq(logarea)));

   /*---------------------------------------------------
    *  Create flood wave attenuation "shoulder" params
    *---------------------------------------------------*/
        if (verbose) printf("Calling HydroShoulder... \n");
        err = hydroshoulder();
        if (err) {
            fprintf( stderr, " ERROR in HydroShoulder: HydroTrend Aborted \n\n" );
            fprintf( fidlog, " ERROR in HydroShoulder: HydroTrend Aborted \n\n" );
            exit(1);
        }

   /*---------------------------------------
    *  Loop through each year of the epoch
    *---------------------------------------*/
        lyear = syear[ep] + nyears[ep];
        for (yr = syear[ep]; yr < lyear; yr++) {

      /*-------------------------------------------------------------
       *  Reset annual arrays tracking carryover from previous year
       *-------------------------------------------------------------*/
            if (ep > 0 || yr != syear[ep]) 
		        for (ii=0; ii<maxday-daysiy; ii++) {
                    Qrainwrap[ii]  = Qrain[ii+daysiy];
                    Qicewrap[ii]   = Qice[ii+daysiy];
                    Qnivalwrap[ii] = Qnival[ii+daysiy];
                    Qsswrap[ii]    = Qnival[ii+daysiy];
		        }
        	    else for (ii=0; ii < maxday-daysiy; ii++) {
                    Qrainwrap[ii]  = 0.0;
                    Qicewrap[ii]   = 0.0;
                    Qnivalwrap[ii] = 0.0;
                    Qsswrap[ii]    = 0.0;
                }
                
      /*---------------------------------------
       *  Keep track of groundwater pool size
       *---------------------------------------*/
            gwlast = gwstore[daysiy-1];

      /*--------------------------------------------------
       *  In case the model exceeds the maximum flood,
       *  loop through a number of times.  This normally
       *  gets the maximum modeled flood to be below
       *  the maximum predicted flood.
       *--------------------------------------------------*/
            exceedflood = 1;
            floodtry = 0;
            while (exceedflood > 0 && floodtry < 10) {

         /*------------------------------
          *  Reset annual arrays/values
          *------------------------------*/
	        for (ii=0; ii<maxday; ii++) {
                Qrain[ii]	= 0.0;
                Qice[ii]	= 0.0;
                Qnival[ii]	= 0.0;
                Qss[ii]	= 0.0;
                Qsumtot[ii]	= 0.0;
                for (p=0; p<maxnoutlet; p++)
 	              	Qsum[ii][p] = 0.0;
	        }
            for (ii=0; ii<daysiy; ii++) {
              	if (outletmodelflag == 1)
	               	for (p=0; p<maxnoutlet; p++){
						Csoutlet[ii][p]		= 0.0;
						Qboutlet[ii][p]		= 0.0;
						Qsoutlet[ii][p]		= 0.0;
	               	}
                Cs[ii] = 0.0;
                Qb[ii] = 0.0;
                Qs[ii] = 0.0;	
                Qicetogw[ii]	= 0.0;
                Qnivaltogw[ii]	= 0.0;
                Pdaily[ii]		= 0.0;
                Tdaily[ii]		= 0.0;
                gwstore[ii]		= 0.0;
            }
            Enivalannual = 0.0;
            Eiceannual   = 0.0;
	
	 /*---------------------------
	  *  Set the initial GW pool
	  *---------------------------*/
                if (yr == syear[0]) {
                    gwstore[0] = gwinitial;
                    gwlast = gwinitial;
                }
                if (yr != syear[0])
                    gwstore[0] = gwlast;

	 /*-----------------------------------------------------------------
	  *  Start new random number sequence.
	  *  Get 'maxran' worth of random numbers and pluck them as needed
	  *  nran counts through the numbers stored in ranarray
	  *-----------------------------------------------------------------*/
				rmin = -6.0;
				while (rmin < -5.0 || rmax > 5.0){
					if (verbose) printf("Calling HydroRandom... \n");
					err = hydrorandom();
					if (err) {
						fprintf( stderr, " ERROR in HydroRandom: HydroTrend Aborted \n\n" );
						fprintf( fidlog, " ERROR in HydroRandom: HydroTrend Aborted \n\n" );
						exit(1);
					}
				}

	 /*---------------------------------
	  *  Set the climate for this year
	  *---------------------------------*/
                if (verbose) printf("Calling HydroClimate... \n");
	        err = hydroclimate( gw_rain );
	        if (err) {
	           fprintf( stderr, " ERROR in HydroClimate: HydroTrend Aborted \n\n" );
	           fprintf( fidlog, " ERROR in HydroClimate: HydroTrend Aborted \n\n" );
	           exit(1);
	        }

	 /*-------------------------------------
	  *  Calculate weather for each day of
	  *  the year, for each hypsometric bin
	  *-------------------------------------*/
                if (verbose) printf("Calling HydroWeather... \n");
	        err = hydroweather(gw_rain);
	        if (err) {
	            fprintf( stderr, " ERROR in HydroWeather: HydroTrend Aborted \n\n" );
	            fprintf( fidlog, " ERROR in HydroWeather: HydroTrend Aborted \n\n" );
	            exit(1);
	        }

	 /*-------------------------------------------------
	  *  Calculate elev grid and T, for each elevation
	  *-------------------------------------------------*/
                if (verbose) printf("Calling HydroHypsom... \n");
	        err = hydrohypsom();
	        if (err) {
	            fprintf( stderr, " ERROR in HydroHypsom: HydroTrend Aborted \n\n" );
	            fprintf( fidlog, " ERROR in HydroHypsom: HydroTrend Aborted \n\n" );
	            exit(1);
	        }
			
	 /*-------------------------------------------------
	  *  Calculate ice accumulation/melt for each day.
	  *  This is done before HydroRain or HydroSnow to
	  *  find the glaciated area
	  *-------------------------------------------------*/
                if (verbose) printf("Calling HydroGlacial... \n");
	        err = hydroglacial();
	        if (err) {
	            fprintf( stderr, " ERROR in HydroGlacial: HydroTrend Aborted \n\n" );
	            fprintf( fidlog, " ERROR in HydroGlacial: HydroTrend Aborted \n\n" );
	            exit(1);
	        }

	 /*------------------------------------------
	  *  Calculate snow fall/melt for each day.
	  *  This is done before HydroRain to find
	  *  the "snow" area for each day
	  *------------------------------------------*/
                if (verbose) printf("Calling HydroSnow... \n");
	        err = hydrosnow();
                if (err) {
	            fprintf( stderr, " ERROR in HydroSnow: HydroTrend Aborted \n\n" );
	            fprintf( fidlog, " ERROR in HydroSnow: HydroTrend Aborted \n\n" );
	            exit(1);
	        }

	 /*---------------------------------
	  *  Calculate precip for each day
	  *---------------------------------*/
                if (verbose) printf("Calling HydroRain... \n");
	        err = hydrorain();
	        if (err) {
	            fprintf( stderr, " ERROR in HydroRain: HydroTrend Aborted \n\n" );
	            fprintf( fidlog, " ERROR in HydroRain: HydroTrend Aborted \n\n" );
	            exit(1);
	        }

	 /*------------------------------------------------------------
	  *  Add the component flows and find peakflow for the year.
	  *  Store the lagged overflow and groundwater pool size for
	  *  the following year.
	  *------------------------------------------------------------*/
                if (verbose) printf("Calling HydroSumFlow... \n");
	        err = hydrosumflow();
	        if (err) {
	            fprintf( stderr, " ERROR in HydroSumFlow: HydroTrend Aborted \n\n" );
	            fprintf( fidlog, " ERROR in HydroSumFlow: HydroTrend Aborted \n\n" );
	            exit(1);
	        }

	 /*-----------------------------------------
      *  Is flood peak less than max allowed ?
      *-----------------------------------------*/
                if (Qpeak < maxflood)
                    exceedflood = 0;
                else {
                	if (setstartmeanQandQs == 4){
	                    fprintf( stderr, "\n FLOOD WARNING: epoch %d, year %d \n", ep+1, yr );
    	                fprintf( stderr, " \t Max.Allowed %.1f, Qpeak %.1f, retry # %d \n", maxflood, Qpeak, floodtry );
                	}
                	floodtry++;
					if (floodtry < 10){
	                    Qgrandtotal[ep] -= Qtotal;
					}
                }
             }  /* end flood exceedence while loop */

      /*-------------------------------------------------------
       *  Track the max flow and if we still exceed the max
       *  predicted flood, send warning flag, but keep going
       *-------------------------------------------------------*/
              Qpeakall[ep] = mx( Qpeak, Qpeakall[ep] );
              if ( outletmodelflag == 1 )
	              for (p=0; p<maxnoutlet; p++)
					Qpeakperoutletall[ep][p] = mx (Qpeakperoutlet[p], Qpeakperoutletall[ep][p] );
              if ( exceedflood > 0 && setstartmeanQandQs == 4) {
	          fprintf( stderr, "   FLOOD WARNING: the maximum predicted flood size");
	          fprintf( stderr, " has been exceeded. \n");
	          fprintf( stderr, "      Epoch %d, year %d \n", ep+1, yr );
	          fprintf( stderr, "      Maximum predicted flood %g (m^3/s) \n", maxflood );
	          fprintf( stderr, "      Modeled flood peak %g (m^3/s) \n", Qpeak );

	          fprintf( fidlog, "   FLOOD WARNING: the maximum predicted flood size");
	          fprintf( fidlog, " has been exceeded. \n");
	          fprintf( fidlog, "      Epoch %d, year %d \n", ep+1, yr );
	          fprintf( fidlog, "      Maximum predicted flood %g (m^3/s) \n", maxflood );
	          fprintf( fidlog, "      Modeled flood peak %g (m^3/s) \n", Qpeak );
              }
        
/*-------------------------------------------------------------
 *  Calculate the maximal events = (biggest Qpeaks per epoch)
 *  for channel switching at delta if option is turned on
 *------------------------------------------------------------- */
				if ( setstartmeanQandQs == 1 && eventnrflag == 0 ){
					if (verbose) printf("Calling HydroMaxEvents... \n");
					err = hydromaxevents();
					if (err) {
						fprintf( stderr, " ERROR in HydroMaxEvents: HydroTrend Aborted \n\n" );
						fprintf( fidlog, " ERROR in HydroMaxEvents: HydroTrend Aborted \n\n" );
						exit(1);
					}
				}

      /*---------------------------
       *  Calculate sediment load 
       *---------------------------*/
			if ( setstartmeanQandQs > 1 ){
              if (verbose) printf("Calling HydroSedLoad... \n");
              err = hydrosedload( gw_rain );
              if (err) {
	          fprintf( stderr, " ERROR in HydroSedLoad: HydroTrend Aborted \n\n" );
	          fprintf( fidlog, " ERROR in HydroSedLoad: HydroTrend Aborted \n\n" );
	          exit(1);
              }
			}

           /*------------------------------------------
            *  Output the binary daily discharge file
            *------------------------------------------*/
              if (setstartmeanQandQs == 4){
                  if (verbose) printf("Calling HydroOutput... \n");
					err = hydrooutput ();
                  if (err) {
	              fprintf( stderr, " ERROR in HydroOutput: HydroTrend Aborted \n\n" );
	              fprintf( fidlog, " ERROR in HydroOutput: HydroTrend Aborted \n\n" );
	              exit(1);
                  }              

           /*------------------------------------------------
            *  Print out annual values to trend (trn) files
            *------------------------------------------------*/ 
                  if (verbose) printf("Calling HydroPrintAnnual... \n");
                  err = hydroprintannual();
                  if (err) {
	              fprintf( stderr, " WARNING in HydroPrintTrend: Continuing \n\n" );
	              fprintf( fidlog, " WARNING in HydroPrintTrend: Continuing \n\n" );
                  }                 
              }  /* end setstartmeanQandQs == 4 statement */

      /*-------------------------------------------------------
       *  Did random number generator create enough values ?
       *-------------------------------------------------------*/
              if (nran >= maxran){
                  fprintf( stderr,"\n\n HydroTrend ERROR: nran exceeded maxran.\n");
                  fprintf( stderr,"\t increase maxran in HydroParams.h. \n");
                  fprintf( stderr,"\t nran = %d, maxran = %d \n\n", nran, maxran);
	              exit(1);
              }            
            }  /* end year loop */

/*------------------------------------------
 *  Calculate Qbartotal[ep] (before the 
 *  river might split in multiple outlets)
 *------------------------------------------*/
            if( setstartmeanQandQs == 0 ){
            	Qbartotal[ep] = Qgrandtotal[ep] / (daysiy*nyears[ep]*dTOs);
				baseflowpercentage = ((Qbartotal[ep] - baseflowtot[ep])/Qbartotal[ep]);            	
            	Qicebartotal[ep] = (Qicetotal[ep] / (daysiy*nyears[ep]*dTOs))* baseflowpercentage;
            	if ( eventnrflag == 1 ){
            		if ( floodcounter == 0 )
            			eventsnr[ep] = 1;
            		if ( floodcounter > 0 ){
            			floodcounter = 0;
						for (p=0; p<nyears[ep]; p++)
							for (k=0; k<daysiy; k++)
								if ( Qpeakfloodtemp[p][k] > 0.0 ){
									Qpeakfloodtemp[p][k] = (Qpeakfloodtemp[p][k] * (( Qbartotal[ep] - baseflowtot[ep] ) / Qbartotal[ep])) + baseflowtot[ep];
									if ( Qpeakfloodtemp[p][k] > floodvalue[ep] )
										floodcounter++;
								}
		            	eventsnr[ep]=floodcounter;
            		}
    	        	eventnrflag = 0;
            	}
            }

/*-----------------------------------------
 *  Calculate Qbar[ep] per outlet, taking
 *  the possible events into acount
 *-----------------------------------------*/            	
            if ( outletmodelflag == 1 && (setstartmeanQandQs == 1 || setstartmeanQandQs == 2))
            	for (p=0; p<maxnoutlet; p++)
            		for (k=0; k<eventsnr[ep]; k++){
            			if ( daysievent[k] > 0 )
	   		        		Qbar[ep][p][k] = (Qgrandtotaloutlet[ep][p][k])/((daysievent[k])*dTOs);
	   		        	else Qbar[ep][p][k] = 0.0;
            		}

			/*------------------
			 * allocate memory
			 *------------------*/
            if	( eventnrflag == 0 && setstartmeanQandQs == 0)
            	hydroallocmemoutlet1(ep, nepochs);
            
            /*--------------------------------------------------
             *  Correction for baseflow on the peakevents. In
             *  hydrosumflow the correction will be calculated
             *  on a daily bases when setstartmeanQandQs > 0
             *--------------------------------------------------*/           
			if ((noutletflag == 1 && setstartmeanQandQs == 0) || (noutletflag == 0 && setstartmeanQandQs == 0 && steadyoutletpctflag == 0))
				for (k=0; k<eventsnr[ep]; k++){					
					Qpeakallevents[ep][k] = ((Qpeakallevents[ep][k] * (( Qbartotal[ep] - baseflowtot[ep] ) / Qbartotal[ep])) + baseflowtot[ep]);

			/*--------------------------------------------
			 *  Shuffle Qfraction per outlet per event		
			 *--------------------------------------------*/
					nroutlets[k] = hydrosetnumberoutlet(k);
					noutlet = nroutlets[k];
					err = hydroqfractionshuffle(k);
				}					

/*---------------------------------------------
 *  Start Hydrocalqsnew to calc. and Qsbarnew
 *---------------------------------------------*/
			Qsglacierbar[ep] = Qsglaciertotal[ep]/(daysiy*nyears[ep]*dTOs);
            if (setstartmeanQandQs == 3){             	         	
                if (verbose) printf("Calling Hydrocalqsnew... \n");
                err = hydrocalqsnew();
                if (err) {
                    fprintf( stderr, " ERROR in Hydrocalqsnew: HydroTrend Aborted \n\n" );
                    fprintf( fidlog, " ERROR in Hydrocalqsnew: HydroTrend Aborted \n\n" );
                    exit(1);
                }
            
	            if (outletmodelflag == 1){
    	        	for (p=0; p<maxnoutlet; p++)
        	    		outletpcttotevents[p][ep] = 0.0;
					for (p=0; p<maxnoutlet; p++){
						totaldays = 0;
						for (k=0; k<eventsnr[ep]; k++){
							outletpcttotevents[p][ep] += outletpct[p][ep][k] * daysievent[k];
							totaldays += daysievent[k];
						}
						outletpcttotevents[p][ep] /= (totaldays);
					}
            	}
            } /* end if setstartmeanQandQs == 3 */
        }    /* end for setstartmeanQandQs<5 loop */

/*------------------------------------------------
 *  Set the variables for the summary statistics
 *------------------------------------------------*/
		if (outletmodelflag == 1){
			totpercentageQ[ep] = 0.0;		
			for (p=0; p<maxnoutlet; p++){
				totaldays = 0;
				for (k=0; k<eventsnr[ep]; k++){
					totpercentageQ[ep] += outletpct[p][ep][k];
					Qdummy[ep][p] += ( Qbar[ep][p][k] * daysievent[k] );
					outletpcttotevents[p][ep] += outletpct[p][ep][k] * daysievent[k];
					totaldays += daysievent[k];
				}
				Qdummy[ep][p] /= totaldays;
				outletpcttotevents[p][ep] /= (totaldays);
				Qgrandtotaltotoutlet[p] = (Qgrandtotaloutlet[ep][p][k]);
				Qsgrandtotaltotoutlet[p] = Qsgrandtotaloutlet[ep][p];
				Qsgrandtotaldelta[ep] += Qsgrandtotaloutlet[ep][p];
				Coutlettotal[ep][p] =Coutlettotal[ep][p]/(daysiy*nyears[ep]);
			}
			if ((outletmodelflag == 1 && steadyoutletpctflag == 0) ||(noutletflag == 1 && steadyoutletpctflag == 1) || (noutletflag == 0 && steadyoutletpctflag == 1)){
				ep=0;
				totpercentageQ[ep] /= eventsnr[ep];
			}
		}
		else totpercentageQ[ep] = 1.0;
		Qsgrandtotaltot += Qsgrandtotal[ep];
		Qgrandtotaltot += Qgrandtotal[ep];
        if (nepochs == 1)
            Qpeakmax = Qpeakall[ep];
        else Qpeakmax = mx( Qpeakmax, Qpeakall[ep] );
        TEtot += TE[ep];
	
/*------------------------------------------------------------------
 *  Print out statistic file, for values used in Qs and Q formulas
 *------------------------------------------------------------------*/
        if (verbose) printf("Calling HydroprintStat... \n");
        err = hydroprintstat();
	    if (err) {
	         fprintf( stderr, " WARNING in HydroPrintStat: Continuing \n\n" );
	         fprintf( fidlog, " WARNING in HydroPrintStat: Continuing \n\n" );
        }
        freematrix1D( (void*) ranarray );
        freematrix1D( (void*) C );
    }  /* end epoch loop */

/*---------------------------------
 *  Print some summary statistics
 *---------------------------------*/
    fprintf( stderr, "\n\nSTATISTICS PER EPOCH PER OUTLET\n" );
    fprintf( stderr, "Ep\tOutlet\tQbar\tQsbar\tQsmean/outlet\tQpeak\tTEbas. TEres.\n");   
	for (ep = 0; ep < nepochs; ep++){
		p=0;
		fprintf( stderr, "%d\t%d(%.0f%%)\t%.2f\t%.2f\t\t\t%.1f\t%.1f   %.1f\n",
			(ep+1),p,totpercentageQ[ep]*100,Qbartotal[ep],( 1.0 -  sedfilter[ep])*(Qsbartot[ep]+(fractionglaciersediment[ep]*Qsglacierbar[ep])),Qpeakall[ep],TE[ep]*100, TEsubbasin[ep]*100);
		if (outletmodelflag == 1)
			for (p=0; p<maxnoutlet; p++)
				fprintf( stderr, "%d\t%d(%.0f%%)\t%.2f\t%.2f\t%\t%.2f\t%.2f\n",
					(ep+1),p+1,outletpcttotevents[p][ep]*100,Qdummy[ep][p],Qsgrandtotaloutlet[ep][p]/(daysiy*nyears[ep]*dTOs),(Qsgrandtotaloutlet[ep][p]/Qsgrandtotaldelta[ep])*100/*/(daysiy*nyears[ep]*dTOs)*/,Qpeakperoutletall[ep][p]);
	    fprintf( stderr, "nr\tnr\t(m^3/s)\t(kg/s)\t(%%)\t\t(m^3/s)\t (%%)   (%%)\n\n");
    	fprintf( stderr, "Basin area                           = %.2f  (km^2) \n",  totalarea/1e6);
	    fprintf( stderr, "Basin relief                         = %.2f  (m) \n\n\n", maxalt);
	}
	
    fprintf( fidlog, "\n\nSTATISTICS PER EPOCH PER OUTLET\n" );
    fprintf( fidlog, "Ep\tOutlet\tQbar\tQsbar\tQsmean/outlet\tQpeak\tTEbas. TEres.\n");
	for (ep = 0; ep < nepochs; ep++){
		p=0;
		fprintf( fidlog, "%d\t%d(%.0f%%)\t%.2f\t%.2f\t\t\t%.1f\t%.1f   %.1f\n",
			(ep+1),p,totpercentageQ[ep]*100,Qbartotal[ep],( 1.0 -  sedfilter[ep])*(Qsbartot[ep]+(fractionglaciersediment[ep]*Qsglacierbar[ep])),Qpeakall[ep],TE[ep]*100, TEsubbasin[ep]*100);
		if (outletmodelflag == 1)
			for (p=0; p<maxnoutlet; p++)
				fprintf( fidlog, "%d\t%d(%.0f%%)\t%.2f\t%.2f\t%\t%.2f\t%.2f\n",
					(ep+1),p+1,outletpcttotevents[p][ep]*100,Qdummy[ep][p],Qsgrandtotaloutlet[ep][p]/(daysiy*nyears[ep]*dTOs),(Qsgrandtotaloutlet[ep][p]/Qsgrandtotaldelta[ep])*100/*/(daysiy*nyears[ep]*dTOs)*/,Qpeakperoutletall[ep][p]);
	    fprintf( fidlog, "nr\tnr\t(m^3/s)\t(kg/s)\t(%%)\t\t(m^3/s)\t (%%)   (%%)\n\n");
    	fprintf( fidlog, "Basin area                           = %.2f  (km^2) \n",  totalarea/1e6);
	    fprintf( fidlog, "Basin relief                         = %.2f  (m) \n\n\n", maxalt);
	}
	

/*-------------------
 *  Close all files
 *------------------*/
    fclose(fidtrend1);
    fclose(fidtrend2);
    fclose(fidtrend3);
    fclose(fiddistot);

    if (strncmp(asciioutput,ON,2) == 0){
        fclose(outp);
        fclose(outp1);
        fclose(outp2);
        fclose(outp3);
        fclose(outp4);
    }
	if ( outletmodelflag == 1  )
		for ( p=0; p<maxnoutlet; p++ )
			fclose(fiddis[p]);        

/*-------------------------------------------------
 *  Swap big-endian and little-endian file format
 *-------------------------------------------------*/
    if (verbose) printf("Calling HydroSwap... \n");
    err = hydroswap();
    if (err)
        fprintf( stderr, " WARNING in HydroSwap: Continuing \n\n" );

/*---------------
 *  Free memory
 *---------------*/
	if(raindatafile ==1)
		freematrix1D( (void*) gw_rain->Tperyear );
	hydrofreememoutlet(ep);		

/*---------------------------
 *  Print program stop time
 *---------------------------*/
    time( &tloc );
    timeptr = localtime( &tloc );
    strftime( pst, TMLEN, "%X  %x", timeptr );
    fprintf(fidlog," \t------------------------- \n\n");
    fprintf(fidlog,"\t Stop: %19s \n", pst);
	fprintf(fidlog," =====================================\n\n"); 
 
fprintf( stderr, "\nHydroTrend 3.0 finished. \n\n");
    fclose(fidlog);
return 0;
}  /* end of HydroTrend */
