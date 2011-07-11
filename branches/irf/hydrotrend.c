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
  int
main (int argc, char **argv) 
{
  
/*-------------------
 *  Local Variables
 *-------------------*/ 
  char pst[TMLEN];
  
  
  
  
  
  
  
/*------------------------
 *  Initialize Variables
 *------------------------*/ 
    err = 0;
  
  
  
  
  
  
  
  
  
  
/*----------------------
 *  Get the start time
 *----------------------*/ 
    time (&tloc);
  
  
/*-----------------------------------------------------------------
 *  Check the command line input; get file name or directory name
 *-----------------------------------------------------------------*/ 
    err = hydrocommandline (&argc, argv);
  
    {
      
      
    
  
/*--------------------------------
 *  Read the main input file.
 *  This reads all of the epochs,
 *  at the very begining
 *--------------------------------*/ 
    if (verbose)
    printf ("Reading input data from: HYDRO.IN... \n");
  
  
    {
      
      
    
  
/*--------------------
 *  Allowcate memory
 *--------------------*/ 
    totpercentageQ = malloc1d (ep, double);
  
/*---------------------------------------
 *  Set the output path name + filename
 *---------------------------------------*/ 
    strcpy (startname, directory);
  
  
/*---------------------
 *  Open the log file
 *---------------------*/ 
    if (verbose)
    printf ("Opening the log file (to append)... \n");
  
  
  
    {
      
               ffnamelog);
      
    
  
/*--------------------------------
 *  Print the program start time
 *--------------------------------*/ 
    strftime (pst, TMLEN, "%X  %x", timeptr);
  
  
  
  
/*---------------------------------
 *  Read the hypsometric curve
 *  and set maxalt and totalarea.
 *---------------------------------*/ 
    if (verbose)
    printf ("Reading hypsometric data from: HYDRO.HYPS... \n");
  
  
    {
      
      
      
    
  
/*---------------------------------------------
 *  Read climate file if its their.
 *---------------------------------------------*/ 
    if (verbose)
    printf ("Reading live climate data files... \n");
  
  
    {
      
      
      
    
  
/*--------------------------------
 *  Set the hardwired parameters
 *--------------------------------*/ 
    if (verbose)
    printf ("Setting hardwired parameters... \n");
  
  
    {
      
      
    
  
/*------------------------------------------
 *  Set global values for those parameters
 *  which doesn't have any input
 *------------------------------------------*/ 
    if (globalparflag > 0)
    {
      
        printf ("Set global values for not filed out input parameters... \n");
      
      
        {
          
                    " ERROR in HydroSetGlobalPar: HydroTrend Aborted \n\n");
          
                    " ERROR in HydroSetGlobalPar: HydroTrend Aborted \n\n");
          
        
    
  
/*---------------------------------------------
 *  Check all of the input values.
 *  This also checks to make sure the climate
 *  variables match at the epoch breaks.
 *---------------------------------------------*/ 
    if (verbose)
    printf ("Checking all input parameters... \n");
  
  
    {
      
      
      
    
  
/*-----------------------
 *  Open the data files
 *-----------------------*/ 
    if (verbose)
    printf ("Opening output data files... \n");
  
  
    {
      
      
      
    
  
/*--------------------------
 *  Run each epoch of data
 *--------------------------*/ 
    if (verbose)
    printf (" \nStarting epoch loop... \n");
  
    {
      
      
      
      
/*-----------------------------------------------------------------
 *  Read Qs constant parameters set by geolocation of river mouth
 *-----------------------------------------------------------------*/ 
        if (verbose)
        printf ("Calling HydroSetGeoParams... \n");
      
      
        {
          
                    " ERROR in HydroSetGeoParams: HydroTrend Aborted \n\n");
          
                    " ERROR in HydroSetGeoParams: HydroTrend Aborted \n\n");
          
        
      
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
        for (setstartmeanQandQs = 0; setstartmeanQandQs < 5;
             setstartmeanQandQs++)
        {
          
          
            {
              
                        (ep + 1));
              
                        (ep + 1));
            
          
            {
              
                        (ep + 1));
              
                        (ep + 1));
            
          
            {
              
                        " Calculate mean suspended sediment load, epoch: %d\n",
                        (ep + 1));
              
                        " Calculate mean suspended sediment load, epoch: %d\n",
                        (ep + 1));
            
          
            {
              
                        " Calculate daily suspended sediment load, epoch: %d\n",
                        (ep + 1));
              
                        " Calculate daily suspended sediment load, epoch: %d\n",
                        (ep + 1));
            
          
/*---------------------------------------------------
 *  Free memory for possible multiple outlet module
 *---------------------------------------------------*/ 
            if ((ep > 0 && setstartmeanQandQs == 0))
            
          
            
          
/*-------------------------------------------------------
 *  Allocate memory for possible multiple outlet module
 *-------------------------------------------------------*/ 
            if (setstartmeanQandQs == 0)
            
          
            
          
/*----------------------------------
 *  Initialize Variables per epoch
 *----------------------------------*/ 
            if ((nooutletpctflag == 0) && (setstartmeanQandQs > 0))
            
              {
                
                
                  {
                    
                    
                    
                  
              
          
            {
              
                
                  
              
              
              
              
              
            
          
         /*-----------------------------------------------------------------
	  *  Start new random number sequence.
	  *  Get 'maxran' worth of random numbers and pluck them as needed
	  *  nran counts through the numbers stored in ranarray
	  *-----------------------------------------------------------------*/ 
            if (verbose)
            printf ("Calling HydroRandom... \n");
          
          
            {
              
                        " ERROR in HydroRandom: HydroTrend Aborted \n\n");
              
                        " ERROR in HydroRandom: HydroTrend Aborted \n\n");
              
            
          
          
 /*-----------------------------------------------------------------
  *  Start new random number sequence for sediment.
  *  Get 'maxran' worth of random numbers and pluck them as needed
  *  nransediment counts through the numbers stored in ranarraysediment
  *-----------------------------------------------------------------*/ 
            if (verbose)
            printf ("Calling HydroRandomsed... \n");
          
          
            {
              
                        " ERROR in HydroRandomsed: HydroTrend Aborted \n\n");
              
                        " ERROR in HydroRandomsed: HydroTrend Aborted \n\n");
              
            
          
          
/*-------------------------------------------------
 *  Initialize Variables per loop through program
 *-------------------------------------------------*/ 
            Qpeakall[ep] = 0.0;
          
          
            
          
/*-------------------------------------------
 *  Set the number of outlets for 10 events
 *  if number of outlets is not specified, 
 *  or given in a range.
 *-------------------------------------------*/ 
            if (noutletflag == 1 && setstartmeanQandQs == 1)
            {
              
                {
                  
                  
                  
                    printf ("Calling hydrooutletfraction... \n");
                  
                  
                    {
                      
                                " ERROR in HydroOutletFraction (HydroOutlet): HydroTrend Aborted \n\n");
                      
                                " ERROR in HydroOutletFraction (HydroOutlet): HydroTrend Aborted \n\n");
                      
                    
                
              
            
          
               && steadyoutletpctflag == 1 && setstartmeanQandQs == 1)
            
              {
                
                
                  printf ("Calling hydrooutletfraction... \n");
                
                
                  {
                    
                              " ERROR in HydroOutletFraction (HydroOutlet): HydroTrend Aborted \n\n");
                    
                              " ERROR in HydroOutletFraction (HydroOutlet): HydroTrend Aborted \n\n");
                    
                  
              
          
/*-----------------------------------------------------------------------
 *  Set the discharge fraction per outlet if there are multiple outlets
 *  and if the discharge fraction is not set yet in the outlet file
 *-----------------------------------------------------------------------*/ 
            if (outletmodelflag == 1 && nooutletpctflag == 1
                && steadyoutletpctflag == 0 && setstartmeanQandQs == 1)
            {
              
              
              
                printf ("Calling hydrooutletfraction... \n");
              
                {
                  
                            " ERROR in HydroOutletFraction (HydroOutlet): HydroTrend Aborted \n\n");
                  
                            " ERROR in HydroOutletFraction (HydroOutlet): HydroTrend Aborted \n\n");
                  
                
            
          
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
            if (verbose)
            printf ("  Epoch = %d \n", ep + 1);
          
          
            pow (10.0, (2.084 + 0.865 * logarea - 0.07 * sq (logarea)));
          
   /*---------------------------------------------------
    *  Create flood wave attenuation "shoulder" params
    *---------------------------------------------------*/ 
            if (verbose)
            printf ("Calling HydroShoulder... \n");
          
          
            {
              
                        " ERROR in HydroShoulder: HydroTrend Aborted \n\n");
              
                        " ERROR in HydroShoulder: HydroTrend Aborted \n\n");
              
            
          
   /*---------------------------------------
    *  Loop through each year of the epoch
    *---------------------------------------*/ 
            lyear = syear[ep] + nyears[ep];
          
            {
              
      /*-------------------------------------------------------------
       *  Reset annual arrays tracking carryover from previous year
       *-------------------------------------------------------------*/ 
                if (ep > 0 || yr != syear[ep])
                
                  {
                    
                    
                    
                    
                  
              
              else
                for (ii = 0; ii < maxday - daysiy; ii++)
                  {
                    
                    
                    
                    
                  
              
      /*---------------------------------------
       *  Keep track of groundwater pool size
       *---------------------------------------*/ 
                gwlast = gwstore[daysiy - 1];
              
      /*--------------------------------------------------
       *  In case the model exceeds the maximum flood,
       *  loop through a number of times.  This normally
       *  gets the maximum modeled flood to be below
       *  the maximum predicted flood.
       *--------------------------------------------------*/ 
                exceedflood = 1;
              
              
                {
                  
         /*------------------------------
          *  Reset annual arrays/values
          *------------------------------*/ 
                    for (ii = 0; ii < maxday; ii++)
                    {
                      
                      
                      
                      
                      
                      
                        
                    
                  
                    {
                      
                        
                          {
                            
                            
                            
                          
                      
                      
                      
                      
                      
                      
                      
                      
                    
                  
                  
                  
         /*---------------------------
	  *  Set the initial GW pool
	  *---------------------------*/ 
                    if (yr == syear[0])
                    {
                      
                      
                    
                  
                    
                  
         /*-----------------------------------------------------------------
	  *  Start new random number sequence.
	  *  Get 'maxran' worth of random numbers and pluck them as needed
	  *  nran counts through the numbers stored in ranarray
	  *-----------------------------------------------------------------*/ 
                    rmin = -6.0;
                  
                    {
                      
                        printf ("Calling HydroRandom... \n");
                      
                      
                        {
                          
                                    " ERROR in HydroRandom: HydroTrend Aborted \n\n");
                          
                                    " ERROR in HydroRandom: HydroTrend Aborted \n\n");
                          
                        
                    
                  
         /*---------------------------------
	  *  Set the climate for this year
	  *---------------------------------*/ 
                    if (verbose)
                    printf ("Calling HydroClimate... \n");
                  
                  
                    {
                      
                                " ERROR in HydroClimate: HydroTrend Aborted \n\n");
                      
                                " ERROR in HydroClimate: HydroTrend Aborted \n\n");
                      
                    
                  
         /*-------------------------------------
	  *  Calculate weather for each day of
	  *  the year, for each hypsometric bin
	  *-------------------------------------*/ 
                    if (verbose)
                    printf ("Calling HydroWeather... \n");
                  
                  
                    {
                      
                                " ERROR in HydroWeather: HydroTrend Aborted \n\n");
                      
                                " ERROR in HydroWeather: HydroTrend Aborted \n\n");
                      
                    
                  
         /*-------------------------------------------------
	  *  Calculate elev grid and T, for each elevation
	  *-------------------------------------------------*/ 
                    if (verbose)
                    printf ("Calling HydroHypsom... \n");
                  
                  
                    {
                      
                                " ERROR in HydroHypsom: HydroTrend Aborted \n\n");
                      
                                " ERROR in HydroHypsom: HydroTrend Aborted \n\n");
                      
                    
                  
         /*-------------------------------------------------
	  *  Calculate ice accumulation/melt for each day.
	  *  This is done before HydroRain or HydroSnow to
	  *  find the glaciated area
	  *-------------------------------------------------*/ 
                    if (verbose)
                    printf ("Calling HydroGlacial... \n");
                  
                  
                    {
                      
                                " ERROR in HydroGlacial: HydroTrend Aborted \n\n");
                      
                                " ERROR in HydroGlacial: HydroTrend Aborted \n\n");
                      
                    
                  
         /*------------------------------------------
	  *  Calculate snow fall/melt for each day.
	  *  This is done before HydroRain to find
	  *  the "snow" area for each day
	  *------------------------------------------*/ 
                    if (verbose)
                    printf ("Calling HydroSnow... \n");
                  
                  
                    {
                      
                                " ERROR in HydroSnow: HydroTrend Aborted \n\n");
                      
                                " ERROR in HydroSnow: HydroTrend Aborted \n\n");
                      
                    
                  
         /*---------------------------------
	  *  Calculate precip for each day
	  *---------------------------------*/ 
                    if (verbose)
                    printf ("Calling HydroRain... \n");
                  
                  
                    {
                      
                                " ERROR in HydroRain: HydroTrend Aborted \n\n");
                      
                                " ERROR in HydroRain: HydroTrend Aborted \n\n");
                      
                    
                  
         /*------------------------------------------------------------
	  *  Add the component flows and find peakflow for the year.
	  *  Store the lagged overflow and groundwater pool size for
	  *  the following year.
	  *------------------------------------------------------------*/ 
                    if (verbose)
                    printf ("Calling HydroSumFlow... \n");
                  
                  
                    {
                      
                                " ERROR in HydroSumFlow: HydroTrend Aborted \n\n");
                      
                                " ERROR in HydroSumFlow: HydroTrend Aborted \n\n");
                      
                    
                  
         /*-----------------------------------------
      *  Is flood peak less than max allowed ?
      *-----------------------------------------*/ 
                    if (Qpeak < maxflood)
                    
                  
                  else
                    {
                      
                        {
                          
                                    "\n FLOOD WARNING: epoch %d, year %d \n",
                                    ep + 1, yr);
                          
                                    " \t Max.Allowed %.1f, Qpeak %.1f, retry # %d \n",
                                    maxflood, Qpeak, floodtry);
                        
                      
                      
                        {
                          
                        
                    
                
              
      /*-------------------------------------------------------
       *  Track the max flow and if we still exceed the max
       *  predicted flood, send warning flag, but keep going
       *-------------------------------------------------------*/ 
                Qpeakall[ep] = mx (Qpeak, Qpeakall[ep]);
              
                
                  
                    mx (Qpeakperoutlet[p], Qpeakperoutletall[ep][p]);
              
                {
                  
                            "   FLOOD WARNING: the maximum predicted flood size");
                  
                  
                  
                            "      Maximum predicted flood %g (m^3/s) \n",
                            maxflood);
                  
                            Qpeak);
                  
                             "   FLOOD WARNING: the maximum predicted flood size");
                  
                  
                  
                            "      Maximum predicted flood %g (m^3/s) \n",
                            maxflood);
                  
                            Qpeak);
                
              
/*-------------------------------------------------------------
 *  Calculate the maximal events = (biggest Qpeaks per epoch)
 *  for channel switching at delta if option is turned on
 *------------------------------------------------------------- */ 
                if (setstartmeanQandQs == 1 && eventnrflag == 0)
                {
                  
                    printf ("Calling HydroMaxEvents... \n");
                  
                  
                    {
                      
                                " ERROR in HydroMaxEvents: HydroTrend Aborted \n\n");
                      
                                " ERROR in HydroMaxEvents: HydroTrend Aborted \n\n");
                      
                    
                
              
      /*---------------------------
       *  Calculate sediment load 
       *---------------------------*/ 
                if (setstartmeanQandQs > 1)
                {
                  
                    printf ("Calling HydroSedLoad... \n");
                  
                  
                    {
                      
                                " ERROR in HydroSedLoad: HydroTrend Aborted \n\n");
                      
                                " ERROR in HydroSedLoad: HydroTrend Aborted \n\n");
                      
                    
                
              
           /*------------------------------------------
            *  Output the binary daily discharge file
            *------------------------------------------*/ 
                if (setstartmeanQandQs == 4)
                {
                  
                    printf ("Calling HydroOutput... \n");
                  
                  
                    {
                      
                                " ERROR in HydroOutput: HydroTrend Aborted \n\n");
                      
                                " ERROR in HydroOutput: HydroTrend Aborted \n\n");
                      
                    
                  
           /*------------------------------------------------
            *  Print out annual values to trend (trn) files
            *------------------------------------------------*/ 
                    if (verbose)
                    printf ("Calling HydroPrintAnnual... \n");
                  
                  
                    {
                      
                                " WARNING in HydroPrintTrend: Continuing \n\n");
                      
                                " WARNING in HydroPrintTrend: Continuing \n\n");
                    
                
              
      /*-------------------------------------------------------
       *  Did random number generator create enough values ?
       *-------------------------------------------------------*/ 
                if (nran >= maxran)
                {
                  
                            "\n\n HydroTrend ERROR: nran exceeded maxran.\n");
                  
                  
                            maxran);
                  
                
            
          
/*------------------------------------------
 *  Calculate Qbartotal[ep] (before the 
 *  river might split in multiple outlets)
 *------------------------------------------*/ 
            if (setstartmeanQandQs == 0)
            {
              
              
                ((Qbartotal[ep] - baseflowtot[ep]) / Qbartotal[ep]);
              
                (Qicetotal[ep] / (daysiy * nyears[ep] * dTOs)) *
                baseflowpercentage;
              
                {
                  
                    
                  
                    {
                      
                      
                        
                          
                            {
                              
                                (Qpeakfloodtemp[p][k] *
                                 ((Qbartotal[ep] -
                                   baseflowtot[ep]) / Qbartotal[ep])) +
                                baseflowtot[ep];
                              
                                
                            
                      
                    
                  
                
            
          
/*-----------------------------------------
 *  Calculate Qbar[ep] per outlet, taking
 *  the possible events into acount
 *-----------------------------------------*/ 
            if (outletmodelflag == 1
                && (setstartmeanQandQs == 1 || setstartmeanQandQs == 2))
            
              
                {
                  
                    
                      (Qgrandtotaloutlet[ep][p][k]) / ((daysievent[k]) * dTOs);
                  
                  else
                    Qbar[ep][p][k] = 0.0;
                
          
                        /*------------------
			 * allocate memory
			 *------------------*/ 
            if (eventnrflag == 0 && setstartmeanQandQs == 0)
            
          
            /*--------------------------------------------------
             *  Correction for baseflow on the peakevents. In
             *  hydrosumflow the correction will be calculated
             *  on a daily bases when setstartmeanQandQs > 0
             *--------------------------------------------------*/ 
            if ((noutletflag == 1 && setstartmeanQandQs == 0)
                || (noutletflag == 0 && setstartmeanQandQs == 0
                    && steadyoutletpctflag == 0))
            
              {
                
                  ((Qpeakallevents[ep][k] *
                    ((Qbartotal[ep] - baseflowtot[ep]) / Qbartotal[ep])) +
                   baseflowtot[ep]);
                
                        /*--------------------------------------------
			 *  Shuffle Qfraction per outlet per event		
			 *--------------------------------------------*/ 
                  nroutlets[k] = hydrosetnumberoutlet (k);
                
                
              
          
/*---------------------------------------------
 *  Start Hydrocalqsnew to calc. and Qsbarnew
 *---------------------------------------------*/ 
            Qsglacierbar[ep] =
            Qsglaciertotal[ep] / (daysiy * nyears[ep] * dTOs);
          
            {
              
                printf ("Calling Hydrocalqsnew... \n");
              
              
                {
                  
                            " ERROR in Hydrocalqsnew: HydroTrend Aborted \n\n");
                  
                            " ERROR in Hydrocalqsnew: HydroTrend Aborted \n\n");
                  
                
              
                {
                  
                    
                  
                    {
                      
                      
                        {
                          
                            outletpct[p][ep][k] * daysievent[k];
                          
                        
                      
                    
                
            
        
      
/*------------------------------------------------
 *  Set the variables for the summary statistics
 *------------------------------------------------*/ 
        if (outletmodelflag == 1)
        {
          
          
            {
              
              
                {
                  
                  
                  
                    outletpct[p][ep][k] * daysievent[k];
                  
                
              
              
              
              
              
              
                Coutlettotal[ep][p] / (daysiy * nyears[ep]);
            
          
               || (noutletflag == 1 && steadyoutletpctflag == 1)
               || (noutletflag == 0 && steadyoutletpctflag == 1))
            {
              
              
            
        
      
      else
        totpercentageQ[ep] = 1.0;
      
      
      
        
      
      else
        Qpeakmax = mx (Qpeakmax, Qpeakall[ep]);
      
      
/*------------------------------------------------------------------
 *  Print out statistic file, for values used in Qs and Q formulas
 *------------------------------------------------------------------*/ 
        if (verbose)
        printf ("Calling HydroprintStat... \n");
      
      
        {
          
          
        
      
      
    
    
/*---------------------------------
 *  Print some summary statistics
 *---------------------------------*/ 
    fprintf (stderr, "\n\nSTATISTICS PER EPOCH PER OUTLET\n");
  
            "Ep\tOutlet\tQbar\tQsbar\tQsmean/outlet\tQpeak\tTEbas. TEres.\n");
  
    {
      
      
                (ep + 1), p, totpercentageQ[ep] * 100, Qbartotal[ep],
                (1.0 - sedfilter[ep]) * (Qsbartot[ep] +
                                         (fractionglaciersediment[ep] *
                                          Qsglacierbar[ep])), Qpeakall[ep],
                TE[ep] * 100, TEsubbasin[ep] * 100);
      
        
          
                    (ep + 1), p + 1, outletpcttotevents[p][ep] * 100,
                    Qdummy[ep][p],
                    Qsgrandtotaloutlet[ep][p] / (daysiy * nyears[ep] * dTOs),
                    (Qsgrandtotaloutlet[ep][p] / Qsgrandtotaldelta[ep]) *
                    100 /*/(daysiy*nyears[ep]*dTOs) */ ,
                    Qpeakperoutletall[ep][p]);
      
                "nr\tnr\t(m^3/s)\t(kg/s)\t(%%)\t\t(m^3/s)\t (%%)   (%%)\n\n");
      
                "Basin area                           = %.2f  (km^2) \n",
                totalarea[ep] / 1e6);
      
                "Basin relief                         = %.2f  (m) \n\n\n",
                maxalt[ep]);
    
  
  
            "Ep\tOutlet\tQbar\tQsbar\tQsmean/outlet\tQpeak\tTEbas. TEres.\n");
  
    {
      
      
                (ep + 1), p, totpercentageQ[ep] * 100, Qbartotal[ep],
                (1.0 - sedfilter[ep]) * (Qsbartot[ep] +
                                         (fractionglaciersediment[ep] *
                                          Qsglacierbar[ep])), Qpeakall[ep],
                TE[ep] * 100, TEsubbasin[ep] * 100);
      
        
          
                    (ep + 1), p + 1, outletpcttotevents[p][ep] * 100,
                    Qdummy[ep][p],
                    Qsgrandtotaloutlet[ep][p] / (daysiy * nyears[ep] * dTOs),
                    (Qsgrandtotaloutlet[ep][p] / Qsgrandtotaldelta[ep]) *
                    100 /*/(daysiy*nyears[ep]*dTOs) */ ,
                    Qpeakperoutletall[ep][p]);
      
                "nr\tnr\t(m^3/s)\t(kg/s)\t(%%)\t\t(m^3/s)\t (%%)   (%%)\n\n");
      
                "Basin area                           = %.2f  (km^2) \n",
                totalarea[ep] / 1e6);
      
                "Basin relief                         = %.2f  (m) \n\n\n",
                maxalt[ep]);
    
  
/*-------------------
 *  Close all files
 *------------------*/ 
    fclose (fidtrend1);
  
  
  
  
    {
      
      
      
      
      
    
  
    
      
  
/*-------------------------------------------------
 *  Swap big-endian and little-endian file format
 *-------------------------------------------------*/ 
    if (verbose)
    printf ("Calling HydroSwap... \n");
  
  
    
  
/*---------------
 *  Free memory
 *---------------*/ 
    if (raindatafile == 1)
    
  
  
/*---------------------------
 *  Print program stop time
 *---------------------------*/ 
    time (&tloc);
  
  
  
  
  
  
  
  


