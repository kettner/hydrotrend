/*-------------------------------------------------------------------------------------------
 *  hydroreadclimate.c
 *
 *	Author: Albert Kettner, March 2006
 *
 *	This subroutine will become active, if the option is used to generate discharge and sediment
 *	load based  on sequential climate input. (Input file used: HYDRO.CLIMATE).
 *
 * Variable		Def.Location		Type		Units	Usage
 * --------		------------		----		-----	-----
 * err			various				int			-		error flag, halts program
 * count		Read_Rainfall_Etc	int			-
 * dummyT		Read_Rainfall_Etc	double		deg.C
 * dummyTtot	Read_Rainfall_Etc	double		deg.C
 * dummyT2		Read_Rainfall_Etc	double		deg.C
 * dummyR		Read_Rainfall_Etc	double		mm
 * dummyRtot	Read_Rainfall_Etc	double		mm
 * HOURSINDAY	Read_Rainfall_Etc	-			hours	define the hours in a day
 * i			Read_Rainfall_Etc	int			-
 * k			Read_Rainfall_Etc	int			-
 * line[200]	Read_Rainfall_Etc	char		-
 * n			Read_Rainfall_Etc	long		-		counter for the number of years
 *
 *-------------------------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "hydroalloc_mem.h"
#include "hydroinout.h"
#include "hydroparams.h"
#include "hydroreadclimate.h"
#include "hydrotimeser.h"
#include "hydroclimate.h"
  
/*-----------------------
 *  Function Definition
 *-----------------------*/ 
void Read_Rainfall_Etc (gw_rainfall_etc * gw_rain);

/*---------------------------
 *  Start of HydroReadInput
 *---------------------------*/ 
  int
hydroreadclimate (gw_rainfall_etc * gw_rain)
{
  
/*-------------------
 *  Local Variables
 *-------------------*/ 
  int k, err;
  
/*-----------------------
 *  Set local Variables
 *-----------------------*/ 
    err = 0;
  
  
  
    
  
/*-------------------------------
 * Read Rainfall input FOR NOW
 *-------------------------------*/ 
    Read_Rainfall_Etc (gw_rain);
  



/*-----------------------
 *  Function Prototypes
 *-----------------------*/ 
  
/*------------------------------
 *  Function Read_Rainfall_Etc
 *------------------------------*/ 
  void
Read_Rainfall_Etc (gw_rainfall_etc * gw_rain)
{
  
/*-------------------
 *  Local Variables
 *-------------------*/ 
#define HOURSINDAY  (24.0) 
  int k, count, i;
  
  
  
  
/*------------------------
 *  Open rain input file
 *------------------------*/ 
    if ((fidinputgw_r = fopen (fnameinputgw_r, "r")) == NULL)
    {
      
                "  Read_Rainfall_Etc MESSAGE: Unable to open input file %s \n",
                fnameinputgw_r);
      
                "    Hydrotrend will generate it's own climate values based on\n");
      
                "    line 12-23 of the input values in the input file.\n\n");
      
    
  
  else
    raindatafile = 1;
  
    {
      
/*--------------------
 *  Strip off header
 *--------------------*/ 
        for (k = 0; k < 6; k++)
        
      
/*---------------------------------
 *  Read number of timesteps & dt
 *---------------------------------*/ 
        fgets (line, sizeof (line), fidinputgw_r);
      
      
/*---------------------------------------------------
 *  Check number of years to run with actually rain
 *  data. If actually rain data is not equal with 
 *  the number of years to run, stop program.
 *---------------------------------------------------*/ 
        if (gw_rain->n_steps / daysiy < total_yr)
        {
          
          
                    "  HydroReadclimate.c ERROR: Unable to run hydrotrend because of\n");
          
          
                    "    Number of model years in HYDRO.IN, total years from all epochs: %d (line 5).\n",
                    total_yr);
          
                    "    is not equal to number of years of climate data: %f.\n",
                    dummydouble);
          
          
        
      
/*------------------------------
 *  Allocate memory for arrays
 *------------------------------*/ 
        gw_rain->R = malloc2d (total_yr, daysiy, double);
      
      
      
/*----------------------------------------------
 *  Read Precipitation and Temperature values
 *  If values are not daily values 
 *  but for example hour values, then
 *  average the temperature and add
 *  all the precipitation values for that day.
 *----------------------------------------------*/ 
        for (n = 0; n < total_yr; n++)
        {
          
          
            {
              
              
              
                {
                  
                  
                  
                  
                    {
                      
                      
                                "  HydroReadclimate.c ERROR: Error occured when\n");
                      
                      
                      
                                "    Precipitation or Temperature data is missing\n");
                      
                                dummyR, dummyT);
                      
                      
                    
                  
                    {
                      
                      
                                "    Precipitation data out of range,\n");
                      
                                dummyR);
                      
                      
                    
                  
                    {
                      
                      
                      
                                dummyT);
                      
                      
                    
                  
                  
                
              
              
              
              
              
            
          
        
      
/*--------------------
 *  Close input file
 *--------------------*/ 
        fclose (fidinputgw_r);
    
  


