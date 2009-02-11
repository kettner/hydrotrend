/*-------------------------------------------------------------------------------------------
 *	hydroreadinput.c
 *
 *	Author: Albert Kettner, March 2006
 *
 *	Reads the main ASCII input file for HYDROTREND (HYDRO.IN)
 *
 *
 * 	Variable	Def.Location		Type	Units	Usage
 * 	--------	------------		----	-----	-----
 * 	chs[150]	HydroReadInput.c	char	-		temporary character string
 * 	dumchr[2]	HydroReadInput.c	char	-		temporary character string
 * 	dumdbl 		HydroReadInput.c	double  -		temporary double
 * 	dumint		HydroReadInput.c	int		-		temporary integer
 * 	err			various				int		-		error flag, halts program
 * 	jj			various				int		-		temporary loop counter
 *
 *-------------------------------------------------------------------------------------------*/

#include <string.h>
#include "hydroclimate.h"
#include "hydroinout.h"
#include "hydroparams.h"
#include "hydroalloc_mem.h"
#include "hydrotrend.h"
#define MAXDIR (100)
  
/*---------------------------
 *  Start of HydroReadInput
 *---------------------------*/ 
  int
hydroreadinput () 
{
  
/*-------------------
 *  Local Variables
 *-------------------*/ 
  char chs[150], dumchr[2];
  
  
  
  
/*------------------------
 *  Initialize Variables
 *------------------------*/ 
    err = 0;
  
  
  
/*-----------------------
 *  Open the input file
 *-----------------------*/ 
    if ((fidinput = fopen (fnameinput, "r")) == NULL)
    {
      
                "  HydroReadInput.c ERROR: Unable to open the input file %s \n",
                fnameinput);
      
      
      
//    exit(1);
    }
  
/*---------------------------------------
 *  1) Read in title of first epoch (-)
 *---------------------------------------*/ 
    fgets (title, 150, fidinput);
  
/*-----------------------------------------------------------------
 *  2) Read the option of writing output yes or no to ascii file
 *-----------------------------------------------------------------*/ 
    for (jj = 0; jj < MAXCHAR; jj++)
    {
      
      
      
        
    
  
  
/*------------------------------------
 *  3) Reed the output directory in
 *------------------------------------*/ 
    for (jj = 0; jj < MAXDIR; jj++)
    {
      
      
        {
          
            
          
          else
            directory[jj] = '\0';
          
        
    
  
  
/*-----------------------------------
 *  4) Read in number of epochs (-)
 *-----------------------------------*/ 
    fscanf (fidinput, "%d", &nepochs);
  
  
/*-------------------------------------------------
 *  Dynamic allocation off input variables
 *-------------------------------------------------*/ 
    hydroinputalloc (nepochs);
  
/*-----------------------------------
 *  Loop through number of epochs
 *  specified and retrieve the data
 *-----------------------------------*/ 
    for (ep = 0; ep < nepochs; ep++)
    {
      
   /*---------------------------------------------------------------
    *  5) Read start year, number of years and timestep (a,-,char)
    *    Keep timestep from first epoch for all subsequent epochs
    *---------------------------------------------------------------*/ 
        if (ep != 0)
        strcpy (dumchr, timestep);
      
      
      
      
      
        {
          
                    "   HydroReadInput.c WARNING: timestep changed between epochs. \n");
          
                    "      Hydrotrend will use the timestep from the begining epoch. \n");
          
          
          
          
                    nyears[ep]);
          
        
      
   /*---------------------------------------------------
    *  7) Read number of grain sizes to simulate (int)
    *   Must be constant for all epochs
    *---------------------------------------------------*/ 
        if (ep == 0)
        {
          
          
        
      
      else
        {
          
          
          
            {
              
                        "  HydroReadInput.c ERROR: ngrain must be constant for all epochs. \n");
              
              
              
              
            
        
      
   /*---------------------------------------------
    *  8) Read percentage of each grain size (%)
    *---------------------------------------------*/ 
        for (jj = 0; jj < ngrain; jj++)
        
      
      
   /*--------------------------------------------------------------
    *  9) Read temperature trend parameters ( degC, degC/a, degC)
    *--------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf %lf ", &Tstart[ep], &Tchange[ep], &Tstd[ep]);
      
      
   /*--------------------------------------------------------------
    *  10) Read precipitation trend parameters (m/a, (m/a)/a, m/a)
    *--------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf %lf ", &Pstart[ep], &Pchange[ep], &Pstd[ep]);
      
      
   /*-------------------------------------------------------
    *  11) Read rainfall mass balance parameters ( %, -, -)
    *-------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf %lf ", &Pmassbal[ep], &Pexponent[ep],
                &Prange[ep]);
      
      
   /*-------------------------------------------------
    *  12) Read constant base flow discharge (m^3/s)
    *-------------------------------------------------*/ 
        fscanf (fidinput, "%lf ", &baseflowtot[ep]);
      
      
   /*--------------------------------------------------------------------------
    *  13-24) Read climate statistics ( month, degC, degC, mm -> m, mm -> m )
    *--------------------------------------------------------------------------*/ 
        for (jj = 0; jj < 12; jj++)
        {
          
                   &Tnominal[jj][ep], &Tnomstd[jj][ep], &Pnominal[jj][ep],
                   &Pnomstd[jj][ep]);
          
          
          
        
      
   /*---------------------------------------------
    *  25) Read lapse rate ( degC/km -> degC/m )
    *---------------------------------------------*/ 
        fscanf (fidinput, "%lf ", &lapserate[ep]);
      
      
      
        {
          
          
        
      
      else
        
      
   /*-------------------------------------------------------------
    *  26) Read ELA start altitude and change per year ( m, m/a)
    *-------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf ", &ELAstart[ep], &ELAchange[ep]);
      
      
   /*-------------------------------------------------------------------
    *  27) Read percentage of nival/ice runoff lost to evaporation (%)
    *-------------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf ", &dryevap[ep]);
      
      
   /*-------------------------------------------------------------------
    *  27a) Read the canopy interception coefficients (mm/d)(-)
    *-------------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf ", &alphag[ep], &betag[ep]);
      
      
      
   /*-------------------------------------------------------------------
    *  27b) Read the evapotranspiration coefficient(alphagwe[ep](mm/day))
	*       Read the evapotransiration coefficient (betagwe[ep](-))
    *-------------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf ", &alphagwe[ep], &betagwe[ep]);
      
      
      
   /*----------------------------------------------
    *  28) Read river bed average slope (m/m)
    *----------------------------------------------*/ 
        fscanf (fidinput, "%lf ", &rslope[ep]);
      
      
        /*----------------------------------------------
	 *  28a) Read bedload rating term (-)
	 *----------------------------------------------*/ 
        fscanf (fidinput, "%lf ", &alphabed[ep]);
      
      
        {
          
        
      
   /*-------------------------------------
    *  29) Read basin length ( km -> m )
    *-------------------------------------*/ 
        fscanf (fidinput, "%lf ", &basinlength[ep]);
      
      
      
   /*-----------------------------------------------------
    *  30) Read percentage of basin covered by lakes (%)
    *-----------------------------------------------------*/ 
        fscanf (fidinput, "%lf %c", &Rvol[ep], &Rparamcheck[ep]);
      
        {
          
          
        
      
        {
          
          
        
      
      
   /*------------------------------------------------------------
    *  31) Read river mouth velocity coeff and exponent ( -, -)
    *------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf ", &velcof[ep], &velpow[ep]);
      
      
   /*------------------------------------------------------------
    *  32) Read river mouth width coeff and exponent ( -, -)
    *------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf ", &widcof[ep], &widpow[ep]);
      
      
      
      
   /*-----------------------------------------
    *  33) Read average river velocity (m/s)
    *-----------------------------------------*/ 
        fscanf (fidinput, "%lf ", &avgvel[ep]);
      
      
   /*-------------------------------------------------------------
    *  34) Read max and min size of groundwater pool ( m^3, m^3)
    *-------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf ", &gwmax[ep], &gwmin[ep]);
      
      
   /*---------------------------------------------------
    *  35) Read initial size of groundwater pool (m^3)
    *    for first epoch only
    *---------------------------------------------------*/ 
        if (ep == 0)
        {
          
          
        
      
      else
        {
          
          
        
      
   /*-----------------------------------------------------------------
    *  36) Read subsurface storm flow coeff and exponent ( m^3/s, -)
    *-----------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf ", &alphass[ep], &betass[ep]);
      
      
   /*-----------------------------------------------------------------
    *  37) Read saturated hydraulic conductivity ( mm/day -> m/day )
    *-----------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf ", &Ko[ep]);
      
      
      
   /*------------------------------------------------------------------------
    *  38) Read latitude (Geographic position of rivermouth) ( in degrees )
    *------------------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf", &lon, &lat);
      
      
   /*------------------------------------------------
    *  39) Read number of outlets to simulate (int)
    *   Must be constant for all epochs
    *------------------------------------------------*/ 
        noutletflag = 0;
      
      
      
        {
          
          
          
          
        
      
      else if (dummyx == 'r' || dummyx == 'R')
        {
          
          
          
        
      
      else
        {
          
          
          
          
          
            {
              
              
            
          
            {
              
            
        
      
      
   /*--------------------------------------
    *  40) Read percentage of each outlet 
    *--------------------------------------*/ 
        steadyoutletpctflag = 0;
      
      
        {
          
          
        
      
        {
          
          
            {
              
              
              
                
              
              else if (dummyx == 'u' || dummyx == 'U')
                
              
              else
                err++;
            
          
          else if (dummyx == '0' || dummyx == '1')
            {
              
                
              
              
                {
                  
                
            
        
      
      
   /*-----------------------------
    *  41) Read number of events
    *-----------------------------*/ 
        fscanf (fidinput, "%c", &dummyx);
      
        {
          
          
          
          
            
          
            
        
      
        {
          
          
          
        
      
      
   /*-----------------------------------------
    *  42) Read the demping factor for Qsbar 
    *-----------------------------------------*/ 
        fscanf (fidinput, "%lf  ", &sedfilter[ep]);
      
        
      
      
   /*-----------------------------------------------------
    *  43) Check which formula to use to calculate Qsbar 
    *-----------------------------------------------------*/ 
        fscanf (fidinput, "%d  ", &Qsbarformulaflag[ep]);
      
      
   /*--------------------------------------------------------
    *  44) If BQART is used -> get extra parameters
    *      read the Lithology factor 
    *--------------------------------------------------------*/ 
        fscanf (fidinput, "%lf  ", &lithology[ep]);
      
      
   /*--------------------------------------------------------
    *  45) If BQART is used -> get extra parameters
    *      read the anthropogenic factor 
    *--------------------------------------------------------*/ 
        fscanf (fidinput, "%lf  ", &anthro[ep]);
      
    
  
  


