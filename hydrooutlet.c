/*-------------------------------------------------------------------------------------------
 *  hydrooutlet.c
 *
 *	Author: Albert Kettner, March 2006
 * 
 *  Contains 5 functions subroutines for multiple outlet option.
 *	Variables are difined per subroutine (see below).
 * 
 *  1) HydroOutletFraction
 *  2) HydroSetNumberOutlet
 *  3) HydroQFractionShuffle
 *  4) HydroAllocMemOutlet
 *  4a)Hydroallocmemoutlet1
 *  5) HydroFreeMemOutlet
 * 
 *-------------------------------------------------------------------------------------------*/
   
  
#include <math.h>
#include "hydroclimate.h"
#include "hydroinout.h"
#include "hydroparams.h"
#include "hydrotimeser.h"
#include "hydroreadclimate.h"
#include "hydroalloc_mem.h"
#include "hydrofree_mem.h"
#include "hydrornseeds.h"
  
/*-------------------------------------------------------------------------------------------
 *  1) HydroOutletFraction
 *
 * If the discharge fraction per outlet is not specified by the user
 * and number of outlets is >1, hydrotrend computes the fraction based
 * on the number of outlets.
 * (If number of outlets changes during the ep, hydrotrend calculates
 * the fraction per event).
 * The calculation of the discharge fraction is based on x=y^2. y 
 * "represents" the number of outlets, and are values between 0 and 1.
 * (y = 1/(nr of outlets + 1)). So y0 = y*1; y1 = y*2; y2 = y*3 etc.
 * x is the power of y. As the total off all x must be equal to 1 (100% 
 * of the discharge is leaving the outlets) each x has to be devide by
 * the sum of all x.
 * 
 * 
 * Variable		Def.Location		Type		Units	Usage
 * --------		------------		----		-----	-----
 * check		HydroOutletFraction	double		-		Check if total fraction is 1.0
 * err			various				int			-		error flag, halts program
 * fdistance	HydroOutletFraction	double		-		
 * p			HydroOutletFraction	int			-		counter
 * Qtempfractot	HydroOutletFraction	double		-
 * totalpct		HydroOutletFraction	double		-
 * *tempfrac	HydroOutletFraction	double		-
 * *Qpowtempf	HydroOutletFraction	double		-
 * 
 *-------------------------------------------------------------------------------------------*/ 
  
/*--------------------------------
 *  Start of HydroOutletFraction
 *--------------------------------*/ 
  int
hydrooutletfraction (int x) 
{
  
/*-------------------
 *  Local Variables
 *-------------------*/ 
  int err, p;
  
  
  
/*------------------------
 *  Initialize variables
 *------------------------*/ 
    err = 0;
  
  
  
/*---------------------------------------
 *  Allocate memory for multiple outlet
 *---------------------------------------*/ 
    tempfrac = malloc1d (maxnoutlet, double);
  
  
/*----------------------------------
 *  Set outlet fraction per outlet
 *----------------------------------*/ 
    for (p = 0; p < maxnoutlet; p++)
    
  
  
    {
      
      
      
    
  
    {
      
      
    
  
  
    
  
/*---------------
 *  Free memory
 *---------------*/ 
    freematrix1D ((void *) tempfrac);
  
  



/*-------------------------------------------------------------------------------------------
 *  2) hydroSetNumberOutlet
 *
 * If the number of outlets is not specified by the user hydrotrend
 * computes randomly the number of outlet between a given range.
 * It will do this per event. So with each event (flood) hydrotrend
 * calculates a new number of outlets.
 * 
 * 
 * Variable		Def.Location			Type		Units	Usage
 * --------		------------			----		-----	-----
 * dumint		HydroSetNumberOutlet	int			-
 * dumflt		HydroSetNumberOutlet	flt			-
 * err			various					int			-		error flag, halts program
 * noutletoptionHydroSetNumberOutlet	int			-
 * x			HydroSetNumberOutlet	int			-
 * 
 *-------------------------------------------------------------------------------------------*/ 
  
/*---------------------------------
 *  Start of HydroSetNumberOutlet
 *---------------------------------*/ 
  int
hydrosetnumberoutlet (int x) 
{
  
/*-------------------
 *  Local Variables
 *-------------------*/ 
  int dumint, noutletoption;
  
  
  
  
  
/*------------------------
 *  Set number of outlet
 *------------------------*/ 
    if (x == minnoutlet)
    
  
  
    {
      
      
        {
          
          
                    "A function in HydroRan2 failed in HydroSetNumberOutlet (HydroOutlet) \n");
          
                    " \t dumflt = %f: \t setting value to 0.5, x = %d \n",
                    dumdbl, x);
          
        
      
      
      
        {
          
          
                    "\t Randomnummer generator failed twice: HydroTrend Aborted \n\n");
          
          
                    "\t Randomnummer generator failed twice: HydroTrend Aborted \n\n");
          
        
    
  
  



/*-------------------------------------------------------------------------------------------
 *
 * 3) Hydroqfractionshuffle
 * 
 * Randomly shuffles the outletfractions.
 *
 *
 * Variable		Def.Location			Type	Units	Usage
 * --------		------------			----	-----	-----
 * dumflt		Hydroqfractionshuffle	float	-		temporary float
 * dvals[31]	Hydroqfractionshuffle	int		-		shuffled array of daily index values
 * err			Hydroqfractionshuffle	int		-		error flag, halts program
 * ii			Hydroqfractionshuffle	int		-		temporary loop counter
 * mnth			Hydroqfractionshuffle	int		-		month of the year
 * yy			Hydroqfractionshuffle	int		-		temporary integer
 * pp			Hydroqfractionshuffle	int		-		temporary integer
 *
 *-------------------------------------------------------------------------------------------*/ 
  
/*------------------------------------
 *  Start of Hydroqfractionshuffle.c
 *------------------------------------*/ 
  int
hydroqfractionshuffle (int k) 
{
  
  
  
  
  
/*---------------------------------------
 *  Allocate memory for multiple outlet
 *---------------------------------------*/ 
    dummyoutletpct = malloc1d (maxnoutlet, double);
  
  
  
/*------------------------
 *  Initialize variables
 *------------------------*/ 
    err = 0;
  
  
    {
      
      
    
  
/*-------------------------------------
 *  shuffle the numbers of the outlet
 *-------------------------------------*/ 
    if (k == 0)
    
  
    {
      
      
        {
          
                    "A function in HydroRan2 failed in HydroShuffle.c \n");
          
                    " \t dumdbl = %f: \t setting value to 0.5, ii = %d \n",
                    dumdbl, ii);
          
          
        
      
        {
          
          
          
            yy += 1;
          
          
          
            {
              
              
                
                  {
                    
                    
                    
                  
              
            
        
    
  
    {
      
      
    
  
/*---------------
 *  Free memory
 *---------------*/ 
    freematrix1D ((void *) dummyoutletpct);
  
  
  



/*-------------------------------------------------------------------------------------------
 *  4) HydroAllocmemOutlet
 *
 * Allocates memory for the outlet variables. The real allocation takes
 * place in Hydroalloc_mem.c
 *
 *-------------------------------------------------------------------------------------------*/ 
  
/*--------------------------------
 *  Start of HydroAllocmemOutlet
 *--------------------------------*/ 
  void
hydroallocmemoutlet (int ep, int nepochs) 
{
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  



/*---------------------------------
 *  Start of HydroAllocmemOutlet1
 *---------------------------------*/ 
  void
hydroallocmemoutlet1 (int ep, int nepochs) 
{
  
  nroutlets = malloc1d (eventsnr[ep], int);
  
  
  
  
  
  
  
  return;



/*-------------------------------------------------------------------------------------------
 *  5) HydroFreeMemOutlet
 *
 * Free memory for the outlet variables. To free the memory, it will use
 * the various subprograms in Hydrofree_mem.c
 *
 *-------------------------------------------------------------------------------------------*/ 
  
/*--------------------------------
 *  Start of HydroFreeMemOutlet
 *--------------------------------*/ 
  void
hydrofreememoutlet (int j) 
{
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  



/*--------------------------------
 *  Start of HydroFreeMemOutlet1
 *--------------------------------*/ 
  void
hydrofreememoutlet1 (int ep) 
{
  
  
  
  
  
  
  
  
  
  



/* end of HydroOutlet.c */ 