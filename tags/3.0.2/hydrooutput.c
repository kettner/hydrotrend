/*-------------------------------------------------------------------------------------------
 *  hydrooutput.c
 *
 *	Author: Albert Kettner, March 2006
 *
 *  Writes the discharge (v,w,d) and sediment load data to a binary file. After averaging
 *	the data to the user requested timestep (d,m,s,y).
 *
 * Variable		Def.Location	Type	Units	Usage
 * --------		------------	----	-----	-----
 * comLen		HydroOutput.c	int		-		length of the title string
 * Cs			HydroOutput.c	float	kg/m^3	averaged suspended concentration per grain class
 * Cstot		HydroOutput.c   float   kg/m^3  averaged suspended concentration total
 * dep[]		HydroOutput.c	float	m		river depth
 * err			various			int		-		error flag, halts program
 * ii			various			int		-		temporary loop counter
 * jj			various			int		-		temporary loop counter
 * kk			various			int		-		temporary loop counter
 * nrecords		HydroOutput.c	int		-		total # of output records
 * nYears		HydroOutput.c	int		-		total # of output years
 * recperyear	HydroOutput.c	int		-		# of output records per year
 * Qavg[]		HydroOutput.c	float	m^3/s	average river discharge
 * Qbavg[]		HydroOutput.c	float	kg/s	averaged bedload discharge
 * Qsavg[]		HydroOutput.c	float	kg/s	averaged suspended load flux
 * vel[]		HydroOutput.c	float	m/s		river velocity
 * wid[]		HydroOutput.c	float	m		river width
 *
 *-------------------------------------------------------------------------------------------*/

#include <string.h>
#include "hydroclimate.h"
#include "hydroparams.h"
#include "hydrotimeser.h"
#include "hydroinout.h"
#include "hydroreadclimate.h"
#include "hydroalloc_mem.h"
#include "hydrofree_mem.h"
#include "hydrodaysmonths.h"
  
/*------------------------
 *  Start of HydroOutput
 *------------------------*/ 
  int
hydrooutput () 
{
  
  
  
  
/*-------------------
 *  Local Variables
 *-------------------*/ 
  int ii, jj, kk, p;
  
  
  
  
  
  
  
  
  
/*---------------------------------------
 *  Allocate memory for multiple outlet
 *---------------------------------------*/ 
    veloutlet = malloc2d (daysiy, maxnoutlet, float);
  
  
  
  
  
  
  
/*------------------------------------------------
 *  Print the header to the binary file (fiddis)
 *------------------------------------------------*/ 
    if (yr == syear[0])
    {
      
        recperyear = 365;
      
      else if (timestep[0] == 'm')
        recperyear = 12;
      
      else if (timestep[0] == 's')
        recperyear = 4;
      
      else
        recperyear = 1;
      
      
      
      
      
      
      
      
      
        
          {
            
            
            
            
            
          
    
  
/*-------------------------------------
 *  Average over the desired interval
 *-------------------------------------*/ 
    if (timestep[0] == 'd')
    {
      
   /*------------------------------------------
    *  Calculate V,W,D.
    *  Daily Qb and Cs are already calculated
    *------------------------------------------*/ 
        for (jj = 0; jj < recperyear; jj++)
        {
          
          
          
          
          
          
          
            
              {
                
                
                  (float) (velcof[ep] * pow (Qavgoutlet[jj][p], velpow[ep]));
                
                  (float) (widcof[ep] * pow (Qavgoutlet[jj][p], widpow[ep]));
                
                  (float) (depcof[ep] * pow (Qavgoutlet[jj][p], deppow[ep]));
                
                
              
    
  
  else if (timestep[0] == 'm')
    {
      
   /*-----------------------------------------------------------
    *  Average over each month and then compute monthly values
    *-----------------------------------------------------------*/ 
        for (jj = 0; jj < recperyear; jj++)
        {
          
          
          
          
            
              {
                
                
                
              
          
            {
              
              
              
              
                
                  {
                    
                    
                    
                  
            
          
          
          
          
          
          
            
              {
                
                
                  (float) (velcof[ep] * pow (Qavgoutlet[jj][p], velpow[ep]));
                
                  (float) (widcof[ep] * pow (Qavgoutlet[jj][p], widpow[ep]));
                
                  (float) (depcof[ep] * pow (Qavgoutlet[jj][p], deppow[ep]));
                
                
              
    
  
  else if (timestep[0] == 's')
    {
      
   /*-------------------------------------------------------------
    *  Average over each season and then compute seasonal values
    *-------------------------------------------------------------*/ 
        for (jj = 0; jj < recperyear; jj++)
        {
          
          
          
          
            
              {
                
                
                
              
          
            {
              
              
              
              
                
                  {
                    
                    
                    
                  
            
          
          
          
          
          
          
            
              {
                
                
                  (float) (velcof[ep] * pow (Qavgoutlet[jj][p], velpow[ep]));
                
                  (float) (widcof[ep] * pow (Qavgoutlet[jj][p], widpow[ep]));
                
                  (float) (depcof[ep] * pow (Qavgoutlet[jj][p], deppow[ep]));
                
                
              
    
  
  else
    {
      
   /*---------------------------------------------------------
    *  Average over each year and then compute annual values
    *---------------------------------------------------------*/ 
        jj = 0;
      
      
      
      
        
          {
            
            
            
            
          
      
        {
          
          
          
          
            
              {
                
                
                
              
        
      
      
      
      
      
      
        
          {
            
            
              (float) (velcof[ep] * pow (Qavgoutlet[jj][p], velpow[ep]));
            
              (float) (widcof[ep] * pow (Qavgoutlet[jj][p], widpow[ep]));
            
              (float) (depcof[ep] * pow (Qavgoutlet[jj][p], deppow[ep]));
            
            
          
    
    
/*------------------------------------------------------
 *	Print the data for each year to the binary file
 *	velocity, width, depth, bedload, concentration per
 *  grain size.
 *------------------------------------------------------*/ 
    for (jj = 0; jj < recperyear; jj++)
    {
      
      
      
      
      
        {
          
          
            
          
      
        
          {
            
            
            
            
            
              {
                
                  (float) (grainpct[kk][ep] * Qsavgoutlet[jj][p] /
                           Qavgoutlet[jj][p]);
                
                  
                
          
    
/*----------------------
 *  Write ascii output
 *----------------------*/ 
    if (strncmp (asciioutput, ON, 2) == 0)
    {
      
        /*----------------
	 *  Print header
	 *----------------*/ 
        if (yr == syear[0])
        {
          
          
          
          
          
          
            
              {
                
                
                
                
                
              
          
          
          
          
          
          
          
          
          
          
          
            
              {
                
                
                
                
                
              
          
          
          
          
          
        
      
        /*----------------
	 *  Print output
	 *----------------*/ 
        for (jj = 0; jj < recperyear; jj++)
        {
          
                    dep[jj]);
          
          
          
            {
              
              
                
              
            
          
          
            
              {
                
                          widoutlet[jj][p], depoutlet[jj][p]);
                
                          veloutlet[jj][p] * widoutlet[jj][p] *
                          depoutlet[jj][p]);
                
                
                  {
                    
                      (float) (grainpct[kk][ep] * Qsavgoutlet[jj][p] /
                               Qavgoutlet[jj][p]);
                    
                      
                    
                  
                
              
          
          
          
          
          
        
    
  
/*---------------
 *  Free memory
 *---------------*/ 
    freematrix2D ((void **) veloutlet, daysiy);
  
  
  
  
  
  
  


