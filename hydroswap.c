/*-------------------------------------------------------------------------------------------
 *  hydroswap.c
 *
 *	Author: Albert Kettner, March 2006
 *
 *  swab - Convert between big-endian and little-endian file formats. It creates a second
 *	binary file which simulair to the created output file of HYDROTREND but then in a swabbed
 *	order. So if you are running hydrotrend on a PC you can read the swabbed binary file on an
 *	Unix platform and visa versa.
 *
 * Variable		Def.Location	Type	Units	Usage
 * --------		------------	----	-----	-----
 * comLen		HydroSwap.c	int	-	length of the title string
 * err  		various 	int	-	error flag, halts program
 * i			various		int	-	temporary loop counter
 * nYears		HydroSwap.c	int	-	total # of output years
 * recperyear   	HydroSwap.c	int	-	# of output records per year
 * start                HydroSwap.c     int     -       Start for filepointer after reading part of Header
 * word                 HydroSwap.c     int     -       Number of bytes of the input file
 * dataWord             HydroSwap.c     char    -       Holding byte info of each byte in a lope
 *
 *-------------------------------------------------------------------------------------------*/
   
  
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "hydroinout.h"
#include "hydroparams.h"
#include "hydroclimate.h"
#include "hydrotimeser.h"
#include "hydroalloc_mem.h"
#define DEFAULT_WORD 4
  
/*----------------------
 *  Start main program
 *----------------------*/ 
  int
hydroswap () 
{
  
/*-------------------
 *  Local Variables
 *-------------------*/ 
  int i, err, comLen, nYears, p;
  
  
  
  
  
  
/*------------------------
 *  Initialize Variables
 *------------------------*/ 
    word = DEFAULT_WORD;
  
  
/*------------------------
 *  Opening binary files
 *------------------------*/ 
    if (verbose)
    printf ("Opening %s... \n", ffnamedistot);
  
    {
      
                "  HydroSwap ERROR: Unable to open the discharge file %s \n",
                ffnamedistot);
      
    
  
  
  
    printf ("Opening %s... \n", ffnameconvdistot);
  
    {
      
                "  HydroSwap ERROR: Unable to open the convdischarge file %s \n",
                ffnameconvdistot);
      
    
  
    {
      
      
        {
          
          
          
          
          
            printf ("Opening %s... \n", ffnamedis);
          
            {
              
                        "  HydroSwap ERROR: Unable to open the discharge file %s \n",
                        ffnamedis);
              
            
          
          
          
          
          
            printf ("Opening %s... \n", ffnameconvdis);
          
            {
              
                        "  HydroSwap ERROR: Unable to open the convdischarge file %s \n",
                        ffnameconvdis);
              
            
        
    
  
/*-------------------
 *  Allocate memory
 *-------------------*/ 
    dataWord = (char *) malloc (sizeof (char) * word);
  
/*------------------------------------
 *  Setting variables for the header
 *------------------------------------*/ 
    if (timestep[0] == 'd')
    recperyear = 365;
  
  else if (timestep[0] == 'm')
    recperyear = 12;
  
  else if (timestep[0] == 's')
    recperyear = 4;
  
  else
    recperyear = 1;
  
  
  
  
/*--------------------------------------------
 *  Swapping parts of header + rest datafile
 *--------------------------------------------*/ 
    fread (dataWord, 1, word, fiddistot);
  
    
  
  
  
    
      
  
    
      {
        
        
          
        
        
        
          
            
      
  
/*-------------------
 *  Close files
 *------------------*/ 
    fclose (fiddistot);
  
  
    
      {
        
        
      
  


