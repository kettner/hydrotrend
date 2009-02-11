/*-------------------------------------------------------------------------------------------
 *  hydrosetglobalpar.c
 *
 *	Author: Albert Kettner, March 2006
 *
 *	Set lapserate value if its not defined in the main input file
 *
 * Variable		Def.Location	Type		Units	Usage
 * --------		------------	----		-----	-----
 * err			various			int			-		error flag, halts program
 *
 *-------------------------------------------------------------------------------------------*/

#include "hydrofree_mem.h"
#include "hydroclimate.h"
#include "hydroinout.h"
#include "hydroparams.h"
#include "hydrotimeser.h"
#include "hydroalloc_mem.h"
#include "hydroreadclimate.h"
  
/*---------------------
 *  Start the program
 *---------------------*/ 
  int
hydrosetglobalpar () 
{
  
/*-------------------
 *  Local Variables
 *-------------------*/ 
  int err, dumint, i;
  
  
  
  
/*------------------------
 *  Initialize Variables
 *------------------------*/ 
    err = 0;
  
  
/*------------------------------------
 *  Start looking up lapserate value
 *------------------------------------*/ 
    if (lapserateflag == 1)
    {
      
        printf ("Opening %s... \n", fnamelapserate);
      
        {
          
                    "  HydroSetGlobalPar ERROR: Unable to open the lapserate table file %s \n",
                    fnamelapserate);
          
        
      
      
      
        {
          
            
          
          
          
          
            
                    || (dumlat > lat && dumint == 5))
              {
                
                
                  sscanf (chs, "%lf%c %lf%c %lf\n", &dumlon, &dumchar, &dumlat,
                          &dumchar, &dumlapserate);
                
                  
                    {
                      
                      
                        sscanf (chs, "%lf%c %lf%c %lf\n", &dumlon, &dumchar,
                                &dumlat, &dumchar, &dumlapserate);
                      
                        {
                          
                          
                        
                    
              
          
            {
              
                        "  HydroSetGlobalPar ERROR: In lapserate table file %s \n",
                        fnamelapserate);
              
                        "     File is corrupt, Unable to read all the variables\n");
              
            
          
        
      
    
  


