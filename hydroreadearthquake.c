/*-------------------------------------------------------------------------------------------
 *	hydroreadearthquake.c
 *
 *	Author: Albert Kettner, September 2011
 *
 *	Reads digitized hypsometeric integral data from file.
 *
 *	Variable		Def.Location	Type		Units	Usage
 *	--------		------------	----		-----	-----
 *
 *-------------------------------------------------------------------------------------------*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "hydroinout.h"
#include "hydroparams.h"        /* includes <stdio.h> */
#include "hydroalloc_mem.h"

/*----------------------------
 *  Start of HydroReadHypsom
 *----------------------------*/
int hydroreadearthquake (char* path, char* in_file_prefix){
  
  /*-------------------
   *  Local Variables
   *-------------------*/
  double binerror;
  int dumint, err, kk, k, ep;
  char chs[220], dummystring[100];
  
  /*------------------------
   *  Initialize Variables
   *------------------------*/
  err = 0;

  /*------------------
   *  Alocate memory
   *------------------*/
  fidquake = allocate_1d_F (nepochs);
  earthquakedatafile = (int *) malloc ((nepochs) * sizeof (int));

  quakeeventcounter = (int *) malloc ((nepochs) * sizeof (int));
  if (!quakeeventcounter){
    perror ("HYDROTREND ERROR; hydroreadearthquake.c\n");
    exit (-1);
  }
  quakeeventyear = (int **) malloc ((nepochs) * sizeof (int *));
  if (!quakeeventyear){
    perror ("HYDROTREND ERROR; hydroreadearthquake.c\n");
    exit (-1);
  }
  quakeeventenergy = (double **) malloc ((nepochs) * sizeof (double *));
  if (!quakeeventenergy){
    perror ("HYDROTREND ERROR; hydroreadearthquake.c\n");
    exit (-1);
  }
  quakeeventdistance = (double **) malloc ((nepochs) * sizeof (double *));
  if (!quakeeventdistance){
    perror ("HYDROTREND ERROR; hydroreadearthquake.c\n");
    exit (-1);
  }
  quakeeventduration = (double **) malloc ((nepochs) * sizeof (double *));
  if (!quakeeventduration){
    perror ("HYDROTREND ERROR; hydroreadearthquake.c\n");
    exit (-1);
  }
  
  /*--------------------------------------------
   *  Read the earthquake data for each epoch
   *--------------------------------------------*/
  for (ep = 0; ep < nepochs; ep++){

    /*-----------------------
     *  Open the input file
     *-----------------------*/
    sprintf (dummystring, "%s/%s%d", path, in_file_prefix, ep);
    //sprintf (dummystring, "%s/%s%d", path, fnamehyps, ep);
    strcpy (ffnamequake, dummystring);
    strcat (ffnamequake, fnamequakeext);
    if ((fidquake[ep] = fopen (ffnamequake, "r")) == NULL){
      fprintf (stderr, "  openfiles WARNING: Unable to open the earthquake data file %s \n", ffnamequake);
      fprintf (stderr, "    Hydrotrend will run without the earthquake routine  for epoch %d\n", ep);
      earthquakedatafile[ep] = 0;      
    }
    else
      earthquakedatafile[ep] = 1;    
    if (earthquakedatafile[ep] == 1){
      
      /*------------------------
       *  Take off file header
       *------------------------*/      
      for (k = 0; k < 5; k++)
        fgets (chs, 220, fidquake[ep]);

      fgets (chs, 220, fidquake[ep]);
      dumint = sscanf (chs, "%d\n", &quakeeventcounter[ep]);
      if (dumint == 1){
        quakeeventyear[ep] = (int *) malloc ((quakeeventcounter[ep]) * sizeof (int));
        if (!quakeeventyear[ep]){
          perror ("HYDROTREND ERROR; hydroreadearthquake.c\n");
          exit (-1);
        }
        quakeeventenergy[ep] = (double *) malloc ((quakeeventcounter[ep]) * sizeof (double));
        if (!quakeeventenergy[ep]){
          perror ("HYDROTREND ERROR; hydroreadearthquake.c\n");
          exit (-1);
        }
        quakeeventdistance[ep] = (double *) malloc ((quakeeventcounter[ep]) * sizeof (double));
        if (!quakeeventdistance[ep]){
          perror ("HYDROTREND ERROR; hydroreadearthquake.c\n");
          exit (-1);
        }
        quakeeventduration[ep] = (double *) malloc ((quakeeventcounter[ep]) * sizeof (double));
        if (!quakeeventduration[ep]){
          perror ("HYDROTREND ERROR; hydroreadearthquake.c\n");
          exit (-1);
        }
        for (kk = 0; kk < quakeeventcounter[ep]; kk++){
          fgets (chs, 220, fidquake[ep]);
          dumint = sscanf (chs, "%d %lf %lf %lf\n", &quakeeventyear[ep][kk], &quakeeventenergy[ep][kk], &quakeeventdistance[ep][kk], &quakeeventduration[ep][kk]);
          if (dumint != 4){
            printf ("hydrotrend error: hydroreadearthquake.c; %d, %d, %lf, %lf, %lf\n", dumint, quakeeventyear[ep][kk], quakeeventenergy[ep][kk], quakeeventdistance[ep][kk], quakeeventduration[ep][kk]);
            exit (-1);
          }
        }
        fgets (chs, 220, fidquake[ep]);  /*not sure if we need this here*/
      }
      else if (dumint > 1){
        fprintf (stderr, "Hydrotrend earthquake error in file %s \n",ffnamequake);
        fprintf (stderr, "    Expected a single number, expressing the number of earthquake events; not %d values", dumint);
        fprintf (stderr, "    Program aborted \n");
        exit (-1);
      }
      
      /*------------------
       *  Close the file
       *------------------*/
      fclose (fidquake[ep]);
      
    }
  }
  return (err);
}                     /* end of HydroReadearthquake */

