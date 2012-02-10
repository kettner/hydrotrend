/*-------------------------------------------------------------------------------------------
 *	hydroreadhypsom.c
 *
 *	Author: Albert Kettner, March 2006
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
int
hydroreadhypsom (char* path, char* in_file_prefix){

/*-------------------
 *  Local Variables
 *-------------------*/
  double dumdbl, dummyelev, adjusttosealevel, binerror;
  int dumint, err, kk, k, ep;
  char chs[120], dummystring[100]; 

/*------------------------
 *  Initialize Variables
 *------------------------*/
  err = 0;

/*-----------------------
 *  Open the input file
 *-----------------------*/
  fidhyps = allocate_1d_F (nepochs);
  for (ep = 0; ep < nepochs; ep++){
    sprintf (dummystring, "%s/%s%d", path, in_file_prefix, ep);
    //sprintf (dummystring, "%s/%s%d", path, fnamehyps, ep);
    strcpy (ffnamehyps, dummystring);
    strcat (ffnamehyps, fnamehypsext);
    if ((fidhyps[ep] = fopen (ffnamehyps, "r")) == NULL){
      fprintf (stderr,"  openfiles ERROR: Unable to open the hypsometeric integral data file %s \n", ffnamehyps);
      fprintf (stderr, "    Make sure the input file name is all in capitals\n");
      fprintf (stderr, "    program aborted \n");
      exit (1);
    }

/*------------------------
 *  Take off file header
 *------------------------*/
    for (k = 0; k < 5; k++)
      fgets (chs, 120, fidhyps[ep]);
  }

/*------------------
 *  Alocate memory
 *------------------*/
  hypsarea = (double **) malloc ((nepochs) * sizeof (double *));
  if (!hypsarea) {
    perror ("HYDROTREND ERROR; hydroreadhypsom.c\n");
    exit (-1);
  }
  hypselev = (double **) malloc ((nepochs) * sizeof (double *));
  if (!hypselev){
    perror ("HYDROTREND ERROR; hydroreadhypsom.c\n");
    exit (-1);
  }

/*--------------------------------------------
 *  Read the hypsometric data for each epoch
 *--------------------------------------------*/
  for (ep = 0; ep < nepochs; ep++) {
    adjusttosealevel = 0.0;
    fgets (chs, 120, fidhyps[ep]);
    dumint = sscanf (chs, "%d\n", &nhypts[ep]);
    if (dumint == 1) {
      hypselev[ep] = (double *) malloc ((nhypts[ep]) * sizeof (double));
      if (!hypselev[ep]) {
        perror ("HYDROTREND ERROR; hydroreadhypsom.c\n");
        exit (-1);
      }
      hypsarea[ep] = (double *) malloc ((nhypts[ep]) * sizeof (double));
      if (!hypsarea[ep]) {
        perror ("HYDROTREND ERROR; hydroreadhypsom.c\n");
        exit (-1);
      }
      for (kk = 0; kk < nhypts[ep]; kk++){
        fgets (chs, 120, fidhyps[ep]);
        dumint = sscanf (chs, "%lf %lf\n", &dummyelev, &dumdbl);
        if (dumint != 2) {
          printf ("hydrotrend error; hydroreadhypsom.c\n");
          exit (-1);
        }
        if (kk == 0 && dummyelev < 0.0)
          adjusttosealevel = dummyelev;
        hypselev[ep][kk] = dummyelev - adjusttosealevel;
        hypsarea[ep][kk] = dumdbl * 1e6;   /* Convert km^2 to m^2 */

           /*-----------------------------------------------------
            *  Make sure the elevation bin size are all the same
            *-----------------------------------------------------*/
        if (kk == 1)
          elevbinsize = hypselev[ep][kk] - hypselev[ep][kk - 1];
        if (kk > 0) {
          binerror = elevbinsize - (hypselev[ep][kk] - hypselev[ep][kk - 1]);
          if (fabs (binerror) > masscheck) {
            printf ("%lf, %lf, %lf, %lf\n", elevbinsize, hypselev[ep][kk], hypselev[ep][kk - 1], hypselev[ep][kk] - hypselev[ep][kk - 1]);
            printf ("\nhydrotrend ERROR; hydroreadhypsom.c\n");
            printf ("   The elevation bin size is not equal in the hypsometry file\n");
            printf ("   at number %d.\n", kk + 1);
            printf ("   Elevation bin size was set to %lf, calculated binsize at %d is %lf.\n", elevbinsize, kk + 1, hypselev[ep][kk] - hypselev[ep][kk - 1]);
            printf ("   Elevation = %lf\n\n", hypselev[ep][kk]);
            exit (-1);
          }
        }
      }
      fgets (chs, 120, fidhyps[ep]);
    }
    else
      printf ("hydrotrend ERROR; hydroreadhypsom.c\n");
    /*------------------
     *  Close the file
     *------------------*/
    fclose (fidhyps[ep]);
  }


/*--------------------------------------------------------
 *  Are the hypsometric curves monotonically increasing?
 *--------------------------------------------------------*/
  for (ep = 0; ep < nepochs; ep++)
    for (kk = 0; kk < nhypts[ep]; kk++) {
      if (kk > 0 && (hypselev[ep][kk] <= hypselev[ep][kk - 1] || hypsarea[ep][kk] <= hypsarea[ep][kk - 1])) {
        fprintf (stderr, "   HydroReadHypsom ERROR: \n");
        fprintf (stderr, "	 Altitude and area in digitized hypsometric  \n");
        fprintf (stderr, "	 data file must be monotonically increasing. \n");
        fprintf (stderr, "	 kk = %d \n", kk);
        fprintf (stderr, "	 hypselev[ep][kk-1] \t = %f \n", hypselev[ep][kk - 1]);
        fprintf (stderr, "	 hypselev[ep][kk]   \t = %f \n", hypselev[ep][kk]);
        fprintf (stderr, "	 hypsarea[ep][kk-1] \t = %f \n", hypsarea[ep][kk - 1]);
        fprintf (stderr, "	 hypsarea[ep][kk]   \t = %f \n", hypsarea[ep][kk]);
        err++;
      }
    }

/*----------------------------------------------------
 *  Use hypsometric data to set maxalt and totalarea
 *----------------------------------------------------*/
  for (ep = 0; ep < nepochs; ep++) {
    totalarea[ep] = hypsarea[ep][nhypts[ep] - 1];
    maxalt[ep] = hypselev[ep][nhypts[ep] - 1];
  }
  return (err);
}                               /* end of HydroReadHypsom */
