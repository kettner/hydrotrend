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
#include "hydroparams.h"       /* includes <stdio.h> */
#include "hydroalloc_mem.h"
  
/*----------------------------
 *  Start of HydroReadHypsom
 *----------------------------*/ 
  int
hydroreadhypsom () 
{
  
/*-------------------
 *  Local Variables
 *-------------------*/ 
  double dumdbl, dummyelev, adjusttosealevel, binerror;
  int dumint, err, kk, i, k, ep;
  char chs[120], dummystring[100];
  
/*------------------------
 *  Initialize Variables
 *------------------------*/ 
    err = 0;
  
/*-----------------------
 *  Open the input file
 *-----------------------*/ 
    if ((fidhyps = fopen (fnamehyps, "r")) == NULL)
    {
      fprintf (stderr,
                "  openfiles ERROR: Unable to open the hypsometeric integral data file %s \n",
                fnamehyps);
      fprintf (stderr,
                "    Make sure the input file name is all in capitals\n");
      fprintf (stderr, "    program aborted \n");
      exit (1);
    }
  
/*------------------------
 *  Take off file header
 *------------------------*/ 
    for (k = 0; k < 5; k++)
    fgets (chs, 120, fidhyps);
  
/*--------------------------------------------
 *  Read the hypsometric data for each epoch
 *--------------------------------------------*/ 
    k = -1;
  adjusttosealevel = 0.0;
  fgets (chs, 120, fidhyps);
  dumint = sscanf (chs, "%d\n", &nhypts);
  if (dumint == 1)
    {
      hypselev = (double *) malloc ((nhypts) * sizeof (double));
      if (!hypselev)
        {
          perror ("HYDROTREND ERROR; hydroreadhypsom.c\n");
          exit (-1);
        }
      hypsarea = (double *) malloc ((nhypts) * sizeof (double));
      if (!hypsarea)
        {
          perror ("HYDROTREND ERROR; hydroreadhypsom.c\n");
          exit (-1);
        }
      for (kk = 0; kk < nhypts; kk++)
        {
          fgets (chs, 120, fidhyps);
          dumint = sscanf (chs, "%lf %lf\n", &dummyelev, &dumdbl);
          if (dumint != 2)
            {
              printf ("hydrotrend error; hydroreadhypsom.c\n");
              exit (-1);
            }
          if (kk == 0 && dummyelev < 0.0)
            adjusttosealevel = dummyelev;
          hypselev[kk] = dummyelev - adjusttosealevel;
          hypsarea[kk] = dumdbl * 1e6; /* Convert km^2 to m^2 */
          
           /*-----------------------------------------------------
            *  Make sure the elevation bin size are all the same
            *-----------------------------------------------------*/ 
            if (kk == 1)
            elevbinsize = hypselev[kk] - hypselev[kk - 1];
          if (kk > 0)
            {
              binerror = elevbinsize - (hypselev[kk] - hypselev[kk - 1]);
              if (fabs (binerror) > masscheck)
                {
                  printf ("%lf, %lf, %lf, %lf\n", elevbinsize, hypselev[kk],
                           hypselev[kk - 1], hypselev[kk] - hypselev[kk - 1]);
                  printf ("\nhydrotrend ERROR; hydroreadhypsom.c\n");
                  printf
                    ("   The elevation bin size is not equal in the hypsometry file\n");
                  printf ("   at number %d.\n", kk + 1);
                  printf
                    ("   Elevation bin size was set to %lf, calculated binsize at %d is %lf.\n",
                     elevbinsize, kk + 1, hypselev[kk] - hypselev[kk - 1]);
                  printf ("   Elevation = %lf\n\n", hypselev[kk]);
                  exit (-1);
                }
            }
        }
      fgets (chs, 120, fidhyps);
    }
  k++;
  
/*------------------
 *  Close the file
 *------------------*/ 
    fclose (fidhyps);
  
/*-----------------------
 *  Is # of points > 1?		
 *----------------------*/ 
    if (nhypts <= 1)
    {
      fprintf (stderr, "   HydroReadHypsom ERROR: \n");
      fprintf (stderr,
                "	 Number of hypsometric points read is less than 2. \n");
      fprintf (stderr, "	 nhypts = %d\n", nhypts);
      err++;
    }
  
/*--------------------------------------------------------
 *  Are the hypsometric curves monotonically increasing?
 *--------------------------------------------------------*/ 
    for (kk = 0; kk < nhypts; kk++)
    if (kk > 0
         && (hypselev[kk] <= hypselev[kk - 1]
             || hypsarea[kk] <= hypsarea[kk - 1]))
      {
        fprintf (stderr, "   HydroReadHypsom ERROR: \n");
        fprintf (stderr,
                  "	 Altitude and area in digitized hypsometric  \n");
        fprintf (stderr,
                  "	 data file must be monotonically increasing. \n");
        fprintf (stderr, "	 kk = %d \n", kk);
        fprintf (stderr, "	 hypselev[ep][kk-1] \t = %f \n",
                  hypselev[kk - 1]);
        fprintf (stderr, "	 hypselev[ep][kk]   \t = %f \n", hypselev[kk]);
        fprintf (stderr, "	 hypsarea[ep][kk-1] \t = %f \n",
                  hypsarea[kk - 1]);
        fprintf (stderr, "	 hypsarea[ep][kk]   \t = %f \n", hypsarea[kk]);
        err++;
      }
  
/*----------------------------------------------------
 *  Use hypsometric data to set maxalt and totalarea
 *----------------------------------------------------*/ 
    totalarea = hypsarea[nhypts - 1];
  maxalt = hypselev[nhypts - 1];
  return (err);
}                              /* end of HydroReadHypsom */


