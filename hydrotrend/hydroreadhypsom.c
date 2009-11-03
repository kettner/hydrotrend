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
hydroreadhypsom (char* path, char* in_file_prefix)
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
  fidhyps = allocate_1d_F (nepochs);
  for (ep = 0; ep < nepochs; ep++)
    {
      sprintf (dummystring, "%s/%s%d", path, in_file_prefix, ep);
      //sprintf (dummystring, "%s/%s%d", path, fnamehyps, ep);
      strcpy (ffnamehyps, dummystring);
      strcat (ffnamehyps, fnamehypsext);
      if ((fidhyps[ep] = fopen (ffnamehyps, "r")) == NULL)
        {
          fprintf (stderr,
                   "  openfiles ERROR: Unable to open the hypsometeric integral data file %s \n",
                   ffnamehyps);
          fprintf (stderr,
                   "    Make sure the input file name is all in capitals\n");
          fprintf (stderr, "    program aborted \n");
          exit (1);
        }
    }

/*------------------------
 *  Take off file header
 *------------------------*/
  for (ep = 0; ep < nepochs; ep++)
    for (k = 0; k < 5; k++)
      fgets (chs, 120, fidhyps[ep]);

/*------------------
 *  Alocate memory
 *------------------*/
  hypsarea = (double **) malloc ((nepochs) * sizeof (double *));
  if (!hypsarea)
    {
      perror ("HYDROTREND ERROR; hydroreadhypsom.c\n");
      exit (-1);
    }
  hypselev = (double **) malloc ((nepochs) * sizeof (double *));
  if (!hypselev)
    {
      perror ("HYDROTREND ERROR; hydroreadhypsom.c\n");
      exit (-1);
    }

/*--------------------------------------------
 *  Read the hypsometric data for each epoch
 *--------------------------------------------*/
  k = -1;
  for (i = 0; i < nepochs; i++)
    {
      adjusttosealevel = 0.0;
      fgets (chs, 120, fidhyps[i]);
      dumint = sscanf (chs, "%d\n", &nhypts[i]);
      if (dumint == 1)
        {
          hypselev[i] = (double *) malloc ((nhypts[i]) * sizeof (double));
          if (!hypselev[i])
            {
              perror ("HYDROTREND ERROR; hydroreadhypsom.c\n");
              exit (-1);
            }
          hypsarea[i] = (double *) malloc ((nhypts[i]) * sizeof (double));
          if (!hypsarea[i])
            {
              perror ("HYDROTREND ERROR; hydroreadhypsom.c\n");
              exit (-1);
            }
          for (kk = 0; kk < nhypts[i]; kk++)
            {
              fgets (chs, 120, fidhyps[i]);
              dumint = sscanf (chs, "%lf %lf\n", &dummyelev, &dumdbl);
              if (dumint != 2)
                {
                  printf ("hydrotrend error; hydroreadhypsom.c\n");
                  exit (-1);
                }
              if (kk == 0 && dummyelev < 0.0)
                adjusttosealevel = dummyelev;
              hypselev[i][kk] = dummyelev - adjusttosealevel;
              hypsarea[i][kk] = dumdbl * 1e6;   /* Convert km^2 to m^2 */

           /*-----------------------------------------------------
            *  Make sure the elevation bin size are all the same
            *-----------------------------------------------------*/
              if (kk == 1)
                elevbinsize = hypselev[i][kk] - hypselev[i][kk - 1];
              if (kk > 0)
                {
                  binerror =
                    elevbinsize - (hypselev[i][kk] - hypselev[i][kk - 1]);
                  if (fabs (binerror) > masscheck)
                    {
                      printf ("%lf, %lf, %lf, %lf\n", elevbinsize,
                              hypselev[i][kk], hypselev[i][kk - 1],
                              hypselev[i][kk] - hypselev[i][kk - 1]);
                      printf ("\nhydrotrend ERROR; hydroreadhypsom.c\n");
                      printf
                        ("   The elevation bin size is not equal in the hypsometry file\n");
                      printf ("   at number %d.\n", kk + 1);
                      printf
                        ("   Elevation bin size was set to %lf, calculated binsize at %d is %lf.\n",
                         elevbinsize, kk + 1,
                         hypselev[i][kk] - hypselev[i][kk - 1]);
                      printf ("   Elevation = %lf\n\n", hypselev[i][kk]);
                      exit (-1);
                    }
                }
            }
          fgets (chs, 120, fidhyps[i]);
        }

      else
        i = nepochs;
      k++;
    }

/*------------------
 *  Close the file
 *------------------*/
  for (i = 0; i < nepochs; i++)
    {
      fclose (fidhyps[i]);

/*----------------------------------------------------------
 *  Warning messages if nepochs != nr. of hyps. data files
 *----------------------------------------------------------*/
      if (k == 0 && nepochs > 1)
        {
          fprintf (stderr, "\n WARNING in HydroReadHypsom: \n");
          fprintf (stderr,
                   "    Hydrotrend found only 1 hypsometric data file for  \n");
          fprintf (stderr, "    multiple Epochs. \n");
          fprintf (stderr,
                   "    The original hypsometric data file will be used for ALL \n");
          fprintf (stderr,
                   "    epochs.  Changes in maximum elevation (or sealevel) may cause\n");
          fprintf (stderr, "    subtle (or obvious) errors. \n\n");
          fprintf (fidlog, "\n WARNING in HydroReadHypsom: \n");
          fprintf (fidlog,
                   "    Hydrotrend found only 1 hypsometric data file for  \n");
          fprintf (fidlog, "    multiple Epochs. \n");
          fprintf (fidlog,
                   "    The original hypsometric data file will be used for ALL \n");
          fprintf (fidlog,
                   "    epochs.  Changes in maximum elevation (or sealevel) may cause\n");
          fprintf (fidlog, "    subtle (or obvious) errors. \n\n");
        }
      if (k != 0 && k < nepochs - 1)
        {
          fprintf (stderr, "\n WARNING in HydroReadHypsom: \n");
          fprintf (stderr,
                   "    Hydrotrend found only less hypsometric data files then  \n");
          fprintf (stderr, "    the number of Epochs. \n");
          fprintf (stderr,
                   "    The last found hypsometric data file will be used for \n");
          fprintf (stderr,
                   "    remaining epochs.  Changes in maximum elevation (or sealevel)\n");
          fprintf (stderr, "    may cause subtle (or obvious) errors. \n\n");
          fprintf (fidlog, "\n WARNING in HydroReadHypsom: \n");
          fprintf (fidlog,
                   "    Hydrotrend found only less hypsometric data files then  \n");
          fprintf (fidlog, "    the number of Epochs. \n");
          fprintf (fidlog,
                   "    The last found hypsometric data file will be used for \n");
          fprintf (fidlog,
                   "    remaining epochs.  Changes in maximum elevation (or sealevel)\n");
          fprintf (fidlog, "    may cause subtle (or obvious) errors. \n\n");
        }

/*-----------------------------------------------------------------------------
 *  Adusting nr. of hyps. data files to the nr. of epochs if they don't match
 *-----------------------------------------------------------------------------*/
      if (k == 0 && nepochs > 1)
        {
          for (k = 0; k < nepochs; k++)
            {
              hypselev[k + 1] =
                (double *) malloc ((nhypts[k]) * sizeof (double));
              if (!hypselev[k + 1])
                {
                  perror ("HYDROTREND ERROR; hydroreadhypsom.c\n");
                  exit (-1);
                }
              hypsarea[k + 1] =
                (double *) malloc ((nhypts[k]) * sizeof (double));
              if (!hypsarea[k + 1])
                {
                  perror ("HYDROTREND ERROR; hydroreadhypsom.c\n");
                  exit (-1);
                }
              for (kk = 0; kk < nhypts[k]; kk++)
                {
                  hypsarea[k + 1][kk] = hypsarea[k][kk];
                  hypselev[k + 1][kk] = hypselev[k][kk];
                }
              nhypts[k + 1] = nhypts[k];
            }
        }
      if (k != 0 && i < nepochs - 1)
        {
          for (k = k; k < nepochs; k++)
            {
              hypselev[k + 1] =
                (double *) malloc ((nhypts[k]) * sizeof (double));
              if (!hypselev[k + 1])
                {
                  perror ("HYDROTREND ERROR; hydroreadhypsom.c\n");
                  exit (-1);
                }
              hypsarea[k + 1] =
                (double *) malloc ((nhypts[k]) * sizeof (double));
              if (!hypsarea[k + 1])
                {
                  perror ("HYDROTREND ERROR; hydroreadhypsom.c\n");
                  exit (-1);
                }
              for (kk = 0; kk < nhypts[k]; kk++)
                {
                  hypsarea[k + 1][kk] = hypsarea[k][kk];
                  hypselev[k + 1][kk] = hypselev[k][kk];
                }
              nhypts[k + 1] = nhypts[k];
            }
        }

/*-----------------------
 *  Is # of points > 1?		
 *----------------------*/
      if (nhypts[i] <= 1)
        {
          fprintf (stderr, "   HydroReadHypsom ERROR: \n");
          fprintf (stderr,
                   "	 Number of hypsometric points read is less than 2. \n");
          fprintf (stderr, "	 nhypts = %d\n", nhypts[i]);
          err++;
        }
    }

/*--------------------------------------------------------
 *  Are the hypsometric curves monotonically increasing?
 *--------------------------------------------------------*/
  for (i = 0; i < nepochs; i++)
    for (kk = 0; kk < nhypts[i]; kk++)
      {
        if (kk > 0
            && (hypselev[i][kk] <= hypselev[i][kk - 1]
                || hypsarea[i][kk] <= hypsarea[i][kk - 1]))
          {
            fprintf (stderr, "   HydroReadHypsom ERROR: \n");
            fprintf (stderr,
                     "	 Altitude and area in digitized hypsometric  \n");
            fprintf (stderr,
                     "	 data file must be monotonically increasing. \n");
            fprintf (stderr, "	 kk = %d \n", kk);
            fprintf (stderr, "	 hypselev[ep][kk-1] \t = %f \n",
                     hypselev[ep][kk - 1]);
            fprintf (stderr, "	 hypselev[ep][kk]   \t = %f \n",
                     hypselev[ep][kk]);
            fprintf (stderr, "	 hypsarea[ep][kk-1] \t = %f \n",
                     hypsarea[ep][kk - 1]);
            fprintf (stderr, "	 hypsarea[ep][kk]   \t = %f \n",
                     hypsarea[ep][kk]);
            err++;
          }
      }

/*----------------------------------------------------
 *  Use hypsometric data to set maxalt and totalarea
 *----------------------------------------------------*/
  for (i = 0; i < nepochs; i++)
    {
      totalarea[i] = hypsarea[i][nhypts[i] - 1];
      maxalt[i] = hypselev[i][nhypts[i] - 1];
    }
  return (err);
}                               /* end of HydroReadHypsom */
