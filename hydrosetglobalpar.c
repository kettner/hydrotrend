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

FILE *fidinput;
FILE *fidtrend1;
FILE *fidtrend2;
FILE *fidtrend3;
FILE *fidstat;
FILE *fiddistot;
FILE **fiddis;
FILE *fidconvdistot;
FILE **fidconvdis;
FILE *fidinputgw_r;
FILE **fidhyps;
FILE *fidlog;
FILE *fidlapserate;
FILE *outp, *outp1, *outp2, *outp3, *outp4, *outp5;

char title[MAXCH];
char moname[12][4];
char ffnametrend1[MAXCH];
char ffnametrend2[MAXCH];
char ffnametrend3[MAXCH];
char ffnamestat[MAXCH];
char ffnamedis[MAXCH];
char ffnamedistot[MAXCH];
char ffnameconvdis[MAXCH];
char ffnameconvdistot[MAXCH];
char ffnamehyps[MAXCH];
char ffnameinputgw_r[MAXCH];
char ffnamelog[MAXCH];
char ffidasc[MAXCH];
char ffidasc1[MAXCH];
char ffidasc2[MAXCH];
char ffidasc3[MAXCH];
char ffidasc4[MAXCH];
char ffidasc5[MAXCH];


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
  char chs[120], dumchar;
  double dumlon, dumlat, dumlapserate;
  long counter;

/*------------------------
 *  Initialize Variables
 *------------------------*/
  err = 0;
  dumlon = 0;

/*------------------------------------
 *  Start looking up lapserate value
 *------------------------------------*/
  if (lapserateflag == 1)
    {
      if (verbose)
        printf ("Opening %s... \n", fnamelapserate);
      if ((fidlapserate = fopen (fnamelapserate, "r")) == NULL)
        {
          fprintf (stderr,
                   "  HydroSetGlobalPar ERROR: Unable to open the lapserate table file %s \n",
                   fnamelapserate);
          exit (-1);
        }
      dumint = 5;
      ep = 0;
      for (ep = 0; ep < nepochs; ep++)
        {
          if (lon > 357.5)
            lon = 0.0;
          fgets (chs, 120, fidlapserate);
          fgets (chs, 120, fidlapserate);
          sscanf (chs, "%li\n", &counter);
          for (i = 0; i < counter; i++)
            while ((dumlon < lon && dumint == 5)
                   || (dumlat > lat && dumint == 5))
              {
                fgets (chs, 120, fidlapserate);
                dumint =
                  sscanf (chs, "%lf%c %lf%c %lf\n", &dumlon, &dumchar, &dumlat,
                          &dumchar, &dumlapserate);
                if (dumlon >= lon)
                  while (dumlat > lat && dumint == 5)
                    {
                      fgets (chs, 120, fidlapserate);
                      dumint =
                        sscanf (chs, "%lf%c %lf%c %lf\n", &dumlon, &dumchar,
                                &dumlat, &dumchar, &dumlapserate);
                      if (dumlat <= lat)
                        {
                          lapserate[ep] = (dumlapserate / 1000.0);
                          i = counter;
                        }
                    }
              }
          if (dumint != 5)
            {
              fprintf (stderr,
                       "  HydroSetGlobalPar ERROR: In lapserate table file %s \n",
                       fnamelapserate);
              fprintf (stderr,
                       "     File is corrupt, Unable to read all the variables\n");
              err++;
            }
          rewind (fidlapserate);
        }
      fclose (fidlapserate);
    }
  return (err);
}                               /* end HydroSetGlobalPar.c */
