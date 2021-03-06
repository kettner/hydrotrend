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
hydroreadinput (char* file) 
{
  
/*-------------------
 *  Local Variables
 *-------------------*/ 
  char chs[150], dumchr[2];
  int jj, err, dumint, totyears, k;
  double dumdbl, evapotranspiration;
  char dummyx;
  
/*------------------------
 *  Initialize Variables
 *------------------------*/ 
    err = 0;
  k = 0;
  totyears = 0;
  
/*-----------------------
 *  Open the input file
 *-----------------------*/ 
    if ((fidinput = fopen (file, "r")) == NULL)
    {
      fprintf (stderr,
                "  HydroReadInput.c ERROR: Unable to open the input file %s \n",
                file);
      fprintf (stderr, "    Make sure the input file name is HYDRO.IN\n");
      fprintf (stderr, "    program aborted \n");
      
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
      fscanf (fidinput, "%c", &asciioutput[jj]);
      asciioutput[jj] = toupper (asciioutput[jj]);
      if (asciioutput[jj] == ' ')
        jj = MAXCHAR;
    }
  fgets (chs, 150, fidinput);
  
/*------------------------------------
 *  3) Reed the output directory in
 *------------------------------------*/ 
    for (jj = 0; jj < MAXDIR; jj++)
    {
      fscanf (fidinput, "%c", &directory[jj]);
      if (directory[jj] == ' ' || directory[jj] == '\t')
        {
          if (directory[jj - 1] != '/')
            directory[jj] = '/';
          
          else
            directory[jj] = '\0';
          jj = MAXDIR;
        }
    }
  fgets (chs, 150, fidinput);
  
/*-----------------------------------
 *  4) Read in number of epochs (-)
 *-----------------------------------*/ 
    fscanf (fidinput, "%d", &nepochs);
  fgets (chs, 150, fidinput);
  
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
      fscanf (fidinput, "%d %d ", &syear[ep], &nyears[ep]);
      fgets (timestep, 2, fidinput);
      timestep[0] = tolower (timestep[0]);
      fgets (chs, 150, fidinput);
      if (ep != 0 && dumchr[0] != timestep[0])
        {
          fprintf (stderr,
                    "   HydroReadInput.c WARNING: timestep changed between epochs. \n");
          fprintf (stderr,
                    "      Hydrotrend will use the timestep from the begining epoch. \n");
          fprintf (stderr, "      Present epoch = %d \n", ep + 1);
          fprintf (stderr, "      epoch # 1 timestep = %s \n", dumchr);
          fprintf (stderr, "      epoch # %d timestep = %s \n", ep, timestep);
          fprintf (stderr, "      syear=%d, nyears=%d\n", syear[ep],
                    nyears[ep]);
          strcpy (timestep, dumchr);
        }
      
   /*---------------------------------------------------
    *  7) Read number of grain sizes to simulate (int)
    *   Must be constant for all epochs
    *---------------------------------------------------*/ 
        if (ep == 0)
        {
          fscanf (fidinput, "%d ", &ngrain);
          fgets (chs, 150, fidinput);
        }
      
      else
        {
          fscanf (fidinput, "%d ", &dumint);
          fgets (chs, 150, fidinput);
          if (dumint != ngrain)
            {
              fprintf (stderr,
                        "  HydroReadInput.c ERROR: ngrain must be constant for all epochs. \n");
              fprintf (stderr, "     present epoch = %d \n", ep + 1);
              fprintf (stderr, "     initial ngrain = %d \n", ngrain);
              fprintf (stderr, "     present ngrain = %d \n", dumint);
              err = 1;
            }
        }
      
   /*---------------------------------------------
    *  8) Read percentage of each grain size (%)
    *---------------------------------------------*/ 
        for (jj = 0; jj < ngrain; jj++)
        fscanf (fidinput, "%lf  ", &grainpct[jj][ep]);
      fgets (chs, 150, fidinput);
      
   /*--------------------------------------------------------------
    *  9) Read temperature trend parameters ( degC, degC/a, degC)
    *--------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf %lf ", &Tstart[ep], &Tchange[ep], &Tstd[ep]);
      fgets (chs, 150, fidinput);
      
   /*--------------------------------------------------------------
    *  10) Read precipitation trend parameters (m/a, (m/a)/a, m/a)
    *--------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf %lf ", &Pstart[ep], &Pchange[ep], &Pstd[ep]);
      fgets (chs, 150, fidinput);
      
   /*-------------------------------------------------------
    *  11) Read rainfall mass balance parameters ( %, -, -)
    *-------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf %lf ", &Pmassbal[ep], &Pexponent[ep],
                &Prange[ep]);
      fgets (chs, 150, fidinput);
      
   /*-------------------------------------------------
    *  12) Read constant base flow discharge (m^3/s)
    *-------------------------------------------------*/ 
        fscanf (fidinput, "%lf ", &baseflowtot[ep]);
      fgets (chs, 150, fidinput);
      
   /*--------------------------------------------------------------------------
    *  13-24) Read climate statistics ( month, degC, degC, mm -> m, mm -> m )
    *--------------------------------------------------------------------------*/ 
        for (jj = 0; jj < 12; jj++)
        {
          fscanf (fidinput, "%3s %lf %lf %lf %lf ", moname[jj],
                   &Tnominal[jj][ep], &Tnomstd[jj][ep], &Pnominal[jj][ep],
                   &Pnomstd[jj][ep]);
          fgets (chs, 150, fidinput);
          Pnominal[jj][ep] /= 1000;
          Pnomstd[jj][ep] /= 1000;
        }
      
   /*---------------------------------------------
    *  25) Read lapse rate ( degC/km -> degC/m )
    *---------------------------------------------*/ 
        fscanf (fidinput, "%lf ", &lapserate[ep]);
      fgets (chs, 150, fidinput);
      lapserateflag = 0;
      if (lapserate[ep] == -9999)
        {
          globalparflag++;
          lapserateflag = 1;
        }
      
      else
        lapserate[ep] /= 1000;
      
   /*-------------------------------------------------------------
    *  26) Read ELA start altitude and change per year ( m, m/a)
    *-------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf ", &ELAstart[ep], &ELAchange[ep]);
      fgets (chs, 150, fidinput);
      
   /*-------------------------------------------------------------------
    *  27) Read percentage of nival/ice runoff lost to evaporation (%)
    *-------------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf ", &dryevap[ep]);
      fgets (chs, 150, fidinput);
      
   /*-------------------------------------------------------------------
    *  27a) Read the canopy interception coefficients (mm/d)(-)
    *-------------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf ", &alphag[ep], &betag[ep]);
      alphag[ep] /= 1000;
      fgets (chs, 150, fidinput);
      
   /*-------------------------------------------------------------------
    *  27b) Read the evapotranspiration coefficient(alphagwe[ep](mm/day))
	*       Read the evapotransiration coefficient (betagwe[ep](-))
    *-------------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf ", &alphagwe[ep], &betagwe[ep]);
      alphagwe[ep] /= 1000;
      fgets (chs, 150, fidinput);
      
   /*----------------------------------------------
    *  28) Read river bed average slope (m/m)
    *----------------------------------------------*/ 
        fscanf (fidinput, "%lf ", &rslope[ep]);
      fgets (chs, 150, fidinput);
      
        /*----------------------------------------------
	 *  28a) Read bedload rating term (-)
	 *----------------------------------------------*/ 
        fscanf (fidinput, "%lf ", &alphabed[ep]);
      fgets (chs, 150, fidinput);
      if (alphabed[ep] == -9999)
        {
          alphabed[ep] = 1.0;
        }
      
   /*-------------------------------------
    *  29) Read basin length ( km -> m )
    *-------------------------------------*/ 
        fscanf (fidinput, "%lf ", &basinlength[ep]);
      fgets (chs, 150, fidinput);
      basinlength[ep] *= 1000;
      
   /*-----------------------------------------------------
    *  30) Read percentage of basin covered by lakes (%)
    *-----------------------------------------------------*/ 
        fscanf (fidinput, "%lf %c", &Rvol[ep], &Rparamcheck[ep]);
      if (Rparamcheck[ep] == 'a' || Rparamcheck[ep] == 'A')
        {
          fscanf (fidinput, "%lf", &Ralt[ep]);
          Rarea[ep] = 0.0;
        }
      if (Rparamcheck[ep] == 'd' || Rparamcheck[ep] == 'D')
        {
          fscanf (fidinput, "%lf", &Rarea[ep]);
          Ralt[ep] = 0.0;
        }
      fgets (chs, 150, fidinput);
      
   /*------------------------------------------------------------
    *  31) Read river mouth velocity coeff and exponent ( -, -)
    *------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf ", &velcof[ep], &velpow[ep]);
      fgets (chs, 150, fidinput);
      
   /*------------------------------------------------------------
    *  32) Read river mouth width coeff and exponent ( -, -)
    *------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf ", &widcof[ep], &widpow[ep]);
      fgets (chs, 150, fidinput);
      depcof[ep] = 1 / (velcof[ep] * widcof[ep]);
      deppow[ep] = 1 - velpow[ep] - widpow[ep];
      
   /*-----------------------------------------
    *  33) Read average river velocity (m/s)
    *-----------------------------------------*/ 
        fscanf (fidinput, "%lf ", &avgvel[ep]);
      fgets (chs, 150, fidinput);
      
   /*-------------------------------------------------------------
    *  34) Read max and min size of groundwater pool ( m^3, m^3)
    *-------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf ", &gwmax[ep], &gwmin[ep]);
      fgets (chs, 150, fidinput);
      
   /*---------------------------------------------------
    *  35) Read initial size of groundwater pool (m^3)
    *    for first epoch only
    *---------------------------------------------------*/ 
        if (ep == 0)
        {
          fscanf (fidinput, "%lf ", &gwinitial);
          fgets (chs, 150, fidinput);
        }
      
      else
        {
          fscanf (fidinput, "%lf ", &dumdbl);
          fgets (chs, 150, fidinput);
        }
      
   /*-----------------------------------------------------------------
    *  36) Read subsurface storm flow coeff and exponent ( m^3/s, -)
    *-----------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf ", &alphass[ep], &betass[ep]);
      fgets (chs, 150, fidinput);
      
   /*-----------------------------------------------------------------
    *  37) Read saturated hydraulic conductivity ( mm/day -> m/day )
    *-----------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf ", &Ko[ep]);
      fgets (chs, 150, fidinput);
      Ko[ep] /= 1000;
      
   /*------------------------------------------------------------------------
    *  38) Read latitude (Geographic position of rivermouth) ( in degrees )
    *------------------------------------------------------------------------*/ 
        fscanf (fidinput, "%lf %lf", &lon, &lat);
      fgets (chs, 150, fidinput);
      
   /*------------------------------------------------
    *  39) Read number of outlets to simulate (int)
    *   Must be constant for all epochs
    *------------------------------------------------*/ 
        noutletflag = 0;
      outletmodelflag = 0;
      fscanf (fidinput, "%c", &dummyx);
      if (dummyx == 'u' || dummyx == 'U')
        {
          noutletflag = 1;
          outletmodelflag = 1;
          minnoutlet = 2;
          maxnoutlet = 9;
        }
      
      else if (dummyx == 'r' || dummyx == 'R')
        {
          noutletflag = 1;
          outletmodelflag = 1;
          fscanf (fidinput, "%d %d", &minnoutlet, &maxnoutlet);
        }
      
      else
        {
          fseek (fidinput, sizeof (char) * -1, SEEK_CUR);
          fscanf (fidinput, "%d ", &noutlet);
          minnoutlet = 2;
          maxnoutlet = noutlet;
          if (noutlet == 1)
            {
              minnoutlet = 0;
              maxnoutlet = 1;
            }
          if (noutlet > 1)
            {
              outletmodelflag = 1;
            }
        }
      fgets (chs, 150, fidinput);
      
   /*--------------------------------------
    *  40) Read percentage of each outlet 
    *--------------------------------------*/ 
        steadyoutletpctflag = 0;
      nooutletpctflag = 0;
      if (dummyx == 'r' || dummyx == 'R' || dummyx == 'u' || dummyx == 'U')
        {
          steadyoutletpctflag = 1;
          nooutletpctflag = 1;
        }
      if (noutletflag == 0)
        {
          fscanf (fidinput, " %c", &dummyx);
          if (dummyx == 'u' || dummyx == 'U')
            {
              nooutletpctflag = 1;
              fscanf (fidinput, " %c ", &dummyx);
              if (dummyx == 's' || dummyx == 'S')
                steadyoutletpctflag = 0;
              
              else if (dummyx == 'u' || dummyx == 'U')
                steadyoutletpctflag = 1;
              
              else
                err++;
            }
          
          else if (dummyx == '0' || dummyx == '1')
            {
              if (ep == 0)
                outletpctdummy = malloc2d (maxnoutlet, nepochs, double);
              fseek (fidinput, sizeof (char) * -1, SEEK_CUR);
              for (jj = 0; jj < noutlet; jj++)
                {
                  fscanf (fidinput, " %lf ", &outletpctdummy[jj][ep]);
                }
            }
        }
      fgets (chs, 150, fidinput);
      
   /*-----------------------------
    *  41) Read number of events
    *-----------------------------*/ 
        fscanf (fidinput, "%c", &dummyx);
      if (dummyx == 'n' || dummyx == 'N')
        {
          fscanf (fidinput, "%d  ", &eventsnr[ep]);
          floodvalue[ep] = 0.0;
          eventnrflag = 0;
          if (eventsnr[ep] == 0)
            eventsnr[ep] = 1;
          if (steadyoutletpctflag == 0)
            eventsnr[ep] = 1;
        }
      if (dummyx == 'q' || dummyx == 'Q')
        {
          eventsnr[ep] = 1;
          eventnrflag = 1;
          fscanf (fidinput, "%lf", &floodvalue[ep]);
        }
      fgets (chs, 150, fidinput);
      
   /*-----------------------------------------
    *  42) Read the demping factor for Qsbar 
    *-----------------------------------------*/ 
        fscanf (fidinput, "%lf  ", &sedfilter[ep]);
      if (noutlet == 1)
        sedfilter[ep] = 0.0;
      fgets (chs, 150, fidinput);
      
   /*-----------------------------------------------------
    *  43) Check which formula to use to calculate Qsbar 
    *-----------------------------------------------------*/ 
        fscanf (fidinput, "%d  ", &Qsbarformulaflag[ep]);
      fgets (chs, 150, fidinput);
      
   /*--------------------------------------------------------
    *  44) If BQART is used -> get extra parameters
    *      read the Lithology factor 
    *--------------------------------------------------------*/ 
        fscanf (fidinput, "%lf  ", &lithology[ep]);
      fgets (chs, 150, fidinput);
      
   /*--------------------------------------------------------
    *  45) If BQART is used -> get extra parameters
    *      read the anthropogenic factor 
    *--------------------------------------------------------*/ 
        fscanf (fidinput, "%lf  ", &anthro[ep]);
      fgets (chs, 150, fidinput);
    }                         /* end epoch loop */
  fclose (fidinput);
  return (err);
}                              /* end of HydroReadInput */


