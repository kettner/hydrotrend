/*-------------------------------------------------------------------------------------------
 *  hydroopenfiles.c
 *
 *	Author: Albert Kettner, March 2006
 *
 *	Opens the input/output files for HydroTrend.
 *
 *	Variable		Def.Location	Type		Units	Usage
 *	--------		------------	----		-----	-----
 * 
 *-------------------------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include "hydroinout.h"
#include "hydroparams.h"
#include "hydroalloc_mem.h"

/*---------------------------
 *  Start of HydroOpenFiles
 *---------------------------*/
int hydroopenfiles()
{

/*-------------------
 *  Local Variables
 *-------------------*/	
int err, p;
char dummystring[300];

/*------------------------
 *  Initialize Variables
 *------------------------*/
err = 0;

/*--------------
 *	Open files
 *--------------*/
strcpy(ffnametrend1,startname);
strcat(ffnametrend1,fnametrend1);
if (verbose) printf("Opening %s... \n",ffnametrend1);
if ( (fidtrend1 = fopen(ffnametrend1,"w")) == NULL) {
   fprintf(stderr, "  HydroOpenFiles ERROR: Unable to open the trend file %s \n",ffnametrend1);
   err = 1;
}

strcpy(ffnametrend2,startname);
strcat(ffnametrend2,fnametrend2);
if (verbose) printf("Opening %s... \n",ffnametrend2);
if ( (fidtrend2 = fopen(ffnametrend2,"w")) == NULL) {
   fprintf(stderr, "  HydroOpenFiles ERROR: Unable to open the trend file %s \n",ffnametrend2);
   err = 1;
}

strcpy(ffnametrend3,startname);
strcat(ffnametrend3,fnametrend3);
if (verbose) printf("Opening %s... \n",ffnametrend3);
if ( (fidtrend3 = fopen(ffnametrend3,"w")) == NULL) {
   fprintf(stderr, "  HydroOpenFiles ERROR: Unable to open the trend file %s \n",ffnametrend3);
   err = 1;
}

strcpy(ffnamestat,startname);
strcat(ffnamestat,fnamestat);
if (verbose) printf("Opening %s... \n",ffnamestat);
if ( (fidstat = fopen(ffnamestat,"w")) == NULL) {
   fprintf(stderr, "  HydroOpenFiles ERROR: Unable to open the trend file %s \n",ffnamestat);
   err = 1;
}

strcpy(ffnamedistot,startname);
sprintf(dummystring,"%s",ffnamedistot);
strcpy(ffnamedistot,dummystring);
strcat(ffnamedistot,fnamedis);
if (verbose) printf("Opening %s... \n",ffnamedistot);
if ( (fiddistot = fopen(ffnamedistot,"wb")) == NULL) {
	fprintf(stderr, "  HydroOpenFiles ERROR: Unable to open the discharge file %s \n",ffnamedistot);
	err = 1;
}

if ( outletmodelflag == 1 )
	fiddis = allocate_1d_F( maxnoutlet );

if ( outletmodelflag == 1  )
	for (p=0; p<maxnoutlet; p++){
		strcpy(ffnamedis,startname);
		sprintf(dummystring,"%sOUTLET%d",ffnamedis,p+1);
		strcpy(ffnamedis,dummystring);
		strcat(ffnamedis,fnamedis);
		if (verbose) printf("Opening %s... \n",ffnamedis);
		if ( (fiddis[p] = fopen(ffnamedis,"wb")) == NULL) {
			fprintf(stderr, "  HydroOpenFiles ERROR: Unable to open the discharge file %s \n",ffnamedis);
			err = 1;
		}
	}

/*-----------------------
 *  Opening ascii files
 *-----------------------*/
if( strncmp(asciioutput,ON,2) == 0){
    strcpy(ffidasc,startname);
    strcat(ffidasc,fidasc);
    if (verbose) printf("Opening %s... \n",ffidasc);
    if ( (outp = fopen(ffidasc,"w")) == NULL) {
        fprintf(stderr, "  HydroOpenFiles ERROR: Unable to open the discharge file %s \n",ffidasc);
        err = 1;
    }

    strcpy(ffidasc1,startname);
    strcat(ffidasc1,fidasc1);
    if (verbose) printf("Opening %s... \n",ffidasc1);
    if ( (outp1 = fopen(ffidasc1,"w")) == NULL) {
        fprintf(stderr, "  HydroOpenFiles ERROR: Unable to open the discharge file %s \n",ffidasc1);
        err = 1;
    }

    strcpy(ffidasc2,startname);
    strcat(ffidasc2,fidasc2);
    if (verbose) printf("Opening %s... \n",ffidasc2);
    if ( (outp2 = fopen(ffidasc2,"w")) == NULL) {
        fprintf(stderr, "  HydroOpenFiles ERROR: Unable to open the discharge file %s \n",ffidasc2);
        err = 1;
    }

    strcpy(ffidasc3,startname);
    strcat(ffidasc3,fidasc3);
    if (verbose) printf("Opening %s... \n",ffidasc3);
    if ( (outp3 = fopen(ffidasc3,"w")) == NULL) {
        fprintf(stderr, "  HydroOpenFiles ERROR: Unable to open the discharge file %s \n",ffidasc3);
        err = 1;
    }

    strcpy(ffidasc4,startname);
    strcat(ffidasc4,fidasc4);
    if (verbose) printf("Opening %s... \n",ffidasc4);
    if ( (outp4 = fopen(ffidasc4,"w")) == NULL) {
        fprintf(stderr, "  HydroOpenFiles ERROR: Unable to open the discharge file %s \n",ffidasc4);
        err = 1;
    }
}
return(err);
}  /* end of HydroOpenFiles */
